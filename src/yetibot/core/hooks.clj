(ns yetibot.core.hooks
  (:require
   [yetibot.core.models.admin :as admin]
   [clojure.set :refer [intersection]]
   [taoensso.timbre :refer [trace info]]
   [yetibot.core.handler]
   [clojure.string :as s]
   [metrics.timers :as timers]
   [yetibot.core.models.channel :as c]
   [yetibot.core.interpreter :refer [handle-cmd]]
   [yetibot.core.models.help :as help]
   [robert.hooke :as rh]
   [yetibot.core.util.command :refer [command-enabled?]]))

; Stores the mapping of prefix-regex -> sub-commands.
(defonce hooks (atom {}))

(defn cmds-for-cat
  "Return collection of vars for cmd handler functions whose :yb/cat meta
   includes `search-cat` and are enabled"
  [search-cat]
  (let [search-cat (keyword search-cat)]
    (->> @hooks vals
         (mapcat
           ;; take every other, dropping the left side of the pair (i.e. the
           ;; regex that triggers the cmd)
           (comp (partial take-nth 2) rest))
         ;; filter down to commands in the provided `search-cat` category
         (filter
           (fn [cmd] ((set (:yb/cat (meta cmd))) search-cat)))

         ;; filter down to enabled commands
         (filter
           (fn [cmd] (-> cmd meta :doc (s/split #" ") first command-enabled?)))

         ;; remove duplicates since multiple regexes can trigger the same
         ;; command
         set)))

(comment
  (map meta (cmds-for-cat "util"))

  (->> (cmds-for-cat "util")
       (filter (fn [cmd] (-> cmd meta :doc (s/split #" ") first command-enabled?)))
       )

  )

(defonce re-prefix->topic (atom {}))

(defn find-sub-cmds
  "Given a top level command prefix look up corresponding sub-cmds by matching
   prefix against command regexes in `hooks.`"
  [prefix]
  (first (filter (fn [[k v]] (re-find (re-pattern k) prefix)) @hooks)))

(defn match-sub-cmds
  [command-args sub-cmds]
  (let [cmd-pairs (partition 2 sub-cmds)]
    (some (fn [[sub-re sub-fn]]
            (when-let [match (re-find sub-re command-args)]
              [match sub-fn]))
          cmd-pairs)))

(defn split-command-and-args
  [cmd-with-args]
  (let [[cmd args] (s/split cmd-with-args #"\s" 2)]
    ;; make args an empty string if no args
    [cmd (or args "")]))

(defn cmd-unhook
  "Removes the sub-commands for a prefix / topic."
  [topic]
  (let [str-re (str "^" topic "$")]
    (help/remove-docs topic)
    (swap! re-prefix->topic dissoc str-re)
    (swap! hooks dissoc str-re)))

(defn handle-with-hooked-cmds
  "Looks up the set of possible commands by matching the first word against
   prefixes stored in `hooks`. If it finds a match, it then matches against
   sub-commands and defaults to help if no sub-commands match.

   If unable to match prefix, it calls the callback, letting `handle-cmd`
   implement its own default behavior."
  [callback cmd-with-args {:keys [chat-source user opts settings] :as extra}]
  (info "handle-with-hooked-cmds" chat-source opts (pr-str settings))
  (let [[cmd args] (split-command-and-args cmd-with-args)
        admin-only-command? (admin/admin-only-command? cmd)
        user-is-admin? (admin/user-is-admin? user)
        _ (info "admin only?" admin-only-command?
                "user is admin?" user-is-admin?)
        [cmd-re sub-cmds] (find-sub-cmds cmd)
        [match sub-fn] (match-sub-cmds args sub-cmds)
        ;; disabled categories for channel
        disabled-cats (if settings (settings c/cat-settings-key) #{})
        fn-cats (set (:yb/cat (meta sub-fn)))
        matched-disabled-cats (seq (intersection disabled-cats fn-cats))]

    ;; possible ways to handle an incoming command:

    (cond
      ;; 1. deny if command is admin only
      (and admin-only-command? (not user-is-admin?))
      {:result/error (format "Only admins are allowed to execute %s commands" cmd)}

      ;; 2. command and sub command found, but category is disabled
      matched-disabled-cats
      (str
       (s/join ", " (map name matched-disabled-cats))
       " commands are disabled in this channel🖐")

      ;; 3. no command found, fallback
      (not sub-cmds) (callback cmd-with-args extra)

      ;; 4. command is disabled, fallback
      (not (command-enabled? cmd)) (callback cmd-with-args extra)

      ;; 5. command found, but no sub commands found, run help on it
      (not sub-fn)
      (:value
       (yetibot.core.handler/handle-unparsed-expr
        (str "help " (get @re-prefix->topic (str cmd-re)))))

      ;; 6. command and sub command found, execute it!
      :else
      (timers/time!
       (timers/timer ["yetibot" cmd (str (:name (meta sub-fn)))])
       (sub-fn (merge extra {:cmd cmd :args args :match match}))))))

;; Hook the actual handle-cmd called during interpretation.
;; TODO remove completely
(rh/add-hook #'handle-cmd #'handle-with-hooked-cmds)

(defn lockdown-prefix-regex
  "Takes a regex and ensures that it's locked down with ^ or $ to prevent
   collisions with runtime aliases."
  [re]
  (let [re-str (str re)]
    (if (or (re-find #"^\^" re-str) (re-find #"\$$" re-str))
      re
      (re-pattern (str "^" re-str "$")))))

(defn cmd-hook-resolved
  "Expects fully resolved syntax where as plain cmd-hook can take normally
   unresolved symbols like _ and translate them into '_"
  [topic-and-pattern & cmds]
  ;; re-prefix can be:
  ;; - a single regex - we take its string value to populate topic
  ;; - [deprecated] a vector of [topic-str regex-pattern]
  ;; - a map {topic pattern} - containing any number of topic/pattern pairs
  ;;
  ;; normalize it into the map form:
  (let [topics->patterns (condp #(%1 %2) topic-and-pattern
                           vector? (let [[topic pattern] topic-and-pattern]
                                     {topic pattern})
                               ;; already in correct form
                           map? topic-and-pattern
                               ;; else - just the pattern
                           {(str topic-and-pattern) topic-and-pattern})
        cmd-pairs (partition 2 cmds)]
    (run!
     (fn [[topic re-prefix]]
       (let [re-prefix (lockdown-prefix-regex re-prefix)]
          ;; store a mapping of re-prefix (string representation) to topic
         (swap! re-prefix->topic conj {(str re-prefix) topic})
          ;; add to help docs
         (if (command-enabled? topic)
           (help/add-docs
            topic
             ;; extract the docstring from each subcommand
            (map (fn [[_ cmd-fn]] (:doc (meta cmd-fn))) cmd-pairs))
           (info "skipping docs for disabled command:" topic))
          ;; store the hooks to match
         (swap! hooks conj {(str re-prefix) cmds})))
     topics->patterns)))

(defmacro cmd-hook
  "Takes potentially special syntax and resolves it to symbols for
   cmd-hook-resolved. Currently _ is the only special syntax."
  [prefix & cmds]
  (let [cmd-pairs (partition 2 cmds)]
    `(cmd-hook-resolved
      ~prefix
      ~@(mapcat (fn [[k# v#]]
                  [(condp = k#
                     '_ #".*"
                     k#)
                   ; Need to resolve the var in order to get at its docstring.
                   ; This only applies to regular usage of cmd-hook. If using
                   ; cmd-hook with unresolvable anonymous functions (such as in
                   ; alias) the client must add the help metdata itself as
                   ; cmd-hook cannot extract it.
                   (if-let [resolved (resolve v#)] resolved v#)])
                cmd-pairs))))

;; TODO make obs-hooks reloadable - maybe each one should have some metadata
;; like name and description of intent assocaited with it?

(comment
  ;; during dev use this to remove all the hooks, then reload the ns to re-hook
  (rh/clear-hooks #'yetibot.core.handler/handle-raw)

  )

(defn obs-hook
  "Pass a collection of event-types you're interested in and an observer
   function that accepts a single arg. If an event occurs that matches the
   events in your event-types arg, your observer will be called with the event's
   json."
  [event-types observer]
  (rh/add-hook
   #'yetibot.core.handler/handle-raw
   (let [event-types (set event-types)]
     (fn [callback chat-source user event-type yetibot-user
          {:keys [body reaction] :as event-info}]
       #_(debug "observed" (:yetibot? user)
                (color-str :blue chat-source)
                {:user user}
                {:event-type event-type}
                (color-str :blue (pr-str event-info)))
       (when (and
              (not (:yetibot? user))
              (contains? event-types event-type))
         (observer (merge event-info
                          {:chat-source chat-source
                           :settings (c/settings-for-chat-source chat-source)
                           :event-type event-type
                           :user user
                           :yetibot-user yetibot-user})))
        ;; observers always pass through to the callback
       (callback chat-source user event-type yetibot-user event-info)))))
