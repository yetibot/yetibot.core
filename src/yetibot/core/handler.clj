(ns yetibot.core.handler
  (:require
    [clojure.core.async :refer [timeout alts!! alts! chan go <! >! >!! <!!]]
    [clojure.core.match :refer [match]]
    [clojure.stacktrace :as st]
    [clojure.string :refer [join]]
    [taoensso.timbre :refer [debug info warn error]]
    [yetibot.core.chat :refer [chat-data-structure]]
    [yetibot.core.util.command :refer [command? extract-command embedded-cmds]]
    [yetibot.core.interpreter :as interp]
    [yetibot.core.models.history :as h]
    [yetibot.core.parser :refer [parse-and-eval transformer parser unparse]]
    [yetibot.core.util.format :refer [to-coll-if-contains-newlines
                                      format-data-structure
                                      format-exception-log]]))

(def expr-eval-timeout-ms 1000)

(def ^:private exception-format "👮 %s 👮")

(def all-event-types #{:message :leave :enter :sound :kick})

(defn handle-parsed-expr
  "Top-level for already-parsed commands.
   Turns a parse tree into a string or collection result."
  [chat-source user yetibot-user parse-tree]
  (binding [interp/*current-user* user
            interp/*yetibot-user* yetibot-user
            interp/*chat-source* chat-source]
    (transformer parse-tree)))

(defn record-and-run-raw
  "Top level message handler.

   Use this when you want to evaluate a command and all the dynamic vars have
   already been bound.

   At the end of the expression pipeline a :value or :error will be returned.
   Unwraps it and returns it formatted for the user.

   body - unparsed expression. Could be one of:
          1. A command like 'echo hello yetibot'
          2. Embedded commands like 'nice day today `weather`'
          3. Normal body text without commands.

   When the body was an expression, it returns:

   [
     {
      :embedded? - whether or not the command was embedded
      :error? - whether or not the result was an error
      :result - the formatted string result
     }
   ]

   Otherwise returns nil for non expressions.
   "
  [body user yetibot-user & [{:keys [record-yetibot-response?]
                              :or {record-yetibot-response? true}}]]
  (info "record-and-run-raw" record-yetibot-response?)
  (let [{:keys [adapter room uuid is-private]
         :as chat-source} interp/*chat-source*
        timestamp (System/currentTimeMillis)
        ;; `correlation-id` lets us correlate two records in the history
        ;; table:
        ;; - an input command from a user
        ;; - the result that Yetibot posts back to chat
        correlation-id (str timestamp "-" (hash [chat-source user body]))
        parsed-normal-command (when-let
                                [[_ body] (extract-command body)]
                                (parser body))

        parsed-cmds
        (or
          ;; if it starts with a command prefix (e.g. !) it's a command
          (and parsed-normal-command [parsed-normal-command])
          ;; otherwise, check to see if there are embedded commands
          (embedded-cmds body))
        cmd? (boolean (seq parsed-cmds))]

    ;; record the body of users' messages if the user is not Yetibot
    (when-not (:yetibot? user)
      (h/add {:chat-source-adapter uuid
              :chat-source-room room
              :is-private is-private
              :correlation-id correlation-id
              :user-id (-> user :id str)
              :user-name (-> user :username str)
              :is-yetibot false
              :is-command cmd?
              :body body}))

    ;; When:
    ;; - the user's input was an expression (or contained embedded exprs)
    ;; - and the user is not Yetibot
    ;; process those exprs:
    ;; - adding them individually to history and
    ;; - evaluating the expression
    ;; then return the collection of results
    (when (and cmd? (not (:yetibot? user)))
      (let [[results timeout-result]
            (alts!!
              [(go (map
                     (fn [parse-tree]
                       (try
                         (let [original-command-str (unparse parse-tree)
                               {:keys [value error]} (handle-parsed-expr
                                                       chat-source user
                                                       yetibot-user
                                                       parse-tree)
                               result (or value error)
                               error? (not (nil? error))
                               [formatted-response _] (format-data-structure
                                                        result)]
                           ;; Yetibot should record its own response in
                           ;; `history` table before/during posting it back to
                           ;; the chat adapter. Then we can more easily
                           ;; correlate request (e.g. commands from user) and
                           ;; response (output from Yetibot)
                           (h/add {:chat-source-adapter uuid
                                   :chat-source-room room
                                   :is-private is-private
                                   :correlation-id correlation-id
                                   :user-id (-> yetibot-user :id str)
                                   :user-name (-> yetibot-user :username str)
                                   :is-yetibot true
                                   :is-command false
                                   :is-error error?
                                   :command original-command-str
                                   :body formatted-response})
                           ;; don't report errors on embedded commands
                           {:embedded? (not parsed-normal-command)
                            :error? error?
                            :result result})
                         (catch Throwable ex
                           (error "error handling expression:" body
                                  (format-exception-log ex))
                           {:embedded?  (not parsed-normal-command)
                            :error? true
                            :result (format exception-format ex)})))
                     parsed-cmds))
               (timeout expr-eval-timeout-ms)])]
        (info "throw away" (pr-str results) (pr-str timeout-result))
        (info "command eval result and timeout"
              (pr-str {:results results :timeout-result timeout-result}))
        (or results
            [{:timeout? true
              :result (str "Evaluation of `" body "` timed out after "
                           (/ expr-eval-timeout-ms 1000) " seconds.")}])))))

(defn handle-unparsed-expr
  "Entry point for parsing and evaluation of ad-hoc commands.
   These are not recorded in the history table."
  ([chat-source user body]
   ; For backward compat, support setting user at this level.
   ; After deprecating, this can be removed.
   ; Currently it's used by the web API in yetibot.
   (binding [interp/*current-user* user
             interp/*chat-source* chat-source]
     (handle-unparsed-expr body)))
  ([body] (parse-and-eval body)))

(defn handle-raw
  "Top level handler for commands. Properly records commands in the database,
   handles errors, and posts the result back to chat.

   Expected event-types are:
   :message
   :leave
   :enter
   :sound
   :kick"
  [{:keys [adapter room uuid is-private] :as chat-source}
   user event-type body yetibot-user]
  (when body
    (binding [interp/*chat-source* chat-source]
      (go
        ;; there may be multiple expr-results, as in the case of multiple embedded
        ;; commands in a single body
        (let [expr-results (record-and-run-raw body user yetibot-user)]
          (run!
            (fn [{:keys [timeout? embedded? error? result]}]
              (if (or (not error?) (not embedded?))
                (chat-data-structure result)
                (info "Skip sending error result for embedded command to chat"
                      result)))
            expr-results))))))

(defn cmd-reader [& args] (handle-unparsed-expr (join " " args)))
