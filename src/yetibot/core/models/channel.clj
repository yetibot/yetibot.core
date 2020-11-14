(ns yetibot.core.models.channel
  (:require
   [yetibot.core.db.channel :as db]
   [taoensso.timbre :refer [debug info warn error]]))

(def cat-settings-key "disabled-categories")
(def channel-is-member-key "is-member")
(def yetibot-channels-key "yetibot-channels")

(def protected-keys
  "Users can not manually set these keys"
  #{cat-settings-key
    yetibot-channels-key
    channel-is-member-key})

;; known channel keys
(def karma-enabled-key "karma-observers-enabled")

(def channel-config-defaults
  "Provides both a list of all available default settings as well as their
   defaults. These are settings known to work with built in commands. Other
   arbitrary settings could exist to be utilized by aliases or crons.

   All settings are flat string key/values."
  {;; whether to send things like global messages and Tweets to a channel
   "broadcast" "false"
   ;; JIRA project
   "jira-project" ""
   ;; default Jenkins project
   "jenkins-default" ""
   ;; whether to enable karma observers
   karma-enabled-key "false"})

(defn merge-defaults [channel-config]
  (merge channel-config-defaults channel-config))

(defn channel-settings
  [uuid channel]
  (let [results (db/query {:where/map {:chat-source-adapter (pr-str uuid)
                                       :chat-source-channel channel}})]
    (->> results
         (map (fn [{:keys [key value]}]
                [key
                 ;; handle special encoding for disabled-categories
                 (if (= key cat-settings-key)
                   (read-string value) value)]))
         (into {})
         merge-defaults)))

(defn settings-for-chat-source
  "Convenience fn that takes a chat-source, extracts the correct keys and uses
   them to call channel-settings"
  [{:keys [uuid room]}]
  (channel-settings uuid room))

(defn find-key
  [uuid channel k]
  (first (db/query
           {:where/map
            (merge
              {:chat-source-adapter (pr-str uuid)
               :key k}
              (when channel
                {:chat-source-channel channel}))})))

(defn set-key
  [uuid channel k v]
  ;; see if the key already exists
  (debug "set-key" (pr-str (find-key uuid channel k)))
  (if-let [{id :id} (find-key uuid channel k)]
    (do
      (debug "updating existing key" id)
      (db/update-where {:id id} {:value v}))
    (do
      (debug "creating new key" uuid channel k v)
      (db/create {:chat-source-adapter (pr-str uuid)
                :chat-source-channel channel
                :key k
                :value v}))))

(defn unset-key
  "Unset a key for a given uuid and channel"
  [uuid channel k]
  (if-let [{id :id} (find-key uuid channel k)]
    (do
      (debug "deleting key" k id)
      (db/delete id))
    (do
      (debug "unset key failed, not found" k)
      nil)))

;; disabled categories



(defn get-disabled-cats [uuid channel]
  (debug "get-disabled-cats" (pr-str uuid) (pr-str channel))
  (if-let [cat-settings (find-key uuid channel cat-settings-key)]
    (do
      (debug "get-disabled-cats" (pr-str cat-settings))
      (read-string (:value cat-settings)))
    #{}))

(defn set-disabled-cats [uuid channel categories]
  (debug "set disabled cats" categories)
  (if (empty? categories)
    (unset-key uuid channel cat-settings-key)
    (set-key uuid channel cat-settings-key (pr-str categories))))

;; yetibot-channels: channels yetibot is in
;; Note we only track these for adapters that don't remember which channels they
;; were in across reconnects (e.g. IRC). Slack does not store this in the db
;; since it keeps track on the server.

(defn get-yetibot-channels
  "Yetibot channels are channels that Yetibot is in or should be in (e.g. IRC
   upon conecting is not yet in them but knows which channels to join using this
   fn)."
  [uuid]
  (if-let [{:keys [value]} (find-key uuid nil yetibot-channels-key)]
    (read-string value)
    #{}))

(comment
  (get-yetibot-channels :freenode)
  )

(defn set-yetibot-channels
  [uuid channels]
  (set-key uuid nil yetibot-channels-key (pr-str channels)))
