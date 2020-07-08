(ns yetibot.core.commands.my
  (:require [yetibot.core.hooks :refer [cmd-hook]]
            [yetibot.core.models.users :as users]
            [taoensso.timbre :refer [info]]
            [yetibot.core.db.my :as db]
            [clojure.string :as string]))

(defn format-user-kv [{user-id :user-id k :key v :value}]
  (format "[%s] %s: %s" user-id k v))
(defn format-kv [{k :key v :value}] (format "%s: %s" k v))
(defn format-v [{v :value}] (format "%s" v))

(defn set-cmd
  "my <key> = <value> # set <key> = to <value> for the current user"
  [{[_ k v] :match
    {user-id :username} :user
    {chat-source-channel :room
     chat-source-uuid :uuid} :chat-source}]
  (let [record {:user-id user-id
                :chat-source-adapter (pr-str chat-source-uuid)
                :chat-source-channel chat-source-channel
                :key k
                :value v}]
    (info "set-cmd" (pr-str record))
    (db/create record)
    (format "✓ Set %s = %s for %s" k v user-id)))

(defn get-cmd
  "my <key> # get the value of <key> for the current user"
  [{k :match {user-id :username} :user}]
  (let [[result] (db/query {:where/map {:key k}})]
    (if result
      {:result/value (format-v result)
       :result/data result}
      {:result/error (format "No value found for key `%s` and user `%s`"
                             k user-id)})))

(defn list-cmd
  "my # list all known keys for the current user"
  [{{user-id :username} :user}]
  (let [results (db/query {:where/map {:user-id user-id}})]
    (info "list-cmd" (pr-str results))
    {:result/value (map format-kv results)
     :result/data results}))

(comment

  (let [user-ids ["devth" "yetibot-devth"]]
    (db/query {:where/clause (str
                              "("
                              (->> user-ids
                                   (map (constantly "user_id=?"))
                                   (string/join " OR "))
                              ")")
               :where/args user-ids}))

  (db/create
   {:user-id "yetibot-devth"
    :chat-source-adapter ":ybslack"
    :chat-source-channel "local"
    :key "zip"
    :value "98104"})
  (db/create
   {:user-id "devth"
    :chat-source-adapter ":ybslack"
    :chat-source-channel "local"
    :key "zip"
    :value "59101"})

  (db/query {:where/map {:key "zip"}})

  (db/find-all))

(cmd-hook #"my"
          #"(\S+)\s+\=\s+(.+)" set-cmd
          #".+" get-cmd
          _ list-cmd)

(defn our-cmd
  "our <key> # get the value of <key> for all users in the channel"
  [{k :match chat-source :chat-source}]
  (let [users (users/get-users chat-source)
        _ (info "our-cmd k" (pr-str k))
        _ (info "our-cmd users" (pr-str users))
        user-ids (map :username users)
        result (db/query {:where/clause (str
                                         "("
                                         (->> user-ids
                                              (map (constantly "user_id=?"))
                                              (string/join " OR "))
                                         ")")
                          :where/args user-ids
                          :where/map {:key k}})]
    (if result
      {:result/value (map format-v result)
       :result/data result}
      {:result/error (format "No values found for key `%s`" k)})))

(defn our-list-cmd
  "our # list all known keys for all users in the channel"
  [{chat-source :chat-source}]
  (let [users (users/get-users chat-source)
        user-ids (map :username users)
        result (db/query {:where/clause (str
                                         "("
                                         (->> user-ids
                                              (map (constantly "user_id=?"))
                                              (string/join " OR "))
                                         ")")
                          :where/args user-ids })]

    (if result
      {:result/value (map format-user-kv result)
       :result/data result}
      {:result/value "No keys set for users in this channel"
       :result/data []}
      )))

(cmd-hook #"our"
          #".+" our-cmd
          _ our-list-cmd)

