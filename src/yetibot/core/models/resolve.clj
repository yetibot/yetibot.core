(ns yetibot.core.models.resolve
  (:require [yetibot.core.db.my :as db]))

(defn resolve-value
  "Given some context and a key, dynamically resolve the value for a key from
   most specific to least:
   
   - user
   - users in the channel
   - channel
   - adapter
   - global"
  [{:keys [chat-source
           user
           users-present]
    :as context}
   k]
)
