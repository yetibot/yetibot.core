(ns yetibot.core.commands.karma
  (:require
   [schema.core :as sch]
   [yetibot.core.config :refer [get-config]]
   [yetibot.core.hooks :refer [cmd-hook]]
   [yetibot.core.models.karma :as model]
   [yetibot.core.commands.karma.specs :as karma.spec]
   [clojure.string :as str]
   [clj-time.format :as t]
   [clojure.spec.alpha :as s]))

;; The Karma feature is presently only available for use in Slack.
;;
;; Slack delivers reaction events by shortcode (actually a slightly
;; modified string version).  Without an exhaustive mapping there's no
;; reliable way for us to support configrable emoji reaction
;; processing in Slack, which we have prioritized.  Unfortunately,
;; this breaks IRC, where response emjoi are then rendered as the
;; shortcode string instead of the character.  We hope to improve this
;; in the future.

(def config (:value (get-config sch/Any [:karma])))

(def pos-emoji (or (-> config :emoji :positive) ":rainbow:"))  ;; 🌈
(def neg-emoji (or (-> config :emoji :negative) ":thunder_cloud_and_rain:"))  ;; ⛈

(def error {:parse "Sorry, I wasn't able to parse that."
            :karma "Sorry, that's not how Karma works. :thinking_face:"})  ;; 🤔

(defn- fmt-user-score
  [user-id score]
  (format "<@%s>: %s\n" user-id score))

(defn- fmt-user-notes
  [notes]
  (str/join "\n"
            (map #(format "_\"%s\"_ --%s _(%s)_"
                          (:note %)
                          (:voter-id %)
                          (t/unparse (t/formatters :mysql) (:created-at %)))
                 notes)))

(defn- fmt-high-scores
  [scores]
  (str/join "\n"
            (map #(fmt-user-score (:user-id %) (:score %))
                 scores)))

(defn get-score
  "karma <user> # get score and recent notes for <user>"
  {:yb/cat #{:fun}}
  [ctx]
  (if-not (s/valid? ::karma.spec/get-score-ctx ctx)
    {:result/error (:parse error)}
    (let [{[_ user-id] :match} ctx
          score (model/get-score user-id)
          notes (model/get-notes user-id)]
      {:result/data {:user-id user-id, :score score, :notes notes}
       :result/value (str (fmt-user-score user-id score)
                          (fmt-user-notes notes))})))

(defn get-high-scores
  "karma # get leaderboard"
  {:yb/cat #{:fun}}
  [_]
  (let [scores (model/get-high-scores)]
    {:result/data scores
     :result/value (fmt-high-scores scores)}))

(defn adjust-score
  "karma <user>(++|--) <note> # adjust karma for <user> with optional <note>"
  {:yb/cat #{:fun}}
  [ctx]
  (let [parsed (s/conform ::karma.spec/adjust-score-ctx ctx)]
    (if (= parsed ::s/invalid)
      {:result/error (:parse error)}
      (let [{{voter-id :id voter-name :name} :user} parsed
            {{user-id :user-id [action _] :action note :note} :match} parsed
            positive-karma? (= action :positive)]
        (if (and positive-karma? (= user-id voter-id))
          {:result/data {:error (:karma error)
                         :user-id user-id
                         :voter-id voter-id}
           :result/value (:karma error)}
          (let [[score-delta reply-emoji] (if positive-karma? [1 pos-emoji] [-1 neg-emoji])]
            (model/add-score-delta! user-id voter-name score-delta note)
            (let [score (model/get-score user-id)
                  notes (model/get-notes user-id)]
              {:result/data {:user-id user-id
                             :score score
                             :notes notes}
               :result/value (format "%s <@%s>: %d" reply-emoji user-id score)})))))))

(cmd-hook "karma"
          #"^(?x) \s* @(\w[-\w]*\w) \s*$" get-score
          #"^(?x) \s* @(\w[-\w]*\w) \s{0,2} (--|\+\+) (?: \s+(.+) )? \s*$" adjust-score
          _ get-high-scores)
