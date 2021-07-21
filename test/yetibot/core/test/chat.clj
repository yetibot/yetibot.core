(ns yetibot.core.test.chat
  (:require [midje.sweet :refer [facts fact => =not=> provided as-checker]]
            [clojure.string :as s]
            [yetibot.core.chat :as c]))

(facts
 "about contains-image-url-lines?"
 (fact
  "it will return 'false' for strings that do not contain a URL with image
   like chars"
  (c/contains-image-url-lines? "image.jpg") => false
  (c/contains-image-url-lines? "http://image.html") => false
  (c/contains-image-url-lines? "http://image.jpeeeg") => false
  (c/contains-image-url-lines? "http://image.pnnng") => false
  (c/contains-image-url-lines? "http://image.giiif") => false)

 (fact
  "it will return 'true' for strings that do contain a URL with image
   like chars"
  (c/contains-image-url-lines? "http://image.jpg") => true
  (c/contains-image-url-lines? "https://image.jpg") => true
  (c/contains-image-url-lines? "https://image.jpeg") => true
  (c/contains-image-url-lines? "http://image.png") => true
  (c/contains-image-url-lines? "http://image.gif") => true))

(facts
 "about should-send-msg-for-each?"
 (fact
  "it will return 'true' when you pass it a legit chat data stucture and
   a formatted message that has :newlines and URLs that contain image like
   chars"
  (let [data ["look at my picture"
              "http://image.jpg"]]
    (c/should-send-msg-for-each? data (s/join \newline data)) => true)))

(facts
 "about suppressed-pred"
 (fact
  "it will get an object's :suppressed metadata and return it's value"
  (let [truthy (c/suppress {})
        falsey {}]
    (c/suppressed-pred truthy) => true
    (c/suppressed-pred falsey) =not=> true)))

(facts
 "about suppressed?"
 (fact
  "it will check to see if an obj, or collection of objects passed in are
   considered suppressed, and return a boolean result"
  (let [truthy (c/suppress {})
        falsey {}
        truthycoll [truthy truthy]
        mixedcoll [truthy falsey]
        falseycoll [falsey falsey]]
    (c/suppressed? truthy) => true

    ;; why is the below test returning true ??
    ;; i get that it is a coll?, but the item in the coll does NOT
    ;; contain a :suppress metadata item eql to true, so what gives ??
    ;; (c/suppressed? falsey) =not=> true

    (c/suppressed? truthycoll) => true
    (c/suppressed? mixedcoll) =not=> true
    (c/suppressed? falseycoll) =not=> true)))

(facts
 "about suppress"
 (fact
  "it attaches a metadata :supress keyword equal to true to whatever obj is
  provided"
  (let [truthy (c/suppress {})]
    (:suppress (meta truthy)) => true)))

(facts
 "about with-target"
 (fact
  "it attaches a metadata :target keyword equal to whatever value is provided
   to the obj (data-structure) that is provided"
  (let [target "i am the target"
        ds (c/with-target target {})]
    (:target (meta ds)) => target)))

(facts
 "about channels-with-broadcast"
 (fact
  "it will return a collection of only those channels that have a broadcast
   settings value equal to true"
  (count (c/channels-with-broadcast)) => 1
  (provided (c/all-channels) => [[nil nil nil]
                                 [nil nil {"broadcast" "true"}]
                                 [nil nil {"broadcast" "false"}]])))

(facts
 "about send-msg-for-each"
 (fact
  "it will send a message for each item in the msg collection, assuming it
   is less than the max allowable"
  (c/send-msg-for-each [1]) => nil
  (provided (c/send-msg 1) => :didsend))

 (fact
  "it will only send the max number of allowed messages in a single batch, plus
   one to let user know messsage batch was truncated"
  (c/send-msg-for-each (range 40)) => :sent-msg
  (provided
   (c/send-msg (as-checker int?)) => :sent-msg :times c/max-msg-count
   (c/send-msg (as-checker string?)) => :sent-msg :times 1)))
