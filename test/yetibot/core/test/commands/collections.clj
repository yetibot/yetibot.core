(ns yetibot.core.test.commands.collections
  (:require
   [yetibot.core.commands.collections :as colls]
   yetibot.core.commands.render
   yetibot.core.commands.about
   yetibot.core.commands.echo
   [taoensso.timbre :refer [info]]
   [yetibot.core.util.command-info :refer [command-execution-info]]
   [clojure.test :refer [deftest testing is]]
   [midje.sweet :refer [fact facts =>]]))

(deftest random-test
  (testing "Random with no args"
    (let [{:keys [matched-sub-cmd result]} (command-execution-info
                                            "random" {:run-command? true})
          random-number (read-string result)]
      (is (= matched-sub-cmd #'yetibot.core.commands.collections/random)
          "It matches the expected `random` command handler")
      (is (number? random-number)
          "It should generate a random number when passed no args")))
  (testing "Random with args"
    (let [{{result :result/value} :result} (command-execution-info
                                            "random" {:opts ["bar" "foo"]
                                                      :run-command? true})]
      (is (or (= "bar" result) (= "foo" result))
          "Random with a collection passed into it picks a random item from the
           collection"))))

(facts "slide-context"
  (fact "gives the context" (colls/slide-context (range 10) 3 2) => [1 2 3 4 5])
  (fact "is shorter if there aren't enough results before"
        (colls/slide-context (range 10) 1 2) => [0 1 2 3])
  (fact "is shorter if there aren't enough results at the end"
        (colls/slide-context (range 10) 9 3) => [6 7 8 9]))

(facts "sliding-filter"
  (fact (colls/sliding-filter 1 #(> % 4) (range 10)) =>
        [[4 5 6] [5 6 7] [6 7 8] [7 8 9] [8 9]])
  (fact (colls/sliding-filter 1 odd? (range 6 10)) =>
        [[6 7 8] [8 9]]))

(facts "grep context"
  (fact "for multiple matches"
        (colls/grep-data-structure #"yes"
                                   (map-indexed vector
                                                ["devth: foo"
                                                 "devth: yes"
                                                 "devth: bar"
                                                 "devth: lol"
                                                 "devth: ok"
                                                 "devth: baz"
                                                 "devth: !history | grep -C 2 yes"])
                                   {:context 2})
        =>
        [[0 "devth: foo"]
         [1 "devth: yes"]
         [2 "devth: bar"]
         [3 "devth: lol"]
         [4 "devth: ok"]
         [5 "devth: baz"]
         [6 "devth: !history | grep -C 2 yes"]])
  (fact "for single match"
        (colls/grep-data-structure #"foo"
                                   (map-indexed vector
                                                ["bar" "lol" "foo" "baz" "qux"])
                                   {:context 1})
        =>
        '([1 "lol"] [2 "foo"] [3 "baz"]))
  (fact "no overlapping matches"
        (colls/grep-data-structure #"foo"
                                   (map-indexed vector
                                                ["foo" "bar" "baz" "foo"])
                                   {:context 2})
        =>
        [[0 "foo"]
         [1 "bar"]
         [2 "baz"]
         [3 "foo"]]))

(facts
 "grep-cmd-test"
 (fact "gives full match"
       (colls/grep-cmd {:match "foo" :opts ["foo" "bar"]})
       =>
       #:result{:value ["foo"] :data nil})
 (fact "gives partial match"
       (colls/grep-cmd {:match "foo" :opts ["foobar" "baz"]})
       =>
       #:result{:value ["foobar"] :data nil})
 (fact "with -C flag gives context"
       (colls/grep-surrounding {:match (re-find #"-C\s+(\d+)\s+(.+)" "-C 1 baz")
                                :opts ["foo" "bar" "baz"]})
       =>
       #:result{:value ["bar" "baz"], :data nil})
 (fact "with -v flag gives inverted match"
       (colls/inverted-grep {:match (re-find #"-v\s+(.+)" "-v bar")
                             :opts ["foo" "bar" "baz"]})
       =>
       #:result{:value ["foo" "baz"], :data nil}))

(facts
 "about flatten-cmd"
 (let [cmn-res ["1" "2" "3"]]
   (fact
    "simple case using vector"
    (:result/value (colls/flatten-cmd {:opts ["1" "2" "3"]}))
    => cmn-res)
   (fact
    "simple case using nested vector"
    (:result/value (colls/flatten-cmd {:opts [["1" "2" "3"]]}))
    => cmn-res)
   (fact
    "case using single str w/ newlines in vector"
    (:result/value (colls/flatten-cmd
                    {:opts [(str 1 \newline 2 \newline 3 \newline)]}))
    => cmn-res)
   (fact
    "case using single str w/ newlines in nested vector"
    (:result/value (colls/flatten-cmd
                    {:opts [[[(str 1 \newline 2 \newline 3 \newline)]]]}))
    => cmn-res)))

(deftest words-test
  (= (:result
      (command-execution-info "words foo bar" {:run-command? true}))
     ["foo" "bar"]))

(deftest random-test2
  (= (:result
       (command-execution-info "repeat 3 echo hi" {:run-command? true})
       {:parse-tree [:expr [:cmd [:words "repeat" [:space " "] "3" [:space " "]
                                  "echo" [:space " "] "hi"]]]
        :sub-commands [#"(\d+)\s(.+)" #'yetibot.core.commands.collections/repeat-cmd]
        :matched-sub-cmd #'yetibot.core.commands.collections/repeat-cmd
        :match ["3 echo hi" "3" "echo hi"]
        :command "repeat"
        :command-args "3 echo hi"
        :result ["hi" "hi" "hi"]})))

(deftest data-test
  (testing "No data results in an error"
    (is
      (=
       #:result{:error "There is no `data` from the previous command 🤔"}
       (:result (command-execution-info "data $.[0]" {:run-command? true})))))

  (testing "Data should be preserved in data <path>"
    (is (=
         {:foo :bar}
         (-> (command-execution-info "data $.[0]" {:data [{:foo :bar}]
                                                   :run-command? true})
             :result :result/data)))))

(def opts (map str ["red" "green" "blue"]))
;; construct some fake data that in theory represents the simplified
;; human-friendly opts above:
(def sample-data {:items (map #(hash-map % %) ["red" "green" "blue"]) :count 3})
(def sample-data-collection (:items sample-data))

(def params {:opts opts
             :data-collection sample-data-collection
             :data sample-data
             :run-command? true})

(defn value->data
  [value]
  (->> value (repeat 2) (apply hash-map)))

(deftest data-propagation-test

  (testing "random should propagate data"
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "random" params)]
      (is
       (= (value->data value) data)
       "random should pull the corresponding random item out of the data and
         propagate it")))

  (testing "head should propagate data"
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "head 2" params)]
      (is (= [{"red" "red"} {"green" "green"}] data (map value->data value))))
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "head" params)]
      (info (pr-str value) (pr-str {:data data}))
      (is (= {"red" "red"} data (value->data value)))))

  (testing "repeat should accumulate the resulting data"
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "repeat 3 random" params)]
      (is (= data (map value->data value)))))

  (testing "keys and vals should propagate data"
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "keys" params)]
      (is (= ["red" "green" "blue"] value))
      (is (= sample-data data)))
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "vals"
                                                 (assoc params
                                                        :opts {:foo :bar}))]
      (is (= [:bar] value))
      (is (= sample-data data))))

  (testing "droplast and rest should propagate data"
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "droplast" params)]
      (is (= data (map value->data value))))

    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "rest" params)]
      (is (= data (map value->data value)))))

  (testing "sort propagates sorted data"
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "sort" params)]
      (info "sorted data" (pr-str data))
      (info "sorted opts" (pr-str value))
      (is (= ["blue" "green" "red"] value))
      (is (= data (map value->data value)))))

  (testing "sortnum propagates sorted data"
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "sortnum"
                                                 (assoc params
                                                        :opts ["2" "1" "3"]))]
      (is (= ["1" "2" "3"] value))
      (is (= [{"green" "green"} {"red" "red"} {"blue" "blue"}] data))))

  (testing "shuffle propagates shuffled data"
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "shuffle" params)]
      (is (= data (map value->data value)))))

  (testing "reverse propagates reversed data"
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                 "reverse" params)]
      (is (= data (map value->data value)))))

  (testing "grep propagates matched data"
    (let [{{:result/keys [value data]} :result} (command-execution-info
                                                  ;; only matches "red" and
                                                  ;; "green"
                                                 "grep e.$" params)]
      (is (= data (map value->data value)))))

  (testing "xargs still works on simple commands that don't return a map"
    (is (= ["value is red" "value is green" "value is blue"]
           (-> (command-execution-info
                 ;; only matches "red" and "green"
                "xargs echo value is" params)
               :result
               :result/value))))

  (testing "xargs accumulates and propagates data when it exists"
    (is (=
         (-> (command-execution-info
                 ;; only matches "red" and
                 ;; "green"
              "xargs trim" params)
             :result)
         #:result{:value ["red" "green" "blue"],
                  :data-collection [nil nil nil],
                  :data [{"red" "red"} {"green" "green"} {"blue" "blue"}]})))

  (testing
   "xargs should properly propagate data for each item when data-collection is
    present"
    (is
     (= (-> (command-execution-info
             "xargs render {{name}}"
             {:data [{:name "foo"} {:name "bar"} {:name "qux"}]
              :data-collection [{:name "foo"} {:name "bar"} {:name "qux"}]
              :opts ["foo" "bar" "qux"]
              :run-command? true})
            :result
            :result/value)
        ["foo" "bar" "qux"])))

  (testing "xargs falls back to data if opts not passed in"
    (is
     (=
      (-> (command-execution-info
           "xargs keys"
           ;; remove opts, forcing it to fallback to data
           (-> params (dissoc :opts)))
          :result)
      #:result{:value [["red"] ["green"] ["blue"]],
               :data-collection [nil nil nil],
               :data [{"red" "red"} {"green" "green"} {"blue" "blue"}]}))))
