(ns daveduthie.vlad-test
  (:require [clojure.test :refer :all]
            [daveduthie.vlad :refer :all]
            [daveduthie.vlad.validators :refer :all]))


(deftest basic-validation-test
  (let [data               {:a {:b "banana"}
                            :c 5}
        +schema+           {:a {:b [int-validator]}
                            :c [int-validator]}
        [conformed errors] (validate +schema+ data)]
    (is {:a {:b "Must be integer"}}
        errors)
    (is (= [{:a {:b 100} :c 5} nil]
           (validate +schema+ (assoc-in data [:a :b] 100))))))


(deftest optional-test
  (let [data               {:a "banana"
                            :b {:d :1}
                            :e [:strawberry :fruit/loves "banana"]}
        +schema+           {:a [string-validator
                                (min-length-validator 4)]
                            :b (opt {:c [int-validator]
                                     :d (req [int-validator])})
                            :e (idx (repeat 3 [(type-validator (type :foo))]))}
        [conformed errors] (validate +schema+ data)]
    (is (= {:b {:d "Must be an integer"}
            :e {2 (str "Must be a " (type :foo))}}
           errors))))


(deftest ctx-test
  (let [->int               (fn [v] (if (int? v) v
                                        (try (Integer/parseInt v)
                                             (catch ClassCastException ex)
                                             (catch NumberFormatException ex))))
        example-validator   {:coerce ->int
                             :deps?  true
                             :pred   (fn [total state]
                                       (let [other-total
                                             (reduce (fnil + 0 0)
                                                     (map (fn [path] (get-in state path))
                                                          [[:some-key] [:some-other :key]]))]
                                         (>= total other-total)))
                             :msg    "Total must be greater than the sum of the others"}
        example-validator-2 {:coerce ->int
                             :deps?  true
                             :pred   (fn [total state]
                                       (let [maximum (get-in state [:d])]
                                         (<= total maximum)))
                             :msg    "Total must not exceed the maximum allowed value"}
        data                {:k {:some-key   "25"
                                 :some-other {:key "75"}}
                             :x {:abc "100"
                                 :d   100}}
        +schema+            {:x {:abc (ord 1 (ctx [:k] [int-validator
                                                        example-validator
                                                        (ctx [:x] example-validator-2)]))}
                             :k {:some-key   [int-validator]
                                 :some-other {:key [int-validator]}}}
        [conformed errors]  (validate +schema+ data)]
    (is (nil? errors))))

