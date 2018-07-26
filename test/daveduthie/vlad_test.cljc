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


(deftest complex-validator-test
  (let [example-validator  {:coerce (fn [v]
                                      (if (int? v) v
                                          (try (Integer/parseInt v)
                                               (catch ClassCastException ex)
                                               (catch NumberFormatException ex))))
                            :deps?  true
                            :pred   (fn [total state ctx]
                                      (let [other-total
                                            (reduce (fnil + 0 0)
                                                    (map (fn [path] (get-in state (prepend path ctx)))
                                                         [[:some-key] [:some-other :key]]))]
                                        (>= total other-total)))
                            :msg    "Total must be greater than the sum of the others"}
        data               {:k {:some-key   "25"
                                :some-other {:key "75"}}
                            :x {:abc 99}}
        +schema+           {:x {:abc (->> [int-validator example-validator] (ctx [:k]) (ord 1))}
                            :k {:some-key   [int-validator]
                                :some-other {:key [int-validator]}}}
        [conformed errors] (validate +schema+ data)]
    (is (= {:x {:abc "Total must be greater than the sum of the others"}}
           errors))))

