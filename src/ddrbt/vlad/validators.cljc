(ns ddrbt.vlad.validators)


(def int-validator
  {:coerce (fn [v] (if (int? v) v
                       #?(:clj (try (Integer/parseInt v)
                                    (catch Exception e))
                          :cljs (let [res (js/parseInt v)]
                                  (when-not (js/isNaN res) res)))))
   :pred   int?
   :msg    "Must be an integer"})


(def string-validator
  {:pred string?
   :msg  "Must be a string"})


(defn min-length-validator
  [len]
  {:pred #(>= (count %) len)
   :msg  (str "Length must be at least " len)})


(defn type-validator
  [type_]
  {:pred #(= (type %) type_)
   :msg  (str "Must be a " type_)})

{:first-name "Billy"
 :last-name "Foo"}

(defrecord Person [first-name last-name])

(->Person "John" "Schmidt")
