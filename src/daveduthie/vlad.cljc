(ns daveduthie.vlad)


(defn pass-thru [val state deps] val)
(defn prepend [xs yx] (vec (into yx xs)))


;; TODO: daveduthie 2018-07-24: allow one-arity predicates
(defn apply-validator
  [state ctx
   {:keys [deps? pred coerce msg opt]
    :or   {coerce identity
           pred   pass-thru}}
   value]
  (let [coerced (coerce value)
        result  (if deps?
                  (pred coerced state ctx)
                  (pred coerced))]
    (if result
      [:ok coerced]
      [:err msg])))


(defn validate-leaf*
  [value validators state]
  (let [ctx (or (::ctx (meta validators)) [])]
    (cond
      value      (reduce
                  (fn [[_ value] validator]
                    (let [[res data :as result]
                          (apply-validator state ctx validator value)]
                      (case res
                        :ok result
                        (reduced result))))
                  [:ok value]
                  validators)
      (-> validators
          meta
          ::opt) [:none]
      :else      [:err "Required value"])))


(defn validate-leaf
  [value validators path conformed errors]
  (let [[res data] (validate-leaf* value validators conformed)]
    (case res
      :none [conformed errors]
      :ok   [(assoc-in conformed path data) errors]
      :err  [conformed (assoc-in errors path data)])))


;; TODO: daveduthie 2018-07-23: Switch to max-ord: more ergonomic
(defn min-ord [x]
  (let [mx (or (::ord (meta x)) 1000)]
    (if-not (map? x)
      mx
      (apply min mx (map (comp min-ord val) x)))))


(defn traverse
  ([f schema conformed] (traverse f schema conformed nil []))
  ([f schema conformed errors path]
   (reduce (fn [[conformed errors] [k v]]
             (let [path+ (conj path k)
                   v     (with-meta v (merge (select-keys (meta schema)
                                                          [::opt ::ord ::ctx])
                                             (meta v)))]
               (if (map? v)
                 (traverse f v conformed errors path+)
                 (f v path+ conformed errors))))
           [conformed errors]
           (sort-by (comp min-ord val) schema))))


(defn validate
  [schema data]
  (traverse
   (fn [validators path conformed errors]
     (validate-leaf (get-in conformed path)
                    validators
                    path
                    conformed
                    errors))
   schema
   data))


(defn opt [x] (with-meta x (assoc (meta x) ::opt true)))
(defn req [x] (with-meta x (assoc (meta x) ::opt false)))
(defn ord [n x] (with-meta x (assoc (meta x) ::ord n)))
(defn ctx [ctx x] (with-meta x (update (meta x) ::ctx prepend ctx)))

(defn idx [xs] (zipmap (range) xs))
