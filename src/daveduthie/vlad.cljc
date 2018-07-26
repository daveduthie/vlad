(ns daveduthie.vlad)


;; Produce schemas from nested maps
;; ================================


(defn pass-thru [x & args] x)
(defn prepend [xs yx] (vec (into yx xs)))
(defn opt [x] (with-meta x (assoc (meta x) ::opt true)))
(defn req [x] (with-meta x (assoc (meta x) ::opt false)))
(defn ord [n x] (with-meta x (assoc (meta x) ::ord n)))
(defn ctx [ctx x] (with-meta x (update (meta x) ::ctx prepend ctx)))
(defn idx [xs] (zipmap (range) xs))


(def meta-ks [::ord ::opt ::ctx])


(defn flatten-map
  "Converts a nested map into a sequence of pairs `[path value]`.
  Merges metadata from parents into children (child keys win)."
  ([m] (flatten-map m [] []))
  ([m path acc]
   (reduce (fn [acc [k x]]
             (let [path+ (conj path k)
                   x     (with-meta x (merge (select-keys (meta m) meta-ks)
                                             (meta x)))]
               (if (map? x)
                 (flatten-map x path+ acc)
                 (conj acc [path+ x]))))
           acc m)))


(defn ->flat-schema
  "Produces a flattened, struct-compatible schema from a nested map."
  [schema]
  (->> schema
       flatten-map
       (sort-by #(-> % second meta ::ord (or 0)))))


;; Validation for flat schemas
;; ===========================


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
  [value validators conformed]
  (let [ctx (or (::ctx (meta validators)) [])]
    (cond value
          (reduce
           (fn [[_ value] validator]
             (let [[res data :as result]
                   (apply-validator conformed ctx validator value)]
               (case res
                 :ok result
                 (reduced result))))
           [:ok value]
           validators)

          (-> validators meta ::opt) [:none]
          :else                      [:err "Required value"])))


(defn validate-leaf
  [value validators path conformed errors]
  (let [[res data] (validate-leaf* value validators conformed)]
    (case res
      :none [conformed errors]
      :ok   [(assoc-in conformed path data) errors]
      :err  [conformed (assoc-in errors path data)])))


(defn validate
  "`schema -> data -> [conformed errors]`"
  [schema data]
  (reduce
   (fn [[conformed errors] [path validators]]
     (let [data-at (get-in conformed path)]
       (validate-leaf data-at validators path conformed errors)))
   [data nil]
   (->flat-schema schema)))

