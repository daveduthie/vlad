(ns daveduthie.vlad)


;; Produce schemas from nested maps
;; ================================


(defn pass-thru [x & args] x)
(defn prepend [xs yx] (vec (into yx xs)))
(defn opt [x] (with-meta x (assoc (meta x) ::opt true)))
(defn req [x] (with-meta x (assoc (meta x) ::opt false)))
(defn ord [n x] (with-meta x (assoc (meta x) ::ord n)))
(defn ctx [ctx x] (with-meta x (assoc (meta x) ::ctx ctx)))
(defn idx [xs] (zipmap (range) xs))


(def meta-ks [::ord ::opt ::ctx])
(defn mmeta
  ([x y] (with-meta y (merge (select-keys (meta x) meta-ks) (meta y))))
  ([x y & more] (reduce mmeta (mmeta x y) more)))


(defn flatten-map
  "Converts a nested map into a sequence of pairs `[path value]`.
  Merges metadata from parents into children (child keys win)."
  ([m] (flatten-map m [] []))
  ([m path acc]
   (reduce
    (fn [acc [k x]]
      (let [path+ (conj path k)]
        (cond
          (map? x)    (flatten-map (mmeta m x) path+ acc)
          (vector? x) (conj acc [path+ (mmeta m x (mapv (partial mmeta m x) x))])
          :else       (throw (ex-info "Not implemented" {:type (type m)})))))
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
  [conformed
   {:as   validator
    :keys [deps? pred coerce msg opt]
    :or   {coerce identity
           pred   pass-thru}}
   value]
  (let [ctx     (or (::ctx (meta validator)) [])
        coerced (coerce value)
        result  (if deps?
                  (pred coerced (get-in conformed ctx))
                  (pred coerced))]
    (if result
      [:ok coerced]
      [:err msg])))


(defn validate-leaf*
  [value validators conformed]
  (cond value
        (reduce
         (fn [[_ value] validator]
           (let [[res data :as result]
                 (apply-validator conformed validator value)]
             (case res
               :ok result
               (reduced result))))
         [:ok value]
         validators)

        (-> validators meta ::opt) [:none]
        :else                      [:err "Required value"]))


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

