(ns ddrbt.vlad)


(def ^:dynamic *mode* :vlad)

(defn ->v
  [path validators dep]
  (case *mode*
    :vlad   [path validators dep]
    :struct [path validators]))


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
  ([m] (flatten-map m [] [] []))
  ([m path acc dep]
   (reduce
    (fn [acc [k x]]
      (let [path+ (conj path k)
            dep   (if (::opt (meta m)) path dep)]
        (cond
          ;;; TODO: daveduthie 2018-07-31: ?
          (satisfies? Validator x) (conj acc path+ x)
          (map? x)                 (flatten-map (mmeta m x) path+ acc dep)
          ;;; TODO: daveduthie 2018-07-31: ?
          (vector? x)              (conj acc
                                         (->v path+
                                              (mmeta m x (mapv (partial mmeta m x) x))
                                              dep))
          :else                    (throw (ex-info "Not implemented" {:type (type m)})))))
    acc m)))

#_
(flatten-map )


(defn ->flat-schema
  "Produces a flattened schema from a nested map."
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


(defn off-the-hook?
  [validators dep conformed]
  (boolean
   (or (-> validators meta ::opt)
       (nil? (get-in conformed dep)))))


(defn validate-leaf*
  [data validators dep conformed]
  (cond data
        (reduce
         (fn [[_ data] validator]
           (let [[res data :as result]
                 (apply-validator conformed validator data)]
             (case res
               :ok result
               (reduced result))))
         [:ok data]
         validators)

        (off-the-hook? validators dep conformed) [:none]
        :else                                    [:err "Required value"]))


(defn validate-leaf
  [data path validators dep conformed errors]
  (let [[res data] (validate-leaf* data validators dep conformed)]
    (case res
      :none [conformed errors]
      :ok   [(assoc-in conformed path data) errors]
      :err  [conformed (assoc-in errors path data)])))


(defn validate
  "`schema -> data -> [conformed errors]`"
  [schema data]
  (reduce
   (fn [[conformed errors] vlad]
     (validate vlad conformed errors))
   [data nil]
   (->flat-schema schema)))
