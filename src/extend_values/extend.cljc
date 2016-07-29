(ns extend-values.extend
  (:refer-clojure :exclude [extend-type])
  (:require [clojure.core.match :refer [match]]))

(def code-storage
  "{Class {Protocol {'fn-name {value {[args] (code ...) ...} 
                               value2 {[args] (code ...) ...}}}}}"
  (atom {}))

(defn merge-recursive [& maps]
  (apply merge-with (fn [val1 val2]
                      (if (every? map? [val1 val2])
                        (merge-recursive val1 val2)
                        val2))
         maps))

(defn match-clause
  [[value code]]
  [[value] code])

(defn build-mmap
  "Input {'fn-name {[arg] {value1 (do (code))
                           value2 (do (code) (more code))}}
                    [arg1 arg2] {value1 (do (code arg1))
                                 value2 (do arg1 (code arg2 arg1))}}"
  [fns]
  (into { }
        (for [[name arities] fns]
          [(keyword name)
           `(fn ~@(for [[args-list value-map] arities]
                   `(~args-list
                     (match [~(first args-list)]
                            ~@(mapcat match-clause value-map)
                            [_#] (throw (ex-info "No normal impl" {}))))))])))

(defn arity-map [value impls]
  (cond 
    (vector? (first impls)) {(first impls) {value `(do ~@(next impls))}}
    (list? (first impls)) (into {} (map (partial arity-map value)) impls)
    (nil? (first impls)) ::done
    :else (throw (ex-info "problem" {:data impls}))))

(defn parse-impls [value specs]
  (loop [ret {} s specs]
    (if (seq s)
      (recur (->> (take-while seq? (next s))
                  (map (fn [[name & rest]] {name (arity-map value rest)}))
                  (apply merge)
                  (assoc ret (first s)))
             (drop-while seq? (next s)))
      ret)))

(defn emit-mmaps [value specs]
  (->> specs
       (parse-impls value)
       (mapcat (fn [[p map]] [p (build-mmap map)]))))

(defmacro extend-type [value & specs]
  `(clojure.core/extend ~(class value) ~@(emit-mmaps value specs)))

;; Re-write!
;; * needs to utilize atom to account for multiple calls
;; * probably needs to account for initial/base implementations in
;; some way
;; * could do similar as you were doing before, but w/ storing code
;; now rather than fn
;;  * ooo, could store it as some dummy symbol value, abstract clause
;;  generator so it'll push that clause to bottom 
;; * though of core match issue: don't have any way of dealing w/
;; core.match order. Maybe it doesn't matter (other than catch-all
;; symbol at bottom)

(comment

  (defprotocol Protocol 
    (p-stuff [x])
    (moar-p-stuff [x] [x y]))

  (defn unset-proto [proto atype]
    (-reset-methods (alter-var-root (:var proto) update :impls dissoc atype)))
  
  (clojure.core/extend-type java.lang.String 
    Protocol 
    (p-stuff [x] (str "p-stuff " x)) 
    (moar-p-stuff
      ([x] (str "moar-p-stuff " x)) 
      ([x y] (str "moar-p-stuff (two args) " x " " y))))
  
  (extend-type "hello world"
    Protocol
    (p-stuff [x] "boom, hotness") 
    (moar-p-stuff ([x] "single arg moar hotness") 
      ([x y] "double arg moar hotness")))

  (extend-type {:person/type :person/the-best}
    Protocol 
    (p-stuff [x] (:person/p x))     
    (moar-p-stuff 
      ([x] (keys x)) 
      ([x y] "double arg moar hotness")))
 
 (unset-proto Protocol java.lang.String)
 (reset! value-fns {})
)
