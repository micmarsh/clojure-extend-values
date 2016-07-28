(ns extend-values.core
  (:require [clojure.core.match :refer [match]]))

(def value-fns
  "Structure {Class {Protocol {value {:fn-name (fn [x] ...)}}}}"
  (atom {}))

(defn only-value-mmaps
  [value-fns class protocol]
  (-> value-fns
      (get-in [class protocol])
      (dissoc ::base-class-impl)))

(defn only-base-impl-mmap 
  [value-fns class protocol]
  (get-in value-fns [class protocol ::base-class-impl])) 

(defn match-pair
  [fn-name [value mmap]]
  `[[~value] (get ~mmap ~fn-name)])

(defn new-mmap
  [atype proto value-fns]
  (let [value-mmaps (only-value-mmaps value-fns atype proto)
        base-impl-mmap (only-base-impl-mmap value-fns atype proto)
        fn-names (keys (first (vals value-mmaps)))]
    (into { }
          (for [name fn-names]
            [name
             (fn [arg & rest]
               (let [f (eval
                        `(match [~arg]
                                ~@(mapcat (partial match-pair name) value-mmaps)
                                [_#] (do (when-not ~base-impl-mmap
                                           (throw (ex-info "u fuked up" {})))
                                         (get ~base-impl-mmap ~name))))]
                 (apply f arg rest)))]))))


(defn add-value-mmap [existing atype proto value mmap]
  (if (nil? existing)
    (with-meta mmap {::extend++ true})
    (throw (IllegalArgumentException. 
            (str atype  " already directly implements " (:on-interface proto) "for value: "  (pr-str value))))))

(defn base-impl-mmap? [mmap]
  (and (some? mmap) (not (::extend++ (meta mmap)))))

(defn extend++*
  [atype value proto+mmaps]
    (doseq [[proto mmap] (partition 2 proto+mmaps)]
      (swap! value-fns update-in [atype proto value] add-value-mmap atype proto value mmap)
      (-reset-methods
       (alter-var-root (:var proto) update-in 
                       [:impls atype]
                       (fn [old-mmap]
                         (when (base-impl-mmap? old-mmap)
                           (swap! value-fns assoc-in [atype proto ::base-class-impl] old-mmap))
                         (new-mmap atype proto @value-fns))))))

(defn extend++
  [atype & proto+mmaps]
  (assert (not= clojure.lang.Symbol (class atype)) 
          "Not allowed to extend to a Symbol literal value (messes with core.match)")
  (if (class? atype)
    (apply extend atype proto+mmaps)
    (extend++* (class atype) atype proto+mmaps)))

;; 

(comment

  (defprotocol Protocol 
    (p-stuff [x])
    (moar-p-stuff [x] [x y]))

  (defn unset-proto [proto atype]
    (-reset-methods (alter-var-root (:var proto) update :impls dissoc atype)))

  (extend java.lang.String 
    Protocol 
    {:p-stuff (fn [x] (str "p-stuff " x)) 
     :moar-p-stuff (fn ([x] (str "moar-p-stuff " x)) 
                     ([x y] (str "moar-p-stuff (two args) " x " " y)))})
  
  (extend++ java.lang.String 
            Protocol 
            {:p-stuff (fn [x] (str "p-stuff " x)) 
             :moar-p-stuff (fn ([x] (str "moar-p-stuff " x)) 
                             ([x y] (str "moar-p-stuff (two args) " x " " y)))})
  
  
  (extend++ "hello world"
            Protocol
            {:p-stuff (fn [x] "boom, hotness") 
             :moar-p-stuff (fn ([x] "single arg moar hotness") 
                             ([x y] "double arg moar hotness"))})
  
 (p-stuff "yo")
 (moar-p-stuff "yo" "dawg")
 
 (unset-proto Protocol java.lang.String)
 (reset! value-fns {})
 
 ;; TODO after roughly repl testing, have the following tentative
 ;; description
 ;; * works for extend++'ing to one value
 ;; * ??? for extend++'ing to > 1 value of the same class
 ;; * looks like using core macros will overwrite everything, which
 ;; may be unavoidable
 ;;  * can think further, but intial hack could be messing w/ :var
 ;;  :impls to replace it w/ a "map" that throws if accessed
 ;;  incorrectly, warning u to use extend++ instead. Nasty, but
 ;;  seemingly feasible
 ;; * will also need to re-impl higher-order macros, but whateva

)
