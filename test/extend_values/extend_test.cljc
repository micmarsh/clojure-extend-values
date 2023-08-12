(ns extend-values.extend-test
  (:refer-clojure :exclude [extend-type])
  (:require [ #?@(:cljs [cljs.test :refer-macros] :clj [clojure.test :refer]) 
             [deftest is testing run-tests use-fixtures]]
            [extend-values.extend #?(:clj :refer :cljs :refer-macros) [extend-type]]))

(defprotocol Protocol 
  (p-stuff [x])
  (moar-p-stuff [x] [x y]))

(defn clear-protocol [proto]
  (-reset-methods (alter-var-root (:var proto) update :impls empty)))

(defn ->clear-protocol-fixture [proto]
  (fn [test]
    (clear-protocol proto)
    (test)))

(use-fixtures :each 
  #?(:clj (->clear-protocol-fixture Protocol)
     :cljs {:before (fn [] (clear-protocol Protocol))}))

(deftest single-extend-call-works

  (extend-type "hello world"
    Protocol
    (p-stuff [x] (str "p-stuff " x)) 
    (moar-p-stuff 
      ([x] (str "single arg " x)) 
      ([x y] (str "double arg " x y))))

  (testing "Function calls on value work as expected"
    (is (= "p-stuff hello world" (p-stuff "hello world")))
    (is (= "single arg hello world" (moar-p-stuff "hello world")))
    (is (= "double arg hello world!" (moar-p-stuff "hello world" "!"))))

  (testing "WILL CHANGE Un-implemented default implementation throws"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No normal impl"
                          (p-stuff "hello")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No normal impl"
                          (moar-p-stuff "hello")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No normal impl"
                          (moar-p-stuff "hello" "world"))))
  (extend-type "hello"
    Protocol
    (p-stuff [x] "one")
    (moar-p-stuff ([x] "two") ([x y] "three")))


  (testing "Doesn't override old values over multiple calls")
  (is (= "one" (p-stuff "hello")))
  (is (= "p-stuff hello world" (p-stuff "hello world"))))
