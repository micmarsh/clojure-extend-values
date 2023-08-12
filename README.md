# extend-values

A Clojure library eventually intended to provide new versions of  `extend-type` and `extend-protocol` that work in terms of arbitrary values, rather than JVM (or JavaScript) types.

To illustrate with an extension of [an example of Clojure's documentation](https://clojuredocs.org/clojure.core/extend-type#example-542692cbc026201cdc326bc2)
```clojure

(defrecord Banana [qty])

(defprotocol Fruit
  (subtotal [item]))

(def other-fruits [{:name "blueberry", :qty 10} {:name "blackberry", :qty 20}])

(extend-protocol Fruit

  Banana
  (subtotal [item]
    (* 158 (:qty item)))

  {:name "blueberry" :qty _}
  (subtotal [item]
    (* 15 (:qty item)))

  {:name "blackberry" :qty _}
  (subtotal [item]
    (* 10 (:qty item)))

  {:qty _} (subtotal [x] (throw (Exception. "Unknown fruit type or non-fruit data"))))
```
Terrible data modeling practices aside, hopefully the above communicates the intent. All we currently (as of 8/12/23) have is a poor implementation of `extend-type`

## Usage
"Usage" may be a bit of a strong term, but `lein test` can be run to illustrate how `extend-type` works for a single value of a type, and has a long way to go to be helpful in any applicaiton.

## License

Copyright © 2023 Michael Marsh

Distributed under the Eclipse Public License version 1.0
