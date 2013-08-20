(ns jack-frost.data)

"A Helper namespace for hold different data types that should be confirmed as working with the system"

(def data
  {:int 1
   :numbers [4.2 4.0 100]
   :set #{"Foo" "bar" "baz"}
   :vector [:foo :bar :baz]
   :range (range 10)})
