(ns wet.parser.nodes)

(defprotocol Parent
  (children [this]
    "Returns node's children that may contain object expressions."))

(defrecord Assertion [operator operands]
  Parent
  (children [_] (cons operator operands)))

(defrecord Assign [var value]
  Parent
  (children [_] [value]))

(defrecord Break [])

(defrecord Capture [var template]
  Parent
  (children [_] [template]))

(defrecord Case [val whens else]
  Parent
  (children [_] (concat [val] whens else)))

(defrecord CollIndex [key]
  Parent
  (children [_] [key]))

(defrecord Condition [pred template]
  Parent
  (children [_] [pred template]))

(defrecord Continue [])

(defrecord Decrement [var]
  Parent
  (children [_] [var]))

(defrecord Else [template]
  Parent
  (children [_] [template]))

(defrecord Filter [name args]
  Parent
  (children [_] (cons name args)))

(defrecord For [var collection for-opts template]
  Parent
  (children [_] (concat [var collection template for-opts])))

(defrecord ForLimit [val])

(defrecord ForOffset [val])

(defrecord ForOpts [limit offset reversed?])

(defrecord ForReversed [])

(defrecord If [conditions else]
  Parent
  (children [_] (cons else conditions)))

(defrecord Increment [var]
  Parent
  (children [_] [var]))

(defrecord Lookup [name fns]
  Parent
  (children [_] [name fns]))

(defrecord ObjectExpr [obj filters]
  Parent
  (children [_] (cons obj filters)))

(defrecord Operator [value]
  Parent
  (children [_] [value]))

(defrecord PredicateAnd [pred1 pred2]
  Parent
  (children [_] [pred1 pred2]))

(defrecord PredicateOr [pred1 pred2]
  Parent
  (children [_] [pred1 pred2]))

(defrecord Range [start end]
  Parent
  (children [_] [start end]))

(defrecord Template [nodes]
  Parent
  (children [_] nodes))

(defrecord Unless [conditions else]
  Parent
  (children [_] (cond else conditions)))

(defrecord When [val template]
  Parent
  (children [_] [val template]))
