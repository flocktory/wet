(ns wet.parser
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [wet.filters :as filters]
            [wet.parser.nodes :as nodes])
  (:import (wet.parser.nodes Condition Else Filter ForLimit ForOffset
                             ForReversed Lookup When)))

(def ^:private parse (insta/parser (io/resource "grammar.bnf")))

(defn- parse-template [& nodes] (nodes/->Template nodes))

(defn- parse-string [& nodes] (apply str nodes))

(defn- parse-condition-expr
  [constructor pred template & nodes]
  (let [conditions (->> nodes
                        (take-while (partial instance? Condition))
                        (cons (nodes/->Condition pred template)))
        else (when (instance? Else (last nodes)) (last nodes))]
    (constructor conditions else)))

(def ^:private parse-if (partial parse-condition-expr nodes/->If))

(def ^:private parse-unless (partial parse-condition-expr nodes/->Unless))

(defn- parse-case
  [val & nodes]
  (let [whens (take-while (partial instance? When) nodes)
        else (when (instance? Else (last nodes)) (last nodes))]
    (nodes/->Case val whens else)))

(defn- parse-assertion
  ([v] v)
  ([operand1 operator operand2]
   (nodes/->Assertion (:value operator) [operand1 operand2])))

(defn- parse-or
  ([pred] pred)
  ([pred1 pred2] (nodes/->PredicateOr pred1 pred2)))

(defn- parse-and
  ([pred] pred)
  ([pred1 pred2] (nodes/->PredicateAnd pred1 pred2)))

(defn- parse-object-expr [obj & filters] (nodes/->ObjectExpr obj filters))

(defn- parse-filter [name & args] (nodes/->Filter name args))

(defn- parse-for-opts
  [& nodes]
  (letfn [(find-node [t] (first (filter (partial instance? t) nodes)))]
    (nodes/map->ForOpts {:limit (:val (find-node ForLimit))
                         :offset (:val (find-node ForOffset))
                         :reversed? (some? (find-node ForReversed))})))

(defn- parse-for
  ([var collection template]
   (parse-for var collection nil template))
  ([var collection for-opts template]
   (nodes/->For var collection for-opts template)))

(defn- parse-lookup [name & nodes] (nodes/->Lookup name nodes))

(def ^:private transformer
  {:template parse-template
   :b identity
   ;; Data types
   :int read-string
   :float read-string
   :bool read-string
   :sq-str-set identity
   :sq-str-escape read-string
   :dq-str-set identity
   :dq-str-escape read-string
   :string parse-string
   ;; Lookup
   :lookup parse-lookup
   :object-expr parse-object-expr
   :filter parse-filter
   :index nodes/->CollIndex
   ;; Assignment
   :capture nodes/->Capture
   :assign nodes/->Assign
   :increment nodes/->Increment
   :decrement nodes/->Decrement
   ;; Conditions
   :operator nodes/->Operator
   :assertion parse-assertion
   :predicate identity
   :else nodes/->Else
   :elsif nodes/->Condition
   :if parse-if
   :unless parse-unless
   :case parse-case
   :when nodes/->When
   :and parse-and
   :or parse-or
   ;; Iteration
   :for parse-for
   :for-opts parse-for-opts
   :for-limit nodes/->ForLimit
   :for-offset nodes/->ForOffset
   :for-reversed nodes/->ForReversed
   :break nodes/->Break
   :continue nodes/->Continue
   :range-start identity
   :range-end identity
   :range nodes/->Range
   ;; Raw
   :raw-block identity
   :raw-body identity})

(defn- transform
  [template]
  (insta/transform transformer template))

(defn parse-and-transform
  [template]
  (let [parsed-template (parse template)]
    (cond
      (insta/failure? parsed-template)
      (let [{:keys [text line column]} parsed-template
            error-message (format "Parse error: %s (%s:%s)" text line column)
            error-context {::error ::parse-error
                           ::parse-error-line line
                           ::parse-error-column column
                           ::parse-error-text text}]
        (throw (ex-info error-message error-context)))
      :else (transform parsed-template))))

(defn analyze
  [transformed-template]
  (let [nodes (->> transformed-template
                   (tree-seq
                     (partial satisfies? nodes/Parent)
                     nodes/children))
        lookups (filter (partial instance? Lookup) nodes)
        filters (->> nodes
                     (filter (partial instance? Filter))
                     (map :name)
                     (distinct)
                     (group-by (comp some? (partial filters/find-by-name))))]
    {:lookups (set (map :name lookups))
     :core-filters (set (get filters true))
     :custom-filters (set (get filters false))}))
