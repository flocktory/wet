(ns wet.rendering
  (:require [clojure.walk :as walk]
            [wet
             [filters :as filters]
             [utils :as utils]]
            [wet.parser.nodes])
  (:import (wet.parser.nodes Assertion Assign Break Capture Case CollIndex
                             Continue Decrement Filter For If
                             Increment Lookup ObjectExpr PredicateAnd
                             PredicateOr Range Template Unless)))

(declare eval-node)
(declare resolve-lookup)

(defn- resolve-range
  [{:keys [start end]} context]
  (let [resolve* (fn [node]
                   (if (instance? Lookup node)
                     (resolve-lookup node context)
                     node))
        start* (utils/safe-long (resolve* start))
        end* (utils/safe-long (resolve* end))]
    (when (and start* end*)
      (if (> end* start*)
        (range start* (inc end*))
        (range start* (dec end*) -1)))))

(defn- resolve-object
  [node context]
  (if-let [resolver (cond
                      (instance? Lookup node) resolve-lookup
                      (instance? Range node) resolve-range)]
    (resolver node context)
    node))

(defn- retrieve-filter
  "Looks for a Liquid filter, first in :filters provided by user,
   then in core filters library."
  [context filter-name]
  (get-in context [:filters filter-name] (filters/find-by-name filter-name)))

(defn- apply-filter
  [{:keys [name args]} s context]
  (if-let [f (retrieve-filter context name)]
    (->> args
         (map #(resolve-object % context))
         (apply (partial f s)))
    s))

(defn- resolve-lookup
  [node context]
  (let [var-name (:name node)]
    (reduce
      (fn [res f]
        (cond
          (instance? Filter f) (apply-filter f res context)
          (instance? CollIndex f) (let [key (:key f)]
                                    (if (sequential? res)
                                      (when (integer? key) (get res key))
                                      (get (walk/stringify-keys res) key)))))
      (get-in context [:params var-name])
      (:fns node))))

(defn- resolve-object-expr
  [node context]
  (let [{:keys [obj filters]} node
        v (resolve-object obj context)]
    (reduce (fn [res f] (apply-filter f res context)) v filters)))

(defn- eval-assertion
  [node context]
  (let [{:keys [operator operands]} node
        operands* (map #(resolve-object % context) operands)]
    (case operator
      "==" (apply = operands*)
      "!=" (apply not= operands*)
      (">" ">=" "<" "<=") (if (every? number? operands*)
                            (apply (resolve (symbol operator)) operands*)
                            false)
      "contains" (cond
                   (every? string? operands*)
                   (.contains (first operands*) (second operands*))
                   (sequential? (first operands*))
                   (some? ((set (first operands*)) (second operands*)))
                   :else false))))

(defn- eval-predicate
  [node context]
  (letfn [(i? [t] (instance? t node))]
    (cond
      (i? Lookup) (resolve-lookup node context)
      (i? Assertion) (eval-assertion node context)
      (i? PredicateOr) (some #(eval-predicate % context)
                             [(:pred1 node) (:pred2 node)])
      (i? PredicateAnd) (every? #(eval-predicate % context)
                                [(:pred1 node) (:pred2 node)]))))

(defn- eval-condition-expr
  [node context]
  (let [conditions (:conditions node)
        eval-pred* (fn [n] (eval-predicate (:pred n) context))
        eval-template* (fn [n] (first (eval-node (:template n) context)))
        entry-holds? (cond-> (eval-pred* (first conditions))
                       (instance? Unless node) not)]
    (if entry-holds?
      (eval-template* (first conditions))
      (loop [conditions* (rest conditions)]
        (if-let [condition (first conditions*)]
          (if (eval-pred* condition)
            (eval-template* condition)
            (recur (rest conditions*)))
          (some-> (:else node) eval-template* first))))))

(defn- eval-case
  [node context]
  (let [val (resolve-object (:val node) context)]
    (loop [whens (:whens node)]
      (if-let [when* (first whens)]
        (if (= val (resolve-object (:val when*) context))
          (first (eval-node (:template when*) context))
          (recur (rest whens)))
        (some-> (:else node) :template (eval-node context) first)))))

(defn- apply-for-opts
  [{:keys [limit offset reversed?]} coll]
  (cond->> coll
    offset (drop offset)
    limit (take limit)
    reversed? (reverse)))

(defn- eval-for
  [{:keys [var collection for-opts template]} context]
  (loop [res ""
         coll (->> (resolve-object collection context)
                   (apply-for-opts for-opts))]
    (if-let [item (first coll)]
      (let [item* (try
                    (first
                      (eval-node template (assoc-in context [:params var] item)))
                    (catch Exception e
                      (or (::iteration (ex-data e)) (throw e))))]
        (case item*
          ::break res
          ::continue (recur res (rest coll))
          (recur (str res item*) (rest coll))))
      res)))

(defn- update-counter
  [node context f]
  ["" (update-in context [:params (:var node)] f)])

(defmulti eval-node (fn [node _] (type node)))

(defmethod eval-node String
  [node context]
  [node context])

(defmethod eval-node Template
  [node context]
  (reduce
    (fn [[res context*] node]
      (let [[node* context**] (eval-node node context*)]
        [(str res node*) context**]))
    ["" context]
    (:nodes node)))

(defmethod eval-node ObjectExpr
  [node context]
  [(resolve-object-expr node context) context])

(defmethod eval-node If
  [node context]
  [(eval-condition-expr node context) context])

(defmethod eval-node Unless
  [node context]
  [(eval-condition-expr node context) context])

(defmethod eval-node Case
  [node context]
  [(eval-case node context) context])

(defmethod eval-node For
  [node context]
  [(eval-for node context) context])

(defmethod eval-node Assign
  [node context]
  (let [[v _] (eval-node (:value node) context)]
    ["" (assoc-in context [:params (:var node)] v)]))

(defmethod eval-node Capture
  [node context]
  (let [[template _] (eval-node (:template node) context)]
    ["" (assoc-in context [:params (:var node)] template)]))

(defmethod eval-node Increment
  [node context]
  (update-counter node context inc))

(defmethod eval-node Decrement
  [node context]
  (update-counter node context dec))

(defmethod eval-node Break
  [_ _]
  (throw (ex-info nil {::iteration ::break})))

(defmethod eval-node Continue
  [_ _]
  (throw (ex-info nil {::iteration ::continue})))

(defn eval-template
  [template-node context]
  (eval-node template-node context))
