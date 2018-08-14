(ns wet.impl.rendering
  (:require [clojure.walk :as walk]
            [wet.filters :as filters]
            [wet.impl.parser.nodes
             #?@(:cljs [:refer [Assertion Assign Break Capture Case CollIndex
                                Continue Decrement Filter For If Increment
                                Lookup ObjectExpr PredicateAnd PredicateOr
                                IntRange Template Unless]])]
            [wet.impl.utils :as utils])
  #?(:clj (:import (wet.impl.parser.nodes
                     Assertion Assign Break Capture Case CollIndex
                     Continue Decrement Filter For If Increment
                     Lookup ObjectExpr PredicateAnd PredicateOr
                     IntRange Template Unless))))

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
                      (instance? IntRange node) resolve-range)]
    (resolver node context)
    node))

(defn- retrieve-filter
  "Looks for a Liquid filter, first in :filters provided by user,
   then in core filters library."
  [context filter-name]
  (or (get-in context [:filters filter-name])
      (get filters/CORE-FILTERS filter-name)))

(defn- apply-filter
  [{:keys [name args]} s context]
  (if-let [f (retrieve-filter context name)]
    (->> args
         (map #(resolve-object % context))
         (apply (partial f s)))
    s))

(defn- resolve-lookup
  [node context]
  (letfn [(->?vec [v] (cond-> v (sequential? v) (vec)))]
    (reduce
      (fn [res f]
        (let [v (->?vec res)]
          (cond
            (instance? Filter f) (apply-filter f v context)
            (instance? CollIndex f)
            (let [key (if (instance? Lookup (:key f))
                        (-> (resolve-lookup (:key f) context)
                            (as-> v* (cond-> v* (keyword? v*) name)))
                        (:key f))]
              (cond
                (not (sequential? v)) (get v key)
                (utils/safe-long key) (get v (utils/safe-long key))
                (= "size" key) (count v)
                (= "last" key) (last v)
                (= "first" key) (first v))))))
      (-> (if (contains? (:params context) (:name node))
            (get-in context [:params (:name node)])
            (get @(::global-scope context) (:name node)))
          (walk/stringify-keys)
          (->?vec))
      (:fns node))))

(defn- resolve-object-expr
  [node context]
  (let [{:keys [obj filters]} node
        v (resolve-object obj context)]
    (reduce (fn [res f] (apply-filter f res context)) v filters)))

(def ^:private NUMERIC-OPERATORS
  {">" > ">=" >= "<" < "<=" <=})

(defn- eval-assertion
  [node context]
  (let [{:keys [operator operands]} node
        operands* (map #(resolve-object % context) operands)]
    (case operator
      "==" (apply = operands*)
      "!=" (apply not= operands*)
      "contains" (cond
                   (every? string? operands*)
                   (.contains (first operands*) (second operands*))
                   (sequential? (first operands*))
                   (contains? (set (first operands*)) (second operands*))
                   :else false)
      (when-let [operator* (get NUMERIC-OPERATORS operator)]
        (when (every? number? operands*)
          (apply operator* operands*))))))

(defn- eval-predicate
  [node context]
  (letfn [(i? [t] (instance? t node))]
    (cond
      (i? Lookup) (resolve-lookup node context)
      (i? Assertion) (eval-assertion node context)
      (i? PredicateOr) (some #(eval-predicate % context) (:predicates node))
      (i? PredicateAnd) (every? #(eval-predicate % context) (:predicates node))
      :else node)))

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
          (some-> (:else node) :template (eval-node context) first))))))

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
  (let [coll (->> (resolve-object collection context)
                   (apply-for-opts for-opts))
        length (count coll)]
    (loop [res ""
           coll* coll
           forloop {:index 1
                    :index0 0
                    :rindex length
                    :rindex0 (dec length)
                    :length length
                    :first true
                    :last (> 2 length)}]
      (if-let [item (first coll*)]
        (let [index (:index forloop)
              context* (-> context
                           (assoc-in [:params var] item)
                           (assoc-in [:params "forloop"] forloop))
              item* (try
                      (first (eval-node template context*))
                      (catch #?(:clj Exception :cljs js/Error) e
                        (or (::iteration (ex-data e)) (throw e))))
              forloop* (-> forloop
                           (update :index inc)
                           (update :index0 inc)
                           (update :rindex dec)
                           (update :rindex0 dec)
                           (assoc :first false)
                           (assoc :last (>= (inc index) length)))]
          (case item*
            ::break res
            ::continue (recur res (rest coll*) forloop*)
            (recur (str res item*) (rest coll*) forloop*)))
        res))))

(defn- update-counter!
  [node context f init]
  (let [v (f (get-in @(::global-scope context) [::counters (:var node)] init))]
    (swap! (::global-scope context) assoc-in [::counters (:var node)] v)
    [(str v) context]))

(defmulti eval-node (fn [node _] (type node)))

(defmethod eval-node #?(:clj String :cljs js/String)
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
    (swap! (::global-scope context) assoc (:var node) v)
    ["" context]))

(defmethod eval-node Capture
  [node context]
  (let [[template _] (eval-node (:template node) context)]
    (swap! (::global-scope context) assoc (:var node) template)
    ["" context]))

(defmethod eval-node Increment
  [node context]
  (update-counter! node context inc -1))

(defmethod eval-node Decrement
  [node context]
  (update-counter! node context dec 0))

(defmethod eval-node Break
  [_ _]
  (throw (ex-info nil {::iteration ::break})))

(defmethod eval-node Continue
  [_ _]
  (throw (ex-info nil {::iteration ::continue})))

(defn eval-template
  [template-node context]
  (->> (assoc context ::global-scope (atom {}))
       (eval-node template-node)
       (first)))
