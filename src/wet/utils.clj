(ns wet.utils
  (:import (java.util Date)))

(defn safe-num
  [v]
  (cond
    (number? v) v
    (string? v) (cond
                  (re-find #"^-?\d+$" v) (Long. v)
                  (re-find #"^-?(\d+\.\d*|\d*\.\d+)$" v) (Float. v))))

(defn safe-date
  [v]
  (cond
    (integer? v) (if (zero? (quot v 1E10)) (Date. (* v 1000)) (Date. v))
    (instance? Date v) v
    (= "now" v) (Date.)
    (string? v) (try
                  (Date. v)
                  (catch IllegalArgumentException _ nil))))

(defn safe-str
  [v]
  (when v
    (if (keyword? v)
      (name v)
      (str v))))
