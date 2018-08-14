(ns wet.impl.utils
  #?(:clj (:import (java.util Date))))

#?(:cljs (defn- non-NaN [v] (when-not (js/isNaN v) v)))

(defn safe-long
  [v]
  #?(:clj (try
            (cond
              (number? v) (.longValue v)
              :else (Long/valueOf v))
            (catch IllegalArgumentException _ nil)
            (catch NumberFormatException _ nil))
     :cljs (non-NaN (js/parseInt v 10))))

(defn safe-num
  ([v] (safe-num v nil))
  ([v fallback]
   #?(:clj (or (cond
                 (number? v) v
                 (string? v) (cond
                               (re-find #"^-?\d+$" v) (Long. v)
                               (re-find #"^-?(\d+\.\d*|\d*\.\d+)$" v) (Double. v)))
               fallback)
      :cljs (or (non-NaN (js/Number. v)) fallback))))

(defn safe-date
  [v]
  (cond
    (integer? v)
    (let [v* (if (zero? (quot v 1E10)) (* v 1000) v)]
      #?(:clj (Date. v*)
         :cljs (js/Date. v*)))

    (instance?
      #?(:clj Date
         :cljs js/Date)
      v)
    v

    (= "now" v)
    #?(:clj (Date.)
       :cljs (js/Date.))

    (string? v)
    #?(:clj (try
              (Date. v)
              (catch IllegalArgumentException _ nil))
       :cljs (js/Date. v))))

(defn safe-str
  [v]
  (when v
    (if (keyword? v)
      (name v)
      (str v))))
