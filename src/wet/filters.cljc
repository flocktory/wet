(ns wet.filters
  (:refer-clojure :exclude [first last map remove replace reverse sort])
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [wet.impl.utils :as utils])
  #?(:clj (:import (java.net URLDecoder URLEncoder)
                   (java.util.regex Pattern))))

(defn abs
  "Returns the absolute value of a number."
  [v]
  (some-> v utils/safe-num Math/abs))

(defn append
  [v & args]
  "Concatenates two strings and returns the concatenated value."
  (apply (partial str v) args))

(defn capitalize
  "Makes the first character of a string capitalized."
  [v]
  (str/capitalize (str v)))

(defn ceil
  "Rounds the input up to the nearest whole number. Liquid tries to convert
   the input to a number before the filter is applied."
  [v]
  (some-> v utils/safe-num Math/ceil int))

(defn compact
  "Removes any nil values from a collection."
  [v]
  (when (sequential? v) (clojure.core/remove nil? v)))

(defn date
  "Converts a timestamp into another date format. The format for the syntax
   is the same as strftime."
  [v fmt]
  (when-let [date (utils/safe-date v)]
    #?(:clj (format (str/replace fmt #"%([a-zA-Z])" "%1\\$t$1") date)
       ;; TODO: js strftime
       :cljs (.toISOString date))))

(defn default
  "Allows you to specify a fallback in case a value doesn’t exist."
  [v fallback]
  (or (if (or (string? v) (coll? v)) (not-empty v) v) fallback))

(defn divided-by
  "Divides a number by the specified number.
   The result is rounded down to the nearest integer (that is, the floor)
   if the divisor is an integer."
  [v divisor]
  (let [v* (utils/safe-num v)
        d* (utils/safe-num divisor)]
    (cond
      (every? integer? [v* d*]) (quot v* d*)
      (every? number? [v* d*]) (/ v* d*)
      :else "0")))

(defn downcase
  "Makes each character in a string lowercase."
  [v]
  (some-> v str str/lower-case))

(def ^:private HTML-ESCAPE
  {\& "&amp;"
   \< "&lt;"
   \> "&gt;"
   \" "&quot;"
   \' "&#39;"})

(defn escape
  "Escapes a string by replacing characters with escape sequences
   (so that the string can be used in a URL, for example). It doesn’t change
   strings that don’t have anything to escape."
  [v]
  (some-> v (str/escape HTML-ESCAPE)))

(defn escape-once
  "Escapes a string without changing existing escaped entities. It doesn’t
   change strings that don’t have anything to escape."
  [v]
  (str/replace
    (str v)
    #"['\"><]|&(?!([a-zA-Z]+|#\d+);)"
    (fn [[match _]]
      (if-let [replacement (get HTML-ESCAPE (clojure.core/first match))]
        replacement
        match))))

(defn first
  "Returns the first item of a collection."
  [v]
  (when (or (string? v) (sequential? v))
    (clojure.core/first v)))

(defn floor
  "Rounds a number down to the nearest whole number. Liquid tries to convert
   the input to a number before the filter is applied."
  [v]
  (some-> v utils/safe-num Math/floor int))

(defn join
  "Combines the items in an array into a single string using the argument as
   a separator."
  [v separator]
  (when (sequential? v) (str/join separator v)))

(defn last
  "Returns the last item of a collection."
  [v]
  (when (or (string? v) (sequential? v))
    (clojure.core/last v)))

(defn lstrip
  "Removes all whitespaces (tabs, spaces, and newlines) from the beginning
   of a string. The filter does not affect spaces between words."
  [v]
  (str/triml (str v)))

(defn map
  "Creates a collection of values by extracting the values of a named property
   from another object."
  [v key]
  (when (and (sequential? v) (every? map? v))
    (mapv
      (fn [el] (get (walk/stringify-keys el) (utils/safe-str key)))
      v)))

(defn minus
  "Subtracts a number from another number."
  [v n]
  (- (utils/safe-num v 0) (utils/safe-num n 0)))

(defn modulo
  "Returns the remainder of a division operation."
  [v n]
  (some-> v utils/safe-num (mod n)))

(defn newline-to-br
  "Replaces every newline (\\n) with an HTML line break (<br>)."
  [v]
  (str/replace (str v) "\n" "<br>"))

(defn plus
  "Adds a number to another number."
  [& args]
  (apply + (clojure.core/map #(utils/safe-num % 0) args)))

(defn prepend
  "Adds the specified string to the beginning of another string."
  [v s]
  (str s v))

(defn remove
  "Removes every occurrence of the specified substring from a string."
  [v s]
  (str/replace (str v) s ""))

(defn remove-first
  "Removes only the first occurrence of the specified substring from a string."
  [v s]
  (str/replace-first (str v) s ""))

(defn replace
  "Replaces every occurrence of an argument in a string with the second
   argument."
  [v match replacement]
  (str/replace v match replacement))

(defn replace-first
  "Replaces only the first occurrence of the first argument in a string with
   the second argument."
  [v match replacement]
  (str/replace-first v match replacement))

(defn reverse
  "Reverses the order of the items in an array."
  [v]
  (when (sequential? v) (clojure.core/reverse v)))

(defn round
  "Rounds an input number to the nearest integer or, if a number is specified
   as an argument, to that number of decimal places."
  ([v] (round v 0))
  ([v precision]
    (when-let [v* (utils/safe-num v)]
      (if (integer? v*)
        v*
        #?(:clj (let [formatted (format (str "%." precision "f") v*)]
                  (cond-> (Float. formatted)
                    (zero? precision) int))
           :cljs (utils/safe-num (.toFixed v* precision)))))))

(defn rstrip
  "Removes all whitespace (tabs, spaces, and newlines) from the right side
   of a string."
  [v]
  (str/trimr (str v)))

(defn size
  "Returns the number of characters in a string or the number of items in
   an array."
  [v]
  (when (or (string? v) (coll? v)) (count v)))

(defn slice
  "Returns a substring of 1 character beginning at the index specified by the
   argument passed in. An optional second argument specifies the length of
   the substring to be returned.
   String indices are numbered starting from 0."
  ([v start] (slice v start 1))
  ([v start length]
    (let [v* (str v)
          str-length (count v*)
          ;; To account for negative indices
          start* (mod (+ str-length start) str-length)
          end (if (<= (+ start* length) str-length) (+ start* length) str-length)]
      (subs v* start* end))))

(defn sort
  "Sorts items in an array by a property of an item in the array. The order
   of the sorted array is case-sensitive."
  [v]
  (when (sequential? v) (clojure.core/sort v)))

(defn sort-natural
  "Sorts items in an array by a property of an item in the array."
  [v]
  (when (sequential? v)
    (sort-by (comp str/lower-case str) v)))

(defn split
  "Divides an input string into an array using the argument as a separator."
  [v separator]
  #?(:clj (str/split (str v) (re-pattern (Pattern/quote (str separator))))
     :cljs (js->clj (.split (str v) separator))))

(defn strip
  "Removes all whitespace (tabs, spaces, and newlines) from both the left and
   right side of a string. It does not affect spaces between words."
  [v]
  (str/trim (str v)))

(defn strip-html
  "Removes any HTML tags from a string."
  [v]
  (str/replace (str v) #"<\\?.+?>" ""))

(defn strip-newlines
  "Removes any newline characters (line breaks) from a string."
  [v]
  (str/replace (str v) #"[\r\n]+" ""))

(defn times
  "Multiplies a number by another number."
  [& args]
  (apply * (clojure.core/map #(utils/safe-num % 0) args)))

(defn truncate
  "Shortens a string down to the number of characters passed as a parameter.
   If the number of characters specified is less than the length of the string,
   an ellipsis (…) is appended to the string and is included in the character
   count."
  ([v n] (truncate v n "..."))
  ([v n ellipsis]
    (let [v* (str v)]
      (if (>= n (count v*))
        v*
        (-> v* (subs 0 n) (str ellipsis))))))

(defn truncatewords
  "Shortens a string down to the number of words passed as the argument. If
   the specified number of words is less than the number of words in
   the string, an ellipsis (…) is appended to the string."
  ([v n] (truncatewords v n "..."))
  ([v n ellipsis]
    (let [words (-> v str (str/split #"\s+"))
          truncated (cond->> words
                      (< n (count words)) (take n)
                      :finally (str/join " "))]
      (str truncated ellipsis))))

(defn uniq
  "Removes any duplicate elements in an array."
  [v]
  (distinct (seq v)))

(defn upcase
  "Makes each character in a string uppercase."
  [v]
  (some-> v str str/upper-case))

(defn url-decode
  "Decodes a string that has been encoded as a URL."
  [v]
  #?(:clj (.. URLDecoder (decode (str v) "utf-8"))
     :cljs (js/decodeURI (str v))))

(defn url-encode
  "Converts any URL-unsafe characters in a string into percent-encoded
   characters."
  [v]
  #?(:clj (.. URLEncoder (encode (str v) "utf-8"))
     :cljs (js/encodeURI (str v))))

(def CORE-FILTERS
  {"abs" abs
   "append" append
   "capitalize" capitalize
   "ceil" ceil
   "compact" compact
   "date" date
   "default" default
   "divided_by" divided-by
   "downcase" downcase
   "escape" escape
   "escape_once" escape-once
   "first" first
   "floor" floor
   "join" join
   "last" last
   "lstrip" lstrip
   "map" map
   "minus" minus
   "modulo" modulo
   "newline_to_br" newline-to-br
   "plus" plus
   "prepend" prepend
   "remove" remove
   "remove_first" remove-first
   "replace" replace
   "replace_first" replace-first
   "reverse" reverse
   "round" round
   "rstrip" rstrip
   "size" size
   "slice" slice
   "sort" sort
   "sort_natural" sort-natural
   "split" split
   "strip" strip
   "strip_html" strip-html
   "strip_newlines" strip-newlines
   "times" times
   "truncate" truncate
   "truncatewords" truncatewords
   "uniq" uniq
   "upcase" upcase
   "url_decode" url-decode
   "url_encode" url-encode})
