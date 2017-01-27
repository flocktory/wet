(ns wet.filters
  (:refer-clojure :exclude [first last map remove replace reverse sort])
  (:require [clojure
             [string :as str]
             [walk :as walk]]
            [wet.utils :as utils])
  (:import (java.net URLDecoder URLEncoder)))

(defmacro deffilter
  [name & body]
  (let [doc (when (string? (clojure.core/first body)) (clojure.core/first body))
        meta {:doc doc ::liquid-name (-> name (str) (str/replace "-" "_"))}
        [args & body*] (if doc (rest body) body)]
    `(defn ~name ~meta ~args ~@body*)))

(defn find-by-name
  [s]
  (->> (ns-publics 'wet.filters)
       (vals)
       (filter (comp (partial = s) ::liquid-name meta))
       (clojure.core/first)))

(deffilter abs
  "Returns the absolute value of a number."
  [v]
  (some-> v utils/safe-num Math/abs))

(deffilter append
  [v & args]
  "Concatenates two strings and returns the concatenated value."
  (apply (partial str v) args))

(deffilter capitalize
  "Makes the first character of a string capitalized."
  [v]
  (-> v str str/capitalize))

(deffilter ceil
  "Rounds the input up to the nearest whole number. Liquid tries to convert
   the input to a number before the filter is applied."
  [v]
  (some-> v utils/safe-num Math/ceil int))

(deffilter compact
  "Removes any nil values from a collection."
  [v]
  (when (sequential? v) (clojure.core/remove nil? v)))

(deffilter date
  "Converts a timestamp into another date format. The format for the syntax
   is the same as strftime."
  [v fmt]
  (when-let [d (utils/safe-date v)]
    (let [fmt* (str/replace fmt #"%([a-zA-Z])" "%1\\$t$1")]
      (format fmt* d))))

(deffilter default
  "Allows you to specify a fallback in case a value doesn’t exist."
  [v fallback]
  (or (if (or (string? v) (coll? v)) (not-empty v) v) fallback))

(deffilter divided-by
  "Divides a number by the specified number.
   The result is rounded down to the nearest integer (that is, the floor)
   if the divisor is an integer."
  [v divisor]
  (when-let [v* (utils/safe-num v)]
    (if (every? integer? [v* divisor]) (quot v* divisor) (/ v* divisor))))

(deffilter downcase
  "Makes each character in a string lowercase."
  [v]
  (some-> v str str/lower-case))

(def ^:private HTML-ESCAPE
  {\& "&amp;"
   \< "&lt;"
   \> "&gt;"
   \" "&quot;"
   \' "&#39;"})

(deffilter escape
  "Escapes a string by replacing characters with escape sequences
   (so that the string can be used in a URL, for example). It doesn’t change
   strings that don’t have anything to escape."
  [v]
  (some-> v (str/escape HTML-ESCAPE)))

(deffilter escape-once
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

(deffilter first
  "Returns the first item of a collection."
  [v]
  (when (or (string? v) (sequential? v))
    (clojure.core/first v)))

(deffilter floor
  "Rounds a number down to the nearest whole number. Liquid tries to convert
   the input to a number before the filter is applied."
  [v]
  (some-> v utils/safe-num Math/floor int))

(deffilter join
  "Combines the items in an array into a single string using the argument as
   a separator."
  [v separator]
  (when (sequential? v) (str/join separator v)))

(deffilter last
  "Returns the last item of a collection."
  [v]
  (when (or (string? v) (sequential? v))
    (clojure.core/last v)))

(deffilter lstrip
  "Removes all whitespaces (tabs, spaces, and newlines) from the beginning
   of a string. The filter does not affect spaces between words."
  [v]
  (-> v str str/triml))

(deffilter map
  "Creates a collection of values by extracting the values of a named property
   from another object."
  [v key]
  (when (and (sequential? v) (every? map? v))
    (clojure.core/map
      (fn [el] (get (walk/stringify-keys el) (utils/safe-str key)))
      v)))

(deffilter minus
  "Subtracts a number from another number."
  [v n]
  (some-> v utils/safe-num (- n)))

(deffilter modulo
  "Returns the remainder of a division operation."
  [v n]
  (some-> v utils/safe-num (mod n)))

(deffilter newline-to-br
  "Replaces every newline (\\n) with an HTML line break (<br>)."
  [v]
  (str/replace v "\n" "<br>"))

(deffilter plus
  "Adds a number to another number."
  [v & ns]
  (when-let [v* (utils/safe-num v)]
    (apply (partial + v*) ns)))

(deffilter prepend
  "Adds the specified string to the beginning of another string."
  [v s]
  (str s v))

(deffilter remove
  "Removes every occurrence of the specified substring from a string."
  [v s]
  (-> v str (str/replace s "")))

(deffilter remove-first
  "Removes only the first occurrence of the specified substring from a string."
  [v s]
  (-> v str (str/replace-first s "")))

(deffilter replace
  "Replaces every occurrence of an argument in a string with the second
   argument."
  [v match replacement]
  (str/replace v match replacement))

(deffilter replace_first
  "Replaces only the first occurrence of the first argument in a string with
   the second argument."
  [v match replacement]
  (str/replace-first v match replacement))

(deffilter reverse
  "Reverses the order of the items in an array."
  [v]
  (when (sequential? v) (clojure.core/reverse v)))

(deffilter round
  "Rounds an input number to the nearest integer or, if a number is specified
   as an argument, to that number of decimal places."
  ([v] (round v 0))
  ([v p]
    (when-let [v* (utils/safe-num v)]
      (let [formatted (format (str "%." p "f") v*)]
        (cond-> (Float. formatted)
          (zero? p) int)))))

(deffilter rstrip
  "Removes all whitespace (tabs, spaces, and newlines) from the right side
   of a string."
  [v]
  (-> v str str/trimr))

(deffilter size
  "Returns the number of characters in a string or the number of items in
   an array."
  [v]
  (when (or (string? v) (coll? v)) (count v)))

(deffilter slice
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

(deffilter sort
  "Sorts items in an array by a property of an item in the array. The order
   of the sorted array is case-sensitive."
  [v]
  (when (sequential? v) (clojure.core/sort v)))

(deffilter sort-natural
  "Sorts items in an array by a property of an item in the array."
  [v]
  (when (sequential? v)
    (sort-by (comp str/lower-case str) v)))

(deffilter split
  "Divides an input string into an array using the argument as a separator."
  [v separator]
  (str/split (str v) (re-pattern (str separator))))

(deffilter strip
  "Removes all whitespace (tabs, spaces, and newlines) from both the left and
   right side of a string. It does not affect spaces between words."
  [v]
  (-> v str str/trim))

(deffilter strip-html
  "Removes any HTML tags from a string."
  [v]
  (-> v str (str/replace #"<\\?.+?>" "")))

(deffilter strip-newlines
  "Removes any newline characters (line breaks) from a string."
  [v]
  (-> v str (str/replace #"[\r\n]+" "")))

(deffilter times
  "Multiplies a number by another number."
  [v & ns]
  (when-let [v* (utils/safe-num v)]
    (apply (partial * v*) ns)))

(deffilter truncate
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

(deffilter truncatewords
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

(deffilter uniq
  "Removes any duplicate elements in an array."
  [v]
  (->> v seq distinct))

(deffilter upcase
  "Makes each character in a string uppercase."
  [v]
  (some-> v str str/upper-case))

(deffilter url-decode
  "Decodes a string that has been encoded as a URL."
  [v]
  (.. URLDecoder (decode (str v) "utf-8")))

(deffilter url-encode
  "Converts any URL-unsafe characters in a string into percent-encoded
   characters."
  [v]
  (.. URLEncoder (encode (str v) "utf-8")))
