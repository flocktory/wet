(ns wet.filters
  (:refer-clojure :exclude [first last map remove replace reverse sort])
  (:require [clojure.string :as str]))

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
  [s]
  ;; TODO
  s)

(deffilter append
  [s]
  ;; TODO
  s)

(deffilter capitalize
  [s]
  ;; TODO
  s)

(deffilter ceil
  [s]
  ;; TODO
  s)

(deffilter compact
  [s]
  ;; TODO
  s)

(deffilter date
  [s]
  ;; TODO
  s)

(deffilter default
  [s]
  ;; TODO
  s)

(deffilter divided-by
  [s]
  ;; TODO
  s)

(deffilter downcase
  [s]
  ;; TODO
  s)

(deffilter escape
  [s]
  ;; TODO
  s)

(deffilter escape-once
  [s]
  ;; TODO
  s)

(deffilter first
  [s]
  ;; TODO
  s)

(deffilter floor
  [s]
  ;; TODO
  s)

(deffilter join
  [s]
  ;; TODO
  s)

(deffilter last
  [s]
  ;; TODO
  s)

(deffilter lstrip
  [s]
  ;; TODO
  s)

(deffilter map
  [s]
  ;; TODO
  s)

(deffilter minus
  [s]
  ;; TODO
  s)

(deffilter modulo
  [s]
  ;; TODO
  s)

(deffilter newline-to-br
  "Replaces every newline (\\n) with an HTML line break (<br>)."
  [s]
  (str/replace s "\n" "<br>"))

(deffilter plus
  [s]
  ;; TODO
  s)

(deffilter prepend
  [s]
  ;; TODO
  s)

(deffilter remove
  [s]
  ;; TODO
  s)

(deffilter remove-first
  [s]
  ;; TODO
  s)

(deffilter replace
  "Replaces every occurrence of an argument in a string with the second
   argument."
  [s match replacement]
  (str/replace s match replacement))

(deffilter replace_first
  [s]
  ;; TODO
  s)

(deffilter reverse
  [s]
  ;; TODO
  s)

(deffilter round
  [s]
  ;; TODO
  s)

(deffilter rstrip
  [s]
  ;; TODO
  s)

(deffilter size
  [s]
  ;; TODO
  s)

(deffilter slice
  [s]
  ;; TODO
  s)

(deffilter sort
  [s]
  ;; TODO
  s)

(deffilter sort-natural
  [s]
  ;; TODO
  s)

(deffilter split
  [s]
  ;; TODO
  s)

(deffilter strip
  [s]
  ;; TODO
  s)

(deffilter strip-html
  [s]
  ;; TODO
  s)

(deffilter strip-newlines
  [s]
  ;; TODO
  s)

(deffilter times
  [s]
  ;; TODO
  s)

(deffilter truncate
  [s]
  ;; TODO
  s)

(deffilter truncatewords
  [s]
  ;; TODO
  s)

(deffilter uniq
  [s]
  ;; TODO
  s)

(deffilter upcase
  "Makes each character in a string uppercase."
  [s]
  (str/upper-case s))

(deffilter url-decode
  [s]
  ;; TODO
  s)

(deffilter url-encode
  [s]
  ;; TODO
  s)
