(ns wet.core
  (:require [clojure.walk :as walk]
            [wet.impl.parser :as parser]
            [wet.impl.rendering :as rendering]))

(defn parse
  "Accepts a template string and attempts to parse it, returning
   an intermediate representation. Possible options are:
   - :analyse? — when true, a summary of used filters and lookups will be
     attached to the returned object's metadata.
   On parse error, an ex-info with :type :wet/parse-error will be thrown."
  ([template]
   (parse template nil))
  ([template options]
   (cond->
     (parser/parse-and-transform template)
     (:analyse? options)
     (as-> transformed-template
           (with-meta
             transformed-template
             (parser/analyse transformed-template))))))

(defn render
  "Accepts an intermediate representation produced by wet.core/parse
   and renders it, possibly with the use of the map of following options:
   - :params — a map of arbitrary Clojure data structures as render parameters,
   - :filters — a map of functions of one more arguments."
  ([transformed-template]
   (render transformed-template nil))
  ([transformed-template options]
   (let [context {:params (walk/stringify-keys (:params options))
                  :filters (walk/stringify-keys (:filters options))}]
     (rendering/eval-template transformed-template context))))
