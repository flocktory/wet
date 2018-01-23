(ns wet.core
  (:require [clojure.walk :as walk]
            [wet
             [parser :as parser]
             [rendering :as rendering]]))

(defn parse
  ([template]
   (parse template nil))
  ([template opts]
   (cond->
     (parser/parse-and-transform template)
     (:analyze? opts)
     (as-> transformed-template
       (with-meta transformed-template (parser/analyze transformed-template))))))

(defn render
  ([transformed-template] (render transformed-template nil))
  ([transformed-template {:keys [params filters]}]
   (let [context {:params (walk/stringify-keys params)
                  :filters (walk/stringify-keys filters)}]
     (rendering/eval-template transformed-template context))))
