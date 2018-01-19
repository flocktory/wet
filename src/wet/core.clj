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
  [transformed-template
   {:keys [params filters return-context?]
    :or {return-context? false}}]
  (let [context {:params (walk/stringify-keys params)
                 :filters (walk/stringify-keys filters)}
        [result context*] (rendering/eval-template transformed-template context)]
    (if return-context?
      [result (select-keys context* [:undefined-variables :undefined-filters])]
      result)))
