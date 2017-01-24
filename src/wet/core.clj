(ns wet.core
  (:require [clojure.walk :as walk]
            [wet.parser :as parser]))

(defn render
  [template params &
   [{:keys [strict-variables strict-filters]
     :or {strict-variables? true strict-filters? true}
     :as opts}]]
  (let [tree (parser/parse-and-transform template)
        params* {:params (walk/stringify-keys params)}
        [res context] (parser/eval-template tree params* opts)]
    res))
