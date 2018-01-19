(ns wet.test-utils
  (:require [wet.core :as core]))

(defn render
  ([s] (render s nil))
  ([s opts] (-> (core/parse s) (core/render opts))))
