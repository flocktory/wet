(ns wet.test-utils
  (:require [wet.core :as core]
            [wet.parser :as parser]))

(defn render
  ([s] (render s nil nil))
  ([s params] (render s params nil))
  ([s params opts] (core/render s params opts)))
