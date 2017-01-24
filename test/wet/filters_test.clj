(ns wet.filters-test
  (:require [clojure.test :refer :all]
            [wet.test-utils :refer [render]]))

(deftest standard-filters-test
  (testing "upcase"
    (is (= "LIQUID" (render "{{ s | upcase }}" {"s" "liquid"})))))

(deftest custom-filter-test
  (let [params {"s" "liquid"}
        opts {:custom-filters {"greet" (partial str "Hello, ")
                               "upcase" (partial str "upcase ")}}]
    (testing "new filter"
      (is (= "Hello, liquid" (render "{{ s | greet }}" params opts))))
    (testing "overriding standard filter"
      (is (= "upcase liquid" (render "{{ s | upcase }}" params opts))))))
