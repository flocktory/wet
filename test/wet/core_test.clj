(ns wet.core-test
  (:require [clojure.test :refer :all]
            [wet.parser :as parser]
            [wet.test-utils :refer [render]]))

(deftest parser-test
  (testing "bare template"
    (is (= "Hello world!" (render "Hello world!"))))

  (testing "assignment"
    (are [expected template]
      (= expected (render template {:params {"foo" 42}}))
      "Hello world!" (str "{% assign bar = 'world' %}"
                          "Hello {{ bar }}!")
      "Hello world!" (str "{% capture bar %}"
                          "world"
                          "{% endcapture %}"
                          "Hello {{ bar }}!")
      "41 43" (str "{% decrement foo %}"
                   "{{ foo }} "
                   "{% increment foo %}"
                   "{% increment foo %}"
                   "{{ foo }}")))

  (testing "objects"
    (are [expected template]
      (= expected (render template {:params {"x" "world" "y" 42}}))
      "Hello world!" "Hello {{ x }}!"
      "Hello WORLD!" "Hello {{ x | upcase }}!"
      "The meaning of Liquid is 42." "The meaning of Liquid is {{ y }}.")

    (are [expected template]
      (= expected (render template
                          {:params {:x [:some-data
                                        {:friends [{:name "Monica"}
                                                   {:name "Ross"}]}]
                                    :attr :name
                                    :n 1}}))
      "MONICA" "{{ x.last['friends'].first['name'] | upcase }}"
      "Ro" "{{ x[1]['friends'][n][attr] | remove: 's' }}"))

  (testing "control flow"
    (are [expected template]
      (= expected (render template {:params {"a" 42 "b" false}}))
      "ok" (str "{% if a %}"
                "ok"
                "{% else %}"
                "not ok"
                "{% endif %}")
      "ok" (str "{% if b %}"
                "not ok"
                "{% elsif a == 43 %}"
                "not ok"
                "{% elsif a < 100 %}"
                "ok"
                "{% endif %}")
      "ok" (str "{% unless b %}"
                "ok"
                "{% endunless %}")
      "ok" (str "{% case a %}"
                "{% when 41 %}"
                "not ok"
                "{% when 42 %}"
                "ok"
                "{% endcase %}")))

  (testing "iteration"
    (are [expected template]
      (= expected (render template {:params {"xs" (range 1 6)
                                             "friends" ["Chandler"
                                                        "Joey"
                                                        "Monica"
                                                        "Phoebe"
                                                        "Rachel"
                                                        "Ross"]}}))
      "12345" (str "{% for x in xs %}"
                   "{{ x }}"
                   "{% endfor %}")
      "12345" (str "{% for x in (1..5) %}"
                   "{{ x }}"
                   "{% endfor %}")
      "54321" (str "{% for x in (5..1) %}"
                   "{{ x }}"
                   "{% endfor %}")
      "Chandler Joey Monica " (str "{% for f in friends %}"
                                   "{% if f == \"Phoebe\"%}"
                                   "{% break %}"
                                   "{% endif %}"
                                   "{{ f }} "
                                   "{% endfor %}")
      "Chandler Monica Phoebe Ross " (str "{% for f in friends %}"
                                          "{% if f == \"Joey\" or f == \"Rachel\" %}"
                                          "{% continue %}"
                                          "{% endif %}"
                                          "{{ f }} "
                                          "{% endfor %}")))

  (testing "raw"
    (are [expected template]
      (= expected (render template {:params {:greeting "Hello" :who "World"}}))
      "Hello {{ x }}" (str "{{ greeting }} "
                           "{% raw %}"
                           "{{ x }}"
                           "{% endraw %}")))

  (testing "template analysis"
    (are [template expected]
      (->> template
           (parser/parse-and-transform)
           (parser/analyze)
           ((juxt :core-filters :custom-filters :lookups))
           (= expected))
      "Plain text"
      [#{} #{} #{}]

      "Hello {{ name | capitalize | my_filter }}"
      [#{"capitalize"} #{"my_filter"} #{"name"}]

      (str "{% for i in (x..y) %}"
           "{{ i | plus: 10 }}"
           "{% endfor %}")
      [#{"plus"} #{} #{"i" "x" "y"}])))
