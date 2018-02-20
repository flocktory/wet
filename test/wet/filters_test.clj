(ns wet.filters-test
  (:require [clojure.test :refer :all]
            [wet.test-utils :refer [render]])
  (:import (java.util Date)))

;; Most of the test examples were taken from the official documentation
;; http://shopify.github.io/liquid

(deftest core-filters-test
  (testing "abs"
    (are [expected template] (= expected (render template))
      "17" "{{ -17 | abs }}"
      "4" "{{ 4 | abs }}"
      "19.86" "{{ \"-19.86\" | abs }}"))

  (testing "append"
    (are [expected template] (= expected (render template))
      "/my/fancy/url.html" "{{ \"/my/fancy/url\" | append: \".html\" }}"
      "website.com/index.html" (str "{% assign filename = \"/index.html\" %}"
                                    "{{ \"website.com\" | append: filename }}")))

  (testing "capitalize"
    (are [expected template] (= expected (render template))
      "Title" "{{ \"title\" | capitalize }}"
      "My great title" "{{ \"my great title\" | capitalize }}"))

  (testing "ceil"
    (are [expected template] (= expected (render template))
      "2" "{{ 1.2 | ceil }}"
      "2" "{{ 2.0 | ceil }}"
      "4" "{{ \"3.5\" | ceil }}"
      "1" "{{ 1 | ceil }}"
      "" "{{ x | ceil }}"))

  (testing "compact"
    (is (= "1 2 3 4 " (render (str "{% assign compacted = xs | compact %}"
                                   "{% for x in compacted %}"
                                   "{{ x }} "
                                   "{% endfor %}")
                              {:params {:xs [1 2 nil 3 nil 4]}}))))

  (testing "date"
    (are [expected template] (= expected (render template {:params {:today (Date. 1485449974810)}}))
      "Thu, Jan 26, 17" "{{ today | date: \"%a, %b %d, %y\" }}"
      "2017" "{{ today | date: \"%Y\" }}"
      "Jan 26, 17" "{{ today | date: \"%b %d, %y\" }}"
      "Jan 26, 17" "{{ \"January 26, 2017\" | date: \"%b %d, %y\" }}"
      "" "{{ \"unknown format\" | date: \"%Y\" }}")
    (is (some? (re-find #"20\d{2}" (render "{{ \"now\" | date: \"%Y\" }}")))))

  (testing "default"
    (are [expected template] (= expected (render template))
      "2.99" "{{ product_price | default: 2.99 }}"
      "4.99" (str "{% assign product_price = 4.99 %}"
                  "{{ product_price | default: 2.99 }}")
      "2.99" (str "{% assign product_price = \"\" %}"
                  "{{ product_price | default: 2.99 }}")))

  (testing "divided_by"
    (are [expected template] (= expected (render template))
      "4" "{{ \"16\" | divided_by: 4 }}"
      "1" "{{ 5 | divided_by: \"5\" }}"
      "2.857142857142857" "{{ 20 | divided_by: 7.0 }}"
      "0" "{{ a | divided_by: \"5\" }}"
      "0" "{{ 5 | divided_by: a }}"))

  (testing "downcase"
    (are [expected template] (= expected (render template))
      "parker moore" "{{ \"Parker Moore\" | downcase }}"
      "apple" "{{ \"APPLE\" | downcase }}"))

  (testing "escape"
    (are [expected template] (= expected (render template))
      "Have you read &#39;James &amp; the Giant Peach&#39;?"
      "{{ \"Have you read 'James & the Giant Peach'?\" | escape }}"

      "Tetsuro Takara" "{{ \"Tetsuro Takara\" | escape }}"))

  (testing "escape_once"
    (are [expected template] (= expected (render template))
      "1 &lt; 2 &amp; 3" "{{ \"1 < 2 & 3\" | escape_once }}"
      "1 &lt; 2 &amp; 3" "{{ \"1 &lt; 2 &amp; 3\" | escape_once }}"))

  (testing "first"
    (are [expected template] (= expected (render template))
      "apples" (str "{% assign my_array = \"apples, oranges, peaches, plums\" | split: \", \" %}"
                    "{{ my_array | first }}")))

  (testing "floor"
    (are [expected template] (= expected (render template))
      "1" "{{ 1.2 | floor }}"
      "2" "{{ 2.0 | floor }}"
      "183" "{{ 183.357 | floor }}"
      "3" "{{ \"3.5\" | floor }}"))

  (testing "join"
    (are [expected template] (= expected (render template))
      "John and Paul and George and Ringo"
      (str "{% assign beatles = \"John, Paul, George, Ringo\" | split: \", \" %}"
           "{{ beatles | join: \" and \" }}")))

  (testing "last"
    (are [expected template] (= expected (render template))
      "plums" (str "{% assign my_array = \"apples, oranges, peaches, plums\" | split: \", \" %}"
                   "{{ my_array | last }}")))

  (testing "lstrip"
    (are [expected template] (= expected (render template))
      "So much room for activities!          "
      "{{ \"So much room for activities!          \" | lstrip }}"))

  (testing "map"
    (is (= "Chandler Joey Monica "
           (render (str "{% assign names = friends | map: \"name\" %}"
                        "{% for name in names %}"
                        "{{ name }} "
                        "{% endfor %}")
                   {:params {:friends [{:name "Chandler"}
                                       {:name "Joey"}
                                       {:name "Monica"}]}}))))

  (testing "minus"
    (are [expected template] (= expected (render template))
      "12" "{{ \"16\" | minus: 4 }}"
      "171.357" "{{ 183.357 | minus: \"12\" }}"
      "3" "{{ 3 | minus: a }}"
      "-3" "{{ a | minus: 3 }}"
      "0" "{{ a | minus: b }}"))

  (testing "modulo"
    (are [expected template] (= expected (render template))
      "1" "{{ 3 | modulo: 2 }}"
      "3.5" "{{ 183.5 | modulo: 12 }}"))

  (testing "newline_to_br"
    (are [expected template] (= expected (render template))
      "<br>Hello<br>there<br>"
      (str "{% capture string_with_newlines %}"
           "\nHello\n"
           "there\n"
           "{% endcapture %}"
           "{{ string_with_newlines | newline_to_br }}")))

  (testing "plus"
    (are [expected template] (= expected (render template))
      "6" "{{ 4 | plus: 2 }}"
      "6" "{{ \"4\" | plus: 2 }}"
      "195.357" "{{ 183.357 | plus: \"12\" }}"))

  (testing "prepend"
    (are [expected template] (= expected (render template))
      "Some fruit: apples, oranges, and bananas"
      "{{ \"apples, oranges, and bananas\" | prepend: \"Some fruit: \" }}"

      "liquidmarkup.com/index.html" (str "{% assign url = \"liquidmarkup.com\" %}"
                                         "{{ \"/index.html\" | prepend: url }}")))

  (testing "remove"
    (are [expected template] (= expected (render template))
      "I sted to see the t through the "
      "{{ \"I strained to see the train through the rain\" | remove: \"rain\" }}"))

  (testing "remove_first"
    (are [expected template] (= expected (render template))
      "I sted to see the train through the rain"
      "{{ \"I strained to see the train through the rain\" | remove_first: \"rain\" }}"))

  (testing "replace"
    (are [expected template] (= expected (render template))
      "Take your protein pills and put your helmet on"
      (str "{{ \"Take my protein pills and put my helmet on\" | replace: \"my\", \"your\" }}")))

  (testing "replace_first"
    (are [expected template] (= expected (render template))
      "Take your protein pills and put my helmet on"
      (str "{{ \"Take my protein pills and put my helmet on\" | replace_first: \"my\", \"your\" }}")))

  (testing "reverse"
    (are [expected template] (= expected (render template))
      "plums, peaches, oranges, apples"
      (str "{% assign my_array = \"apples, oranges, peaches, plums\" | split: \", \" %}"
           "{{ my_array | reverse | join: \", \" }}")))

  (testing "round"
    (are [expected template] (= expected (render template))
      "1" "{{ 1.2 | round }}"
      "3" "{{ 2.7 | round }}"
      "183.36" "{{ 183.357 | round: 2 }}"
      "2" "{{ 2 | round: 1 }}"
      "2" "{{ '2' | round: 1 }}"
      "" "{{ x | round: 1 }}"))

  (testing "rstrip"
    (are [expected template] (= expected (render template))
      "          So much room for activities!"
      "{{ \"          So much room for activities!          \" | rstrip }}"))

  (testing "size"
    (are [expected template] (= expected (render template))
      "28" "{{ \"Ground control to Major Tom.\" | size }}"
      "4" (str "{% assign my_array = \"apples, oranges, peaches, plums\" | split: \", \" %}"
               "{{ my_array | size }}")))

  (testing "slice"
    (are [expected template] (= expected (render template))
      "L" "{{ \"Liquid\" | slice: 0 }}"
      "q" "{{ \"Liquid\" | slice: 2 }}"
      "quid" "{{ \"Liquid\" | slice: 2, 5 }}"
      "ui" "{{ \"Liquid\" | slice: -3, 2 }}"))

  (testing "sort"
    (are [expected template] (= expected (render template))
      "Sally Snake, giraffe, octopus, zebra"
      (str "{% assign my_array = \"zebra, octopus, giraffe, Sally Snake\" | split: \", \" %}"
           "{{ my_array | sort | join: \", \" }}")))

  (testing "sort_natural"
    (are [expected template] (= expected (render template))
      "zebra, octopus, giraffe, Sally Snake"
      (str "{% assign my_array = \"zebra, octopus, giraffe, Sally Snake\" | split: \", \" %}"
           "{{ my_array | sort_natual | join: \", \" }}")))

  (testing "split"
    (are [expected template] (= expected (render template))
      "John Paul George Ringo "
      (str "{% assign beatles = \"John, Paul, George, Ringo\" | split: \", \" %}"
           "{% for member in beatles %}"
           "{{ member }} "
           "{% endfor %}")

      "John Paul George Ringo "
      (str "{% assign beatles = 'John|Paul|George|Ringo' | split: '|' %}"
           "{% for member in beatles %}"
           "{{ member }} "
           "{% endfor %}")))

  (testing "strip"
    (are [expected template] (= expected (render template))
      "So much room for activities!"
      "{{ \"          So much room for activities!          \" | strip }}"))

  (testing "strip_html"
    (are [expected template] (= expected (render template))
      "Have you read Ulysses?"
      "{{ \"Have <em>you</em> read <strong>Ulysses</strong>?\" | strip_html }}"))

  (testing "strip_newlines"
    (are [expected template] (= expected (render template))
      "Hellothere" (str "{% capture string_with_newlines %}"
                        "Hello\n"
                        "there"
                        "{% endcapture %}"
                        "{{ string_with_newlines | strip_newlines }}")))

  (testing "times"
    (are [expected template] (= expected (render template))
      "6" "{{ 3 | times: \"2\" }}"
      "2200.284" "{{ \"183.357\" | times: 12 }}"
      "0" "{{ 3 | times: a }}"))

  (testing "truncate"
    (are [expected template] (= expected (render template))
      "Ground control to..." "{{ \"Ground control to Major Tom.\" | truncate: 17 }}"
      "Ground control, and so on" "{{ \"Ground control to Major Tom.\" | truncate: 14, \", and so on\" }}"
      "Ground control to Ma" "{{ \"Ground control to Major Tom.\" | truncate: 20, \"\" }}"))

  (testing "truncatewords"
    (are [expected template] (= expected (render template))
      "Ground control to..." "{{ \"Ground control to Major Tom.\" | truncatewords: 3 }}"
      "Ground control to--" "{{ \"Ground control to Major Tom.\" | truncatewords: 3, \"--\" }}"
      "Ground control to" "{{ \"Ground control to Major Tom.\" | truncatewords: 3, \"\"}}"))

  (testing "uniq"
    (is (= "ants, bugs, bees"
           (render (str "{% assign my_array = \"ants, bugs, bees, bugs, ants\" | split: \", \" %}"
                           "{{ my_array | uniq | join: \", \" }}")))))

  (testing "upcase"
    (are [expected template] (= expected (render template))
      "PARKER MOORE" "{{ \"Parker Moore\" | upcase }}"
      "APPLE" "{{ \"APPLE\" | upcase }}"))

  (testing "url_decode"
    (are [expected template] (= expected (render template))
      "'Stop!' said Fred" "{{ \"%27Stop%21%27+said+Fred\" | url_decode }}"))

  (testing "url_encode"
    (are [expected template] (= expected (render template))
      "john%40liquid.com" "{{ \"john@liquid.com\" | url_encode }}"
      "Tetsuro+Takara" "{{ \"Tetsuro Takara\" | url_encode }}"))

  (testing "coll_index"
    (are [expected template assigns] (= expected (render template {:params assigns}))
      "2" "{{ a.b }}" {:a {:b 2}}
      "3" "{{ a['b'] }}" {:a {:b 3}}
      "4" "{{ a[1] }}" {:a [nil 4 5]})))

(deftest custom-filter-test
  (let [params {:params {:s "liquid"}
                :filters {:greet (partial str "Hello, ")
                          :upcase (partial str "upcase ")}}]
    (testing "new filter"
      (is (= "Hello, liquid" (render "{{ s | greet }}" params))))

    (testing "overriding a core filter"
      (is (= "upcase liquid" (render "{{ s | upcase }}" params))))))
