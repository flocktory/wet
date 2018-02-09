# Wet 💧

[![Build Status](https://travis-ci.org/superkonduktr/wet.svg?branch=master)](https://travis-ci.org/superkonduktr/wet)

Wet is a pure Clojure implementation of the [Liquid template language](https://shopify.github.io/liquid/)
built on top of [Instaparse](https://github.com/Engelberg/instaparse).

## Installation

#### Leiningen/Boot

```
[superkonduktr/wet "0.1.12"]
```

#### CLI

```clojure
{:deps {superkonduktr/wet {:mvn/version "0.1.12"}}}
```

## Usage

The whole deal:

```clojure
(def template
  (str "{{ season | capitalize }} kept us warm, {{ 'cover' | gerund }} "
       "{{ planet }} in forgetful snow, {{ 'feed' | gerund }} "
       "a {{ size }} life with dried tubers."))
=> #'user/template

(def parsed-template (wet/parse template))
=> #'user/parsed-template

(wet/render parsed-template {:params {:season "winter"
                                      :planet "Earth"
                                      :size "little"}
                             :filters {:gerund (fn [verb] (str verb "ing"))}})
=> "Winter kept us warm, covering Earth in forgetful snow, feeding a little life with dried tubers."
```

Request a rudimentary template analysis from parser prior to rendering:

```clojure
(def parsed-template (wet/parse template {:analyze? true}))
=> #'user/parsed-template

(meta parsed-template)
=> {:lookups #{"season" "planet" "size"},
    :core-filters #{"capitalize"},
    :custom-filters #{"gerund"}}
```

## Thanks

[Aleksey Burlak](https://github.com/leshaburlak)
