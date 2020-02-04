# wet 💧

wet is a pure Clojure/ClojureScript port of the [Liquid template language](https://shopify.github.io/liquid/)
built on top of [Instaparse](https://github.com/Engelberg/instaparse).

## Installation

#### Leiningen/Boot

```
[amperity/wet "0.2.3"]
```

#### CLI

```clojure
{:deps {amperity/wet {:mvn/version "0.2.3"}}}
```

## Usage

In the vein of the [original library](https://github.com/Shopify/liquid),
wet provides a minimalistic interface comprising `parse` and `render` functions.
Calling wet from Clojure and ClojureScript is completely identical.

[`wet.core`](https://github.com/com.flocktory/wet/blob/master/src/wet/core.cljc)
is the only namespace you are going to need.

```clojure
(:require [wet.core :as wet])
```

#### An example

Prepare the template:

```clojure
(def template
  (str "{{ season | capitalize }} kept us warm, {{ 'cover' | gerund }} "
       "{{ planets.habitable[0] }} in forgetful snow, {{ 'feed' | gerund }} "
       "a {{ size }} life with dried tubers."))

;; An intermediate representation of the parsed template
(def parsed-template
  (wet/parse template))
```

It may also be convenient to request a rudimentary template analysis
from the parser prior to rendering with the `:analyse?` option.
That way, a basic summary of the template's contents can be collected
from the parsed template's metadata.

```clojure
(def parsed-template
  (wet/parse template {:analyse? true}))
```

```clojure
(meta parsed-template)
=> {:lookups #{"season" "planet" "size"},
    :core-filters #{"capitalize"},
    :custom-filters #{"gerund"}}
```

Finally, obtain the rendered result:

```clojure
(wet/render
  parsed-template
  ;; :params may contain any Clojure data structures
  {:params {:season "winter"
            :planets {"habitable" ["Earth" "Mars"]}
            :size "little"}
   ;; Any Clojure function of arity one or more may act as a Liquid filter
   ;; when passed in the :filters map. The first argument is the object
   ;; being transformed, and the rest is passed to the filter as parameters.
   ;; For detailed examples on filters please consult wet.filters-test.
   :filters {:gerund (fn [verb] (str verb "ing"))}})
```

```clojure 
=> "Winter kept us warm, covering Earth in forgetful snow, feeding a little life with dried tubers."
```

The complete list of core Liquid filters can be found in
[`wet.filters`](https://github.com/com.flocktory/wet/blob/master/src/wet/filters.cljc).

## Thanks

[Aleksey Burlak](https://github.com/leshaburlak)
