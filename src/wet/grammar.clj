(ns wet.grammar
  (:require [instaparse.core :as insta]))

(def parse
  (insta/parser "
    (* COMMON RULES *)

    template = (b* lookup* tag-expression*)*

    b = #'(?s)((?!\\{\\{|\\{\\%).)*'

    s = #'[\\s\\n\\r]*'
    <ltag> = <'{%'> <s>
    <rtag> = <s> <'%}'>
    <token> = #'[a-zA-Z-_]+[a-zA-Z0-9-_]*'

    <tag-expression> = assign
                     | capture
                     | increment
                     | decrement
                     | if
                     | unless
                     | case
                     | for

    <body> = template


    (* OBJECTS & TYPES *)

    number = #'[0-9]+\\.?[0-9]*'
    string = <'\"'> #'((?!\\\").)*' <'\"'> | <'\\''> #'((?!\\').)*' <'\\''>
    bool = 'true' | 'false'
    lookup-token = #'[a-zA-Z\\[_-]+[a-zA-Z0-9\\'\\\"\\[\\]\\._-]*'
    <object> = bool / number / string / lookup-token

    <lookup> = <'{{'> <s> lookup-expr? <'}}'>
    lookup-expr = (object <s> filter* <s>)
    filter = <'|'> <s> (token | token <s> <':'> <s> args) <s>
    args = object (<s> <','> <s> object)*


    (* PREDICATES *)

    operator = '==' | '!=' | '>' | '<' | '>=' | '<=' | 'contains'
    or = 'or'
    and = 'and'
    operand = <s> object <s>
    predicate = operand
              | <s> <'('> predicate <')'> <s>
              | (predicate or predicate / predicate and predicate)
              | predicate operator predicate


    (* VARIABLES *)

    assign = ltag <'assign '> <s> token <s> <'='> <s> token <s> rtag
    increment = ltag <'increment '> <s> token rtag
    decrement = ltag <'decrement '> <s> token rtag
    capture = ltag <'capture '> <s> token rtag
              body
              ltag <'endcapture'> rtag


    (* CONTROL FLOW *)

    elsif = ltag <'elsif '> <s> predicate rtag body
    else = ltag <'else '> <s> rtag body

    if = ltag <'if '> <s> predicate rtag
         body
         elsif*
         else?
         ltag <'endif'> rtag

    unless = ltag <'unless '> <s> predicate rtag
             body
             else?
             ltag <'endunless'> rtag

    <case-expr> = object
    when = ltag <'when '> <s> object rtag body
    case = ltag <'case '> <s> case-expr rtag <s>
           when*
           else?
           ltag <'endcase'> rtag


    (* ITERATION *)

    break = ltag <'break'> rtag

    for = ltag <'for '> token <s> <'in '> lookup-expr rtag
          body
          ltag <'endfor'> rtag
    "
    :no-slurp true
    :start :template))
