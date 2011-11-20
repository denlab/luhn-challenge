(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished anon-window )

;; test

(defn digit?
  [c]
  (and (char? c)
       (<= (int \0) (int c) (int \9))))

(fact
  (digit? [0 1 2]) => false
  (digit? \a) => false
  (digit? \0) => true
  (digit? \9) => true)

(defn inc-digit
  [c] (char (inc (int c))))

(fact (inc-digit \0) => \1)

(defn inc-digits "Take a seq of char and inc each of them, non digit are untouched"
  [s] (w/postwalk #(if (digit? %)
                (inc-digit %)
                %) (seq s)))

(defn window-32 [])

(defn anon "Takes a seq of char, return a seq of char anonymised"
  ([s] (anon [] (first s) (next s)))
  ([togive f r] (lazy-seq (cond (seq togive) (cons (first togive)
                                                   (anon (rest togive) f r))
                                (digit? f)   (let [[ff rr] (window-32 (cons f r))]
                                               (anon (anon-window ff) (first rr) (next rr)))
                                :else       (cons f
                                                  (when r (anon [] (first r) (next r))))))))

(fact "trivial, non anonymisation"
  (anon "ab") => (seq "ab"))

(fact ""
  (anon "ab12fg") => (seq "abxxfg")
  (provided
    (digit? \a) => false
    (digit? \b) => false
    (digit? \1) => true
    (window-32 (seq "d")) => [[\1 \2] (seq "fg")]
    (anon-window [\1 \2]) => [\x \x]))

(fact
  (inc-digits "a1b2") => (seq "a2b3"))

(println "--------- END OF CORE  ----------" (java.util.Date.))


