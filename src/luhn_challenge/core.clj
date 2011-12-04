(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished init-state max-cc-size char-type digit? cc-max-size
            other-char?  get-action blank? )

(defn char-type "Given a char returns the type of it: :blank | :other | :digit"
  [c] (case c
        \0 :digit, \1 :digit, \2 :digit, \3 :digit, \4 :digit, \5 :digit, \6 :digit, \7 :digit, \8 :digit, \9 :digit,
        \- :blank, \space :blank,
        :other))

(tabular
 (fact (char-type ?c) => ?expected)
 ?c ?expected
 \0 :digit, \1 :digit, \2 :digit, \3 :digit, \4 :digit, \5 :digit, \6 :digit, \7 :digit, \8 :digit, \9 :digit,
 \- :blank, \space :blank,
 \_ :other, \a :other)

(defprotocol State
  (nxt [this c])
  (out [this]))

(defrecord HandleOther [o seq]
  State
    (nxt [this c]))

(defrecord HandleBlank [dob seq]
    State
    (nxt [this c]))

(defrecord HandleDigit []
  State
    (nxt [this c]))

(defn anon- "Takes a seq of char, return a seq of vec of anonymised chars"
  [s] (take-while #(% 1)
                  (iterate (fn [[[frst & rst] state]] [rst (nxt state frst)])
                           [(rest s) (init-state (first s))])))

(fact "anon-: start"
  (take 10 (anon- [:x :y])) => [[[:y] :state1]
                                [nil  :state2]
                                [nil  :state3]]
  (provided
    (init-state :x)   => :state1
    (nxt :state1 :y)  => :state2
    (nxt :state2 nil) => :state3
    (nxt :state3 nil) => nil))

(future-fact "anon- : edge case: nil seq"
      (take 10 (anon- nil)) => []
      (provided
       (nxt (Idle. nil)) => nil))

(future-fact "anon- : edge case: empty seq"
      (take 10 (anon- [])) => []
      (provided
       (nxt (Idle. [])) => nil))

(defn anon "Takes a seq of char, returns a seq of anonymised chars"
  [s] (flatten (map out (anon- s))))

(fact
 (anon :in-seq) => [:x1 :x2 :x3]
 (provided
  (anon- :in-seq) => [:state1 :state2 :state3] 
  (out   :state1) => [:x1]
  (out   :state2) => []
  (out   :state3) => [:x2 :x3]))

(println "--------- END CORE  ----------" (java.util.Date.))



