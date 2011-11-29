(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished)

(defn anon- "Takes a seq of char, return a seq of vec of anonymised chars"
  [s] )

(fact "anon-"
      (anon- [:db :o]) => [[:x] [:o]]
      (provided
       (digit-or-blank? :bd) => true
       (digit-or-blank? :o)  => false
       (anon-chunk [:bd])    => [:x]))

(defn anon "Takes a seq of char, returns a seq of anonymised chars"
  [s] (flatten (anon- s)))

(fact
 (anon :in-seq) => [:x1 :x2 :x3]
 (provided
  (anon- :in-seq) => [[:x1 :x2] [:x3]]))
