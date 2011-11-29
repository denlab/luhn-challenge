(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished blank? )

(defn anon-proc-
  [ctx])

(future-fact "anon-proc-"
      (anon-proc- {:seq- [:c]}) => {:seq- [] :out [:c]}
      (provided
       (blank? :c) => true))

(defn anon- "Takes a seq of char, return a seq of vec of anonymised chars"
  [s] (take-while identity
                  (iterate (fn [[seq- ctx]] 
                             (cond (seq seq-)    [(rest seq-) (anon-proc- (first seq-) ctx)]
                                   (nil? seq-)   nil
                                   :else         [nil         (anon-proc- nil          ctx)]))
                           [s {}])))

(fact "anon-: EOF"
      (take 4 (anon- [:a])) => [[[:a] {}]
                       [[]   :ctx1]
                       [nil   :ctx2]]
      (provided
       (anon-proc- :a {}) => :ctx1
       (anon-proc- nil :ctx1) => :ctx2))

(fact "anon-: nominal"
      (take 4 (anon- [:a :b])) => [[[:a :b] {}]
                                   [[:b]    :ctx1]
                                   [[]      :ctx2]
                                   [nil      :ctx3]]
      (provided
       (anon-proc- :a  {})    => :ctx1
       (anon-proc- :b  :ctx1) => :ctx2
       (anon-proc- nil :ctx2) => :ctx3))

(defn anon "Takes a seq of char, returns a seq of anonymised chars"
  [s] (flatten (anon- s)))

(fact
 (anon :in-seq) => [:x1 :x2 :x3]
 (provided
  (anon- :in-seq) => [[:x1 :x2] [:x3]]))

(println "--------- END CORE  ----------" (java.util.Date.))
