(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished get-action blank? )

(defn anon-proc-
  [c ctx] )

(future-fact "anon-proc-"
      (anon-proc- :c :ctx) => 
      (get-action :c :ctx) => :start-acc-char)



(defn anon- "Takes a seq of char, return a seq of vec of anonymised chars"
  [s] (take-while identity
                  (iterate (fn [[seq- ctx]] 
                             (cond (nil? seq-)   nil
                                   (empty? seq-) [nil         (anon-proc- nil          ctx)]
                                   :else         [(rest seq-) (anon-proc- (first seq-) ctx)]))
                           [s {}])))

(fact "anon-: nominal"
      (anon- [:a :b]) => [[[:a :b] {}]
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

(comment 

  (defrecord Add2
      Calculate
    (do-calc [entity input]) => (+ 2 input))

  (fact
   (do-calc (Mult2. 3)) => 6
   (do-calc (Add2. 3)) => 5))

(defprotocol Calculate
  (do-calc [entity input]))

(defrecord Mult2 [input]
  Calculate
  (do-calc [entity input] (* 2 input)))

(defrecord Add2 [input]
  Calculate
  (do-calc [entity input] (+ 2 input)))

(fact
 (do-calc (Mult2. 0) 3) => 6)

(fact
 (do-calc (Add2. 0) 3) => 5)


