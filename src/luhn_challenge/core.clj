(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished)


;; --------------------------------------------------------------------------------

(defn lazy-concat-helper-
  [s] (iterate (fn [{:keys [curr-char curr-vec curr-seq cnt]}]
                 (cond  (> cnt 50)    (/ 0 0)
                        (seq curr-vec) {:curr-char (first curr-vec)
                                        :curr-vec  (rest curr-vec)
                                        :curr-seq  curr-seq
                                        :cnt       (inc cnt)}
                        (seq curr-seq) {:curr-char (first (first curr-seq))
                                        :curr-vec  (rest (first curr-seq))
                                        :curr-seq  (rest curr-seq)
                                        :cnt       (inc cnt)}
                        :else          {}))
               {:curr-char (first (first s))
                :curr-vec  (rest (first s))
                :curr-seq  (rest s)
                :cnt       0}))

(let [r (lazy-concat-helper- (iterate (fn [[a b]] [(inc a) (inc b)]) [1 2]))]
  (fact "lazy-concat"
        (:curr-char (first r))        => 1
        (:curr-vec (first r))         => [2]
        (first (:curr-seq (first r))) => [2 3]))

;.;. The highest reward for a man's toil is not what he gets for it but
;.;. what he becomes by it. -- Ruskin
(fact
  (take 5 (lazy-concat-helper- [[1 2] [3 4]])) => '({:curr-char 1
                                                     :curr-vec  [2]
                                                     :curr-seq  [[3 4]]
                                                     :cnt 0},
                                                    {:curr-char 2
                                                     :curr-vec  []
                                                     :curr-seq  [[3 4]]
                                                     :cnt 1},
                                                    {:curr-char 3
                                                     :curr-vec  [4]
                                                     :curr-seq  []
                                                     :cnt 2},
                                                    {:curr-char 4
                                                     :curr-vec  []
                                                     :curr-seq  []
                                                     :cnt 3},
                                                    {}))

(defn lazy-concat
  [s] (map :curr-char
           (take-while seq (lazy-concat-helper- s))))

(fact
 (lazy-concat :seq) => [:a :b]
 (provided
  (lazy-concat-helper- :seq) => [{:curr-char :a}, {:curr-char :b}, {}, {}]))

(println "--------- END OF CORE  ----------" (java.util.Date.))
