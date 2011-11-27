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
                 (cond (seq curr-vec) {:curr-char (first curr-vec)
                                       :curr-vec  (next curr-vec)
                                       :curr-seq       curr-seq}
                       (seq curr-seq) {:curr-char (first (first curr-seq))
                                       :curr-vec  (next (first curr-seq))
                                       :curr-seq  (next curr-seq)}
                       :else          {}))
               {:curr-char (first (first s))
                :curr-vec  (next (first s))
                :curr-seq  (next s)}))

(let [r (lazy-concat-helper- (iterate (fn [[a b]] [(inc a) (inc b)]) [1 2]))]
  (fact "lazy-concat"
        (:curr-char (first r))        => 1
        (:curr-vec (first r))         => [2]
        (first (:curr-seq (first r))) => [2 3]))

(fact
  (take 6 (lazy-concat-helper- [[1 2] [3 4]])) => [{:curr-char 1
                                                    :curr-vec  [2]
                                                    :curr-seq  [[3 4]]},
                                                   {:curr-char 2
                                                    :curr-vec  nil
                                                    :curr-seq  [[3 4]]},
                                                   {:curr-char 3
                                                    :curr-vec  [4]
                                                    :curr-seq  nil},
                                                   {:curr-char 4
                                                    :curr-vec  nil
                                                    :curr-seq  nil},
                                                   {}, {}])
(defn lazy-concat
  [s] (map :curr-char
           (take-while seq (lazy-concat-helper- s))))

(fact
 (lazy-concat :seq) => [:a :b]
 (provided
  (lazy-concat-helper- :seq) => [{:curr-char :a}, {:curr-char :b}, {}, {}]))

(println "--------- END OF CORE  ----------" (java.util.Date.))

(comment
  (def ss (lazy-concat (map #(if (< % 1000000000) [:a] [:b]) (range))))
  (time (first (drop-while #(not= :b %) (lazy-concat (map #(if (< % 1000000000) [:a] [:b]) (range)))))))

;; 1gig : 28mn
(comment
  (time (first (drop-while #(not= :b %) (lazy-concat (map #(if (< % 1000000000) [:a] [:b]) (range))))))
  "Elapsed time: 1695599.3079 msecs"
  :b)

;; 100 meg: 74sec
(comment
  (time (first (drop-while #(not= :b %) (lazy-concat (map #(if (< % 100000000) [:a] [:b]) (range))))))
  "Elapsed time: 1695599.3079 msecs"
  :b)

;; 10meg: 45sec
(comment
  (time (first (drop-while #(not= :b %) (lazy-concat (map #(if (< % 10000000) [:a] [:b]) (range))))))
  "Elapsed time: 1695599.3079 msecs"
  :b)
