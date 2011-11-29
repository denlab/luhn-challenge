(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished digit-or-blank? anon-chunk blank? )



;; --------------------------------------------------------------------------------

(defn lazy-concat-helper-
  [s] (iterate (fn [{:keys [curr-char curr-vec curr-seq cnt]}]
                 (cond (seq curr-vec) {:curr-char (first curr-vec)
                                       :curr-vec  (rest curr-vec)
                                       :curr-seq       curr-seq}
                       (seq curr-seq) {:curr-char (first (first curr-seq))
                                       :curr-vec  (rest (first curr-seq))
                                       :curr-seq  (rest curr-seq)}
                       :else          {}))
               {:curr-char (first (first s))
                :curr-vec  (rest (first s))
                :curr-seq  (rest s)}))

(let [r (lazy-concat-helper- (iterate (fn [[a b]] [(inc a) (inc b)]) [1 2]))]
  (fact "lazy-concat"
        (:curr-char (first r))        => 1
        (:curr-vec (first r))         => [2]
        (first (:curr-seq (first r))) => [2 3]))

;.;. The highest reward for a man's toil is not what he gets for it but
;.;. what he becomes by it. -- Ruskin
(fact
  (take 6 (lazy-concat-helper- [[1 2] [3 4]])) => [{:curr-char 1
                                                    :curr-vec  [2]
                                                    :curr-seq  [[3 4]]},
                                                   {:curr-char 2
                                                    :curr-vec  []
                                                    :curr-seq  [[3 4]]},
                                                   {:curr-char 3
                                                    :curr-vec  [4]
                                                    :curr-seq  []},
                                                   {:curr-char 4
                                                    :curr-vec  []
                                                    :curr-seq  []},
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

;; 1gig : 28mn (on zeus)
(comment
  (time (first (drop-while #(not= :b %) (lazy-concat (map #(if (< % 1000000000) [:a] [:b]) (range))))))
  "Elapsed time: 1695599.3079 msecs"
  :b)

;; 100 meg: 74sec (on zeus)
(comment
  (time (first (drop-while #(not= :b %) (lazy-concat (map #(if (< % 100000000) [:a] [:b]) (range))))))
  "Elapsed time: 1695599.3079 msecs"
  :b)

;; 10meg: 45sec (on zeus)
(comment
  (time (first (drop-while #(not= :b %) (lazy-concat (map #(if (< % 10000000) [:a] [:b]) (range))))))
  "Elapsed time: 1695599.3079 msecs"
  :b)

;; 1meg: 3sec  (on macbook air)
(comment
  (time (first (drop-while #(not= :b %) (lazy-concat (map #(if (< % 1000000) [:a] [:b]) (range))))))
  "Elapsed time: 2933.64925 msecs"
  :b)

;; 10meg: 32sec  (on macbook air)
(comment
  (time (first (drop-while #(not= :b %) (lazy-concat (map #(if (< % 10000000) [:a] [:b]) (range))))))
  "Elapsed time: 32566.30825 msecs"
  :b)

;; 10meg: 29sec with flatten (on macbook air)
(comment
  (time (first (drop-while #(not= :b %) (flatten (map #(if (< % 10000000) [:a] [:b]) (range))))))
  "Elapsed time: 29044.32825 msecs"
  :b)

;; 3 types of chars:
;;   - blank? (\space \-)
;;   - digits
;;   - others

;; partitions by digits or whitespace:

;; ...digitorblank... ...otherchars...

;; other

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
