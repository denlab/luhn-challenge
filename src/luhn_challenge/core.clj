(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished max-size-cb anon-window anon-ch )


(fact "")

(defn digit?
  [c]
  (and (char? c)
       (<= (int \0) (int c) (int \9))))

(fact
  (digit? [0 1 2]) => false
  (digit? \a) => false
  (digit? \0) => true
  (digit? \9) => true)

(defn read-window "Given a seq, and an acc, returns [anonymised, rest of seq]"
  [s acc] )

(fact "read-window"
      (read-window [:s1 :s2 :s3 :s4] [:pair1 :pair2]) => [[:pair1 :pair2 [:s1 nil] [:s2 nil] [:s3 nil]]
                                                          [:s4]]
      (provided
       (max-size-cb) => 2
       (digit? :s1)  => true
       (digit? :s2)  => false
       (digit? :s3)  => true))



(defn anon-chunk-then-split "given a seq starting with a candidate and a reminder : a seq of pair [digit, toAnon?], returns: a finite seq of anonymised data, a map {:rem [] :to-anon []} reminder of digits, a lazy seq of the rest]"
  [cand-seq remind] (let [[wind s-rest]  (read-window cand-seq remind)
                          [wind-anon acc] (anon-window wind)]
                      [wind-anon acc s-rest]))

(fact "anon-chunk-then-split"
      (anon-chunk-then-split :is :acc) => [:anon :azz :rest]
      (provided
       (read-window :is :acc) => [:window-32-digits :rest]
       (anon-window :window-32-digits) => [:anon :azz]))

(defn anon-char
  [] \X)

(defn min-size-cb
  [] 14)

(defn max-size-cb
  [] 16)

(defn vec-to-map
  [v] (zipmap (range (count v))
              v))

(fact "vec-to-map"
      (vec-to-map [:a :b]) => {0 :a 1 :b})

(defn as-combi-map "Takes a vec and returns all combination of min max"
  [min max s]
  (let [s-size (inc (count s))]
    (mapcat (fn [size] (map (fn [start] (vec-to-map (subvec s start (+ start size))))
                           (range (- s-size size))))
            (range min (inc max)))))

(fact "as-combi-map"
      (as-combi-map 1 2 [:a :b :c]) => [:m1 :m2 :m3 :m4 :m5]
      (provided
       (vec-to-map [:a])    => :m1
       (vec-to-map [:b])    => :m2
       (vec-to-map [:c])    => :m3
       (vec-to-map [:a :b]) => :m4
       (vec-to-map [:b :c]) => :m5))

(fact "as-combi-map: edge case empty seq"
      (as-combi-map 1 2 []) => [])

(defn char2int "Returns the digit represented by this char"
  [c] (case c
        \0 0 \5 5
        \1 1 \6 6
        \2 2 \7 7
        \3 3 \8 8
        \4 4 \9 9))

(tabular
 (fact (char2int ?c) => ?expected) 
 ?c ?expected
 \0 0 \5 5
 \1 1 \6 6
 \2 2 \7 7
 \3 3 \8 8
 \4 4 \9 9)


(defn charx2 "Takes a char representing a digit, multiply this digit by 2, and returns all the digits of this number"
  [c] (case c
        \0 [0] \5 [1 0]
        \1 [2] \6 [1 2]
        \2 [4] \7 [1 4]
        \3 [6] \8 [1 6]
        \4 [8] \9 [1 8]))

(tabular
 (fact (charx2 ?c) => ?expected) 
 ?c ?expected
 \0 [0]        \5 [1 0]
 \1 [2]        \6 [1 2]
 \2 [4]        \7 [1 4]
 \3 [6]        \8 [1 6]
 \4 [8]        \9 [1 8])

(defn luhn?
  [s] (zero? (rem
              (reduce + (mapcat (fn [[odd even]] (cons (char2int odd) (charx2 even)))
                                (partition-all 2 (reverse s))))
              10)))

(fact "luhn?"
      (luhn? [\5 \6 \7 \8]) => true)

(fact "luhn?"
      (luhn? [\6 \7 \8 \9]) => false)

(defn anon-map "Takes a map (which represent a vector of value) and return a seq of indice of vals to anonymise"
  [m] (if (luhn? (vals m))
        (keys m)
        []))

(fact "anon-map: luhn? test passes"
      (anon-map {0 :a 1 :b} ) => [0 1]
      (provided (luhn? [:a :b]) => true))

(fact "anon-map: luhn? test don't passes"
      (anon-map {0 :a 1 :b} ) => []
      (provided (luhn? [:a :b]) => false))

(defn anon-chunk "Given a seq of pair [digit, to-anon?] returns the anonymised seq"
  [s] (let [ac (anon-char)]
        (reduce (fn [r i] #_(println "r=" r "i=" i "ac=" ac) (assoc r i ac))
                s
                (reduce concat
                        (map anon-map
                             (as-combi-map (min-size-cb) (max-size-cb) s))))))

(fact "anon-chunk"
      (anon-chunk [:a :b :c]) => [:X :X :c]
      (provided
       (min-size-cb) => :min
       (max-size-cb) => :max
       (anon-char) => :X
       (as-combi-map :min :max [:a :b :c]) => [:map1 :map2]
       (anon-map :map1) => [0 1]
       (anon-map :map2) => [0]))

(fact "anon-chunk: empty seq"
      (anon-chunk []) => [])

(defn anon-old "Takes a seq of char, return a seq of char anonymised"
  ([s] (anon-old [] (first s) (next s) 0))
  ([f to-eat acc is cnt]
     (lazy-seq
      (cons f
            (cond (< 4 cnt)    nil
                  (seq to-eat) (anon-old (first to-eat) (next to-eat)    acc is        (inc cnt))
                  (seq is)     (if (digit? (first is)) (let [[anonized azz rem-seq] (anon-chunk-then-split is acc)]
                                                         (anon-old (first anonized) (next anonized) azz rem-seq (inc cnt)))
                                   (anon-old (first is) (anon-chunk acc) [] (next is) (inc cnt)))
                  (seq acc)    (let [[ff & rr] (anon-chunk acc)] (anon-old ff rr [] is (inc cnt)))
                  :else        nil)))))

(fact "anon: end, but acc remaining"
      (anon-old :f [] [:ac1 :ac2] [] 0)      => [:f :anon1 :anon2]
      (provided
       (anon-chunk [:ac1 :ac2]) => [:anon1 :anon2]))

(fact "anon: consumation in progress, then eof and acc empty"
      (anon-old \a [\b] [] [] 0) => [\a \b])

(fact "anon: no consumation in progress, then one more non-digit char in is"
      (anon-old \a [] [] [\b] 0 ) => [\a \b]
      (provided
       (digit? \b)     => false
       (anon-chunk []) => []))

(fact "anon: test call to anon-chunk-then-split and transmit"
      (anon-old :f [] [:ac1 :ac2] [:is1 :is2] 0)      => [:f :anon1 :anon2]
      (provided
       (digit? :is1)                             => true
       (anon-chunk-then-split [:is1 :is2] [:ac1 :ac2]) => [[:anon1 :anon2] [] []]))

(println "--------- END OF CORE  ----------" (java.util.Date.))


