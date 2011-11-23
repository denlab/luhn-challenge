(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished anon-map   luhn?)

(defn digit?
  [c]
  (and (char? c)
       (<= (int \0) (int c) (int \9))))

(fact
  (digit? [0 1 2]) => false
  (digit? \a) => false
  (digit? \0) => true
  (digit? \9) => true)

(defn anon-chunk-then-split "given a seq starting with a candidate and a reminder : a seq of pair [digit, toAnon?], returns: a finite seq of anonymised data, a map {:rem [] :to-anon []} reminder of digits, a lazy seq of the rest]"
  [s])

(defn anon-char
  [] \X)

(defn min-size-cb
  [] 14)

(defn max-size-cb
  [] 16)

(defn as-combi-map "Takes a vec and returns all combination of min max"
  [min max s]
  (mapcat (fn [size] (map (fn [start] (subvec s start (+ start size)))
                         (range (- (inc (count s)) size))))
          (range min (inc max))))

(fact "as-combi-map"   ;; 0  1  2  3  4  5       
      (as-combi-map 2 4 [:a :b :c :d :e :f]) => [{0 :a 1 :b} {1 :b 2 :c} {2 :c 3 :d} {3 :d 4 :e} {4 :e 5 :f}
                                                 {0 :a 1 :b 2 :c} {1 :b 2 :c 3 :d} {2 :c 3 :d 4 :e} {3 :d 4 :e 5 :f}
                                                 {0 :a 1 :b 2 :c 3 :d} {1 :b 2 :c 3 :d 4 :e} {2 :c 3 :d 4 :e 5 :f}])

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

(future-fact "anon-chunk: empty seq"
      (anon-chunk []) => [])

(future-fact "anon-chunk: not a credit card number"
             (anon-chunk [:a :b :c :d :e :f]) => [:a :b :c :d :e :f]
             (provided
              (min-size-cb) => 2
              (max-size-cb) => 4
              (luhn? [:a :b]) => false
              (luhn? [:b :c]) => false
              (luhn? [:c :d]) => false
              (luhn? [:d :e]) => false
              (luhn? [:e :f]) => false
              (luhn? [:a :b :c]) => false
              (luhn? [:b :c :d]) => false
              (luhn? [:c :d :e]) => false
              (luhn? [:a :b :c :d]) => false
              (luhn? [:b :c :d :e]) => false
              (luhn? [:c :d :e :f]) => false))

(defn anon "Takes a seq of char, return a seq of char anonymised"
  ([s] (anon [] (first s) (next s) 0))
  ([f to-eat acc is cnt]
     (lazy-seq
      (cons f
            (cond (< 4 cnt)    nil
                  (seq to-eat) (anon (first to-eat) (next to-eat)    acc is        (inc cnt))
                  (seq is)     (if (digit? (first is)) (let [[anonized azz rem-seq] (anon-chunk-then-split is acc)]
                                                         (anon (first anonized) (next anonized) azz rem-seq (inc cnt)))
                                   (anon (first is) (anon-chunk acc) [] (next is) (inc cnt)))
                  (seq acc)    (let [[ff & rr] (anon-chunk acc)] (anon ff rr [] is (inc cnt)))
                  :else        nil)))))

(fact "anon: end, but acc remaining"
      (anon :f [] [:ac1 :ac2] [] 0)      => [:f :anon1 :anon2]
      (provided
       (anon-chunk [:ac1 :ac2]) => [:anon1 :anon2]))

(fact "anon: consumation in progress, then eof and acc empty"
      (anon \a [\b] [] [] 0) => [\a \b])

(fact "anon: no consumation in progress, then one more non-digit char in is"
      (anon \a [] [] [\b] 0 ) => [\a \b]
      (provided
       (digit? \b)     => false
       (anon-chunk []) => []))

(fact "anon: test call to anon-chunk-then-split and transmit"
      (anon :f [] [:ac1 :ac2] [:is1 :is2] 0)      => [:f :anon1 :anon2]
      (provided
       (digit? :is1)                             => true
       (anon-chunk-then-split [:is1 :is2] [:ac1 :ac2]) => [[:anon1 :anon2] [] []]))


(println "--------- END OF CORE  ----------" (java.util.Date.))


