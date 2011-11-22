(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished anon-chunk split-by-candidate split-by-non-candidate )

(defn digit?
  [c]
  (and (char? c)
       (<= (int \0) (int c) (int \9))))

(fact
  (digit? [0 1 2]) => false
  (digit? \a) => false
  (digit? \0) => true
  (digit? \9) => true)

(defn anon-chunk-then-split "given a seq starting with a candidate and a reminder, returns: [a finite seq of anonymised data, a reminder of digits, a lazy seq of the rest]"
  [s])

(defn anon "Takes a seq of char, return a seq of char anonymised"
  ([s] (anon [] (first s) (next s) 0))
  ([f to-eat acc is cnt] (lazy-seq (cons f
                                         (do #_(println "f=" f "to-eat=" to-eat "acc=" acc "is=" is)
                                             (cond (< 4 cnt)    nil
                                                   (seq to-eat) (anon (first to-eat) (next to-eat)    acc is        (inc cnt))
                                                   (seq is)     (if (digit? (first is))
                                                                  (let [[anonized azz rem-seq] (anon-chunk-then-split is acc)]
                                                                    (anon (first anonized) (next anonized) azz rem-seq (inc cnt)))
                                                                  (anon (first is) (anon-chunk acc) [] (next is) (inc cnt)))
                                                   (seq acc)    :shouldnt-be-there-yet
                                                   :else        nil))))))

(fact "anon: consumation in progress, then eof and acc empty"
      (anon \a [\b] [] [] 0) => [\a \b])

(fact "anon: no consumation in progress, then one more non-digit char in is"
      (anon \a [] [] [\b] 0 ) => [\a \b]
      (provided
       (digit? \b)     => false
       (anon-chunk []) => []))

(fact "anon: test call to anon-chunk-then-split and transmit"
      (anon \Y [] [\1 \2] [\3 \4] 0)      => [\Y \1 \X \3 \X]
      (provided
       (digit? \3)                             => true
       (anon-chunk-then-split [\3 \4] [\1 \2]) => [[\1 \X \3 \X] [] []]))

(fact "anon: test call to anon-chunk-then-split and transmit"
      (anon :f [] [:ac1 :ac2] [:is1 :is2] 0)      => [:f :anon1 :anon2]
      (provided
       (digit? :is1)                             => true
       (anon-chunk-then-split [:is1 :is2] [:ac1 :ac2]) => [[:anon1 :anon2] [] []]))


(println "--------- END OF CORE  ----------" (java.util.Date.))


