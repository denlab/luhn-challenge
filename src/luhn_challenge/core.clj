(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished split-by-candidate split-by-non-candidate )

;; test

#_(defn digit?
  [c]
  (and (char? c)
       (<= (int \0) (int c) (int \9))))

#_(fact
  (digit? [0 1 2]) => false
  (digit? \a) => false
  (digit? \0) => true
  (digit? \9) => true)

(defn anon "Takes a seq of char, and return a seq of char anonymised"
  ([s] (let [[non-cand cand] (split-by-non-candidate s)]
         (anon (first non-cand) (next non-cand)
               cand             0)))
  ([f nc c cnt] (lazy-seq (cons f
                                (cond (< 4 cnt) nil
                                      (seq nc)  (anon (first nc)     (next nc)  c         (inc cnt))
                                      (seq  c)  (let [[cand non-cand] (split-by-candidate c)]
                                                  (anon (first cand) (next cand) non-cand (inc cnt)))
                                      :else     nil)))))

(fact "anon: non-candidate then candidate, then end"
 (anon "ab12") => [\a \b \X \X]
 (provided
  (split-by-non-candidate "ab12") => ["ab" "12"]
  (split-by-candidate     "12")   => ["XX" nil]))

(fact "anon: non-candidate is empty then candidate, then end"
 (anon "12") => [\X \X]
 (provided
  (split-by-non-candidate "12") => [nil "12"]
  (split-by-candidate     "12") => ["XX" nil]))

(println "--------- END OF CORE  ----------" (java.util.Date.))


