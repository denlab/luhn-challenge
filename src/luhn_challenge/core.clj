(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:use     [clojure.walk   :only [macroexpand-all]])
  (:require [clojure.set                       :as set])
  (:import (java.util Date)))

(unfinished anon-digit take-until-not-digit char-candidate?
            anon-candidate)

(defn take-until-digit "Takes a seq of char, and return a vec: [char while not digit, rest]"
  [s])

(defn seq-of-candidate-or-not
  "Take a string and return a seq of string which are candidate, non-candidate, candidate, non-candidate, ..."
  [s])

(defn luhn-check [s]
  "Check if the s sequence is a card number"
  false)

(defn anon "Takes a seq of char, and return an anonymized seq of char"
  [s] (let [[non-digits r] (take-until-digit s)]
        (loop [acc [] v r]
          (let [[digits rprime] (take-until-not-digit v)])
          )))

(fact "anon"
      (anon "..123..45") => "..x2x..45"
      (provided
       (take-until-digit     "..123..") => [".." "123.."]
       (take-until-not-digit "123.."  ) => ["123" "..45"]
       (anon-digit "123")               => "x2x"))

(defn sum-of-2 "lazy seq of add two number"
  ([s] (sum-of-2 (+ (first s) (second s)) (rest s)))
  ([x r] (lazy-seq (cons x
                         (sum-of-2 (+ (first r) (second r)) (rest r))))))

(fact
 (first (sum-of-2 (range))) => 1
 (second (sum-of-2 (range))) => 3
 (nth (sum-of-2 (range)) 2) => 5)

(println "--------- END OF CORE  ----------" (java.util.Date.))

