(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished char-type digit? cc-max-size other-char? get-op get-action
            blank? )

(defn char-type "Given a char returns the type of it: :blank | :other | :digit"
  [c])

(defn next-op "Given a context returns a keyword representing the next operation to perform"
  [{:keys [seq- digit-cnt blank-cnt to-eat]}]
  (if to-eat
    :consume
    (case (char-type (first seq-))
      :other :output-char-and-flush-acc
      :digit (cond (< (inc digit-cnt) (cc-max-size)) :acc-digit
                   :else                             :anon-chunk)
      :blank (cond (zero? digit-cnt) :output-char-and-flush-acc
                   (zero? blank-cnt) :init-count-blank
                   :else             :acc-blank)
      :empty :anon-chunk)))

(fact "next-op : consuming"
      (next-op {:to-eat :seq}) => :consume)

(fact "next-op: other-char"
      (next-op {:seq- [:c]}) => :output-char-and-flush-acc
      (provided
       (char-type :c) => :other))

(fact "next-op: digit acc not full"
      (next-op {:seq- [:c] :digit-cnt 0}) => :acc-digit
      (provided
       (cc-max-size)    => 2
       (char-type :c) => :digit))

(fact "next-op: digit acc full"
      (next-op {:seq- [:c] :digit-cnt 1}) => :anon-chunk
      (provided
       (cc-max-size)    => 2
       (char-type :c) => :digit))

(fact "next-op: blank without digit acc"
      (next-op {:seq- [:c] :digit-cnt 0}) => :output-char-and-flush-acc
      (provided
       (char-type :c) => :blank))

(fact "next-op: blank with digit acc, no blank count"
      (next-op {:seq- [:c] :digit-cnt 1 :blank-cnt 0}) => :init-count-blank
      (provided
       (char-type :c) => :blank))

(fact "next-op: blank with digit acc, accumulating blank"
      (next-op {:seq- [:c] :digit-cnt 1 :blank-cnt 1}) => :acc-blank
      (provided
       (char-type :c) => :blank))

;; TODO redonant
(fact "next-op: EOF, but to-eat"
      (next-op {:seq- [] :to-eat [:x]}) => :consume)

(fact "next-op: EOF, but has acc"
      (next-op {:seq- [] :digit-cnt 1}) => :anon-chunk
      (provided
       (char-type nil) => :empty))



(defn anon-proc-
  [ctx] ((get-op (next-op ctx)) ctx))

(fact "anon-proc-"
      (anon-proc- :ctx) => :ctx2
      (provided
       (next-op :ctx) => :op-keyword
       ;; emulating a mock that returns a mock
       (get-op  :op-keyword) => {:ctx :ctx2}))

(defn anon- "Takes a seq of char, return a seq of vec of anonymised chars"
  [s] (take-while identity
                  (rest (iterate anon-proc- {:seq- s}))))

(fact "anon-"
      (take 10 (anon- :in-seq)) => [:ctx1 :ctx2]
      (provided
       (anon-proc- {:seq- :in-seq}) => :ctx1
       (anon-proc- :ctx1)   => :ctx2
       (anon-proc- :ctx2)   => nil))

(defn anon "Takes a seq of char, returns a seq of anonymised chars"
  [s] (flatten (anon- s)))

(fact
 (anon :in-seq) => [:x1 :x2 :x3]
 (provided
  (anon- :in-seq) => [[:x1 :x2] [:x3]]))

(println "--------- END CORE  ----------" (java.util.Date.))

