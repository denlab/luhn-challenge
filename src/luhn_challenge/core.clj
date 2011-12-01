(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished char-type digit? cc-max-size other-char?  get-action
            blank? )

(defn k-output-char-and-flush-acc
  [ctx] )

(fact "k-output-char-and-flush-acc"
      (k-output-char-and-flush-acc {:seq- [:c1 :c2], :acc :acc-seq}) => {:seq- [:c2], :to-eat :to-eat-seq})

(let [kw->fn {:k-output-char-and-flush-acc k-output-char-and-flush-acc}]
  (defn get-op
   [op-keyw] (kw->fn op-keyw)))

(fact
 (get-op :k-output-char-and-flush-acc) => (exactly k-output-char-and-flush-acc))

(defn char-type "Given a char returns the type of it: :blank | :other | :digit"
  [c] (case c
        \0 :digit, \1 :digit, \2 :digit, \3 :digit, \4 :digit, \5 :digit, \6 :digit, \7 :digit, \8 :digit, \9 :digit,
        \- :blank, \space :blank,
        :other))

(tabular
 (fact (char-type ?c) => ?expected)
 ?c ?expected
 \0 :digit, \1 :digit, \2 :digit, \3 :digit, \4 :digit, \5 :digit, \6 :digit, \7 :digit, \8 :digit, \9 :digit,
 \- :blank, \space :blank,
 \_ :other, \a :other)

(defn next-op "Given a context returns a keyword representing the next operation to perform"
  [{:keys [seq- digit-cnt blank-cnt to-eat]}]
  (if to-eat
    :k-consume
    (case (char-type (first seq-))
      :other :k-output-char-and-flush-acc
      :digit (cond (< (inc digit-cnt) (cc-max-size)) :k-acc-digit
                   :else                             :k-anon-chunk)
      :blank (cond (zero? digit-cnt) :k-output-char-and-flush-acc
                   (zero? blank-cnt) :k-init-count-blank
                   :else             :k-acc-blank)
      :empty :k-anon-chunk)))

(fact "next-op : consuming"
      (next-op {:to-eat :seq}) => :k-consume)

(fact "next-op: other-char"
      (next-op {:seq- [:c]}) => :k-output-char-and-flush-acc
      (provided
       (char-type :c) => :other))

(fact "next-op: digit acc not full"
      (next-op {:seq- [:c] :digit-cnt 0}) => :k-acc-digit
      (provided
       (cc-max-size)    => 2
       (char-type :c) => :digit))

(fact "next-op: digit acc full"
      (next-op {:seq- [:c] :digit-cnt 1}) => :k-anon-chunk
      (provided
       (cc-max-size)    => 2
       (char-type :c) => :digit))

(fact "next-op: blank without digit acc"
      (next-op {:seq- [:c] :digit-cnt 0}) => :k-output-char-and-flush-acc
      (provided
       (char-type :c) => :blank))

(fact "next-op: blank with digit acc, no blank count"
      (next-op {:seq- [:c] :digit-cnt 1 :blank-cnt 0}) => :k-init-count-blank
      (provided
       (char-type :c) => :blank))

(fact "next-op: blank with digit acc, accumulating blank"
      (next-op {:seq- [:c] :digit-cnt 1 :blank-cnt 1}) => :k-acc-blank
      (provided
       (char-type :c) => :blank))

(fact "next-op: EOF, but has acc"
      (next-op {:seq- [] :digit-cnt 1}) => :k-anon-chunk
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

(defprotocol State
  (next [this]))

(defrecord StateC [i]
    State
    (next [this] (println "i=" i ", C state")
      ))

(println "--------- END CORE  ----------" (java.util.Date.))

