(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished maybee-anon anon-acc add-digit-to-acc acc-full?
            anon-partial max-cc-size char-type digit?  cc-max-size
            other-char?  get-action blank? )

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

(defprotocol State
  "when out returns nil, mark the end"
  (nxt [this c])
  (out [this]))

(defrecord HandleOther [o acc to-anon]
  State
  (nxt [this c] (case (char-type c)
                  :other (HandleOther. c nil nil)
                  :blank (HandleOther. c nil nil)
                  :digit (HandleDigit. c nil nil)
                  :empty nil))
  (out [this] (anon-partial o acc to-anon)))

(fact "HandleOther : nxt other"
  (nxt (HandleOther. :_ :_ :_) :c) => (HandleOther. :c nil nil)
  (provided
    (char-type :c) => :other))

(fact "HandleOther : nxt blank"
  (nxt (HandleOther. :_ :_ :_) :c) => (HandleOther. :c nil nil)
  (provided
    (char-type :c) => :blank))

(fact "HandleOther : nxt digit"
  (nxt (HandleOther. :_ :_ :_) :c) => (HandleDigit. :c nil nil)
  (provided
    (char-type :c) => :digit))

(fact "HandleOther : nxt empty"
  (nxt (HandleOther. :_ :_ :_) :c) => nil
  (provided
    (char-type :c) => :empty))

(fact "HandleOther : out"
  (out (HandleOther. :o :acc :to-anon)) => :anon-seq
  (provided
    (anon-partial :o :acc :to-anon) => :anon-seq))

(defrecord HandleBlank [b acc to-anon]
    State
    (nxt [this c]))

(defrecord HandleDigit [d acc to-anon]
  State
  (nxt [this c] (let [{:keys [acc to-anon]} (maybee-anon d acc to-anon)]
                  (case (char-type c)
                    :digit (HandleDigit. c acc to-anon))))
  (out [this] (:out (maybee-anon d acc to-anon))))

(fact "HandleDigit : out"
  (out (HandleDigit. :d :acc :to-anon)) => :o
  (provided
    (maybee-anon :d :acc :to-anon) => {:out :o, :acc :acc2, :to-anon :to-anon2}))

(fact "HandleDigit : nxt digit"
  (nxt (HandleDigit. :d :acc :to-anon) :c) => (HandleDigit. :c :acc2 :to-anon2)
  (provided
    (maybee-anon :d :acc :to-anon) => {:out :o, :acc :acc2, :to-anon :to-anon2}
    (char-type :c) => :digit))

(defn digit?
  [c] (= :digit (char-type c)))

(fact "digit? yes"
  (digit? :c) => true
  (provided
    (char-type :c) => :digit))

(fact "digit? no"
  (digit? :c) => false
  (provided
    (char-type :c) => :other-stuff))

(defn init-state
  [c] (if (digit? c)
        (HandleDigit. c nil nil)
        (HandleOther. c nil nil)))

(fact "init-state : digit"
  (init-state :c) => (HandleDigit. :c nil nil)
  (provided
    (digit? :c) => true))

(fact "init-state : empty, blank or other"
  (init-state :c) => (HandleOther. :c nil nil)
  (provided
    (digit? :c) => false))

(defn anon- "Takes a seq of char, return a seq of vec of anonymised chars"
  [s] (take-while #(% 1)
                  (iterate (fn [[[frst & rst] state]] [rst (nxt state frst)])
                           [(rest s) (init-state (first s))])))

(fact "anon-: start"
  (take 10 (anon- [:x :y])) => [[[:y] :state1]
                                [nil  :state2]
                                [nil  :state3]]
  (provided
    (init-state :x)   => :state1
    (nxt :state1 :y)  => :state2
    (nxt :state2 nil) => :state3
    (nxt :state3 nil) => nil))

(defn anon "Takes a seq of char, returns a seq of anonymised chars"
  [s] (flatten (map out (anon- s))))

(fact
 (anon :in-seq) => [:x1 :x2 :x3]
 (provided
  (anon- :in-seq) => [:state1 :state2 :state3] 
  (out   :state1) => [:x1]
  (out   :state2) => []
  (out   :state3) => [:x2 :x3]))

(println "--------- END CORE  ----------" (java.util.Date.))



