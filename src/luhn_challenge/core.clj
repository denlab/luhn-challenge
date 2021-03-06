(ns ^{:doc "Luhn challenge by crazybob: http://blog.crazybob.org/2011/11/coding-challenge-luhny-bin.html"}
  luhn-challenge.core
  (:use     [midje.sweet])
  (:use     [clojure.pprint :only [pp pprint]])
  (:require [clojure.walk :as w])
  (:require [clojure.set  :as set])
  (:import (java.util Date)))

(println "--------- BEGIN CORE  ----------" (java.util.Date.))

(unfinished )

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

(defn digit? [c]
  (= (char-type c) :digit))

(fact "digit? true"
      (digit? :c) => true
      (provided
       (char-type :c) => :digit))

(fact "digit? false"
      (digit? :c) => false
      (provided
       (char-type :c) => :other-stuff))

(defn extract-digits
  [acc] (filter digit? acc))

(fact "extract-digits"
      (extract-digits [:a :b :c]) => [:a :c]
      (provided
       (digit? :a) => true
       (digit? :b) => false
       (digit? :c) => true))

(defn blank?
  [c] (= (char-type c) :blank))

(fact "blank? : true"
      (blank? :c) => true
      (provided
       (char-type :c) => :blank))

(fact "blank? : false"
      (blank? :c) => false
      (provided
       (char-type :c) => :other-stuff))

(defn cc-max-size [] 16)

(fact (cc-max-size) => 16)

(defn extract-blanks
  [acc] (reduce (fn [[blks1 blks2 :as blks] [idx chr]]
                  (cond (not (blank? chr))    blks
                        (< idx (cc-max-size)) [(assoc blks1 idx chr) blks2]
                        :else                 [blks1 (assoc blks2 idx chr)]))
                [{} {}]
                (zipmap (range) acc)))

(fact "extract-blanks"
      (extract-blanks [:c1 :c2 :c3 :c4]) => [{0 :c1, 1 :c2} {2 :c3}]
      (provided
       (cc-max-size) => 2
       (blank? :c1) => true
       (blank? :c2) => true
       (blank? :c3) => true
       (blank? :c4) => false))

(defn merge-combin-cb
  [vec-coord-cb]
  (map (fn [idx] (some (fn [[[start end] _]] (< (dec start) idx end)) (filter #(second %) vec-coord-cb)))
       (range (bit-shift-left (cc-max-size) 1))))

(fact "merge-combin-cb"
      (merge-combin-cb [[[0 2] true]
                        [[1 3] true]
                        [[2 4] false]]) => [true true true nil]
      (provided
       (cc-max-size) => 2))

(defn sum-ddigits
  [ddigits] 
  (reduce
   (fn [sum numb] (if (< 9 numb)
                   (+ sum 1 (- numb 10))
                   (+ sum numb)))
   0
   ddigits))

(facts
  (sum-ddigits [8 14 6 10]) => 20
  (sum-ddigits [9 16 7 12]) => 26)

(defn double-digits
  [digits]
  (mapcat (fn [[odd even]] [odd (* 2 even)])
          (split-at 2 (reverse digits))))

(facts
  (double-digits [5 6 7 8]) => [8 14 6 10]
  (double-digits [6 7 8 9]) => [9 16 7 12])

(defn cb? "Is the vector of digits a cb?"
  [digits]
  (zero? (rem (sum-ddigits (double-digits digits)) 10)))

(fact
  (cb? :digits) => true
  (provided
    (double-digits :digits) => :digits2
    (sum-ddigits :digits2) => 10))

(fact
  (cb? :digits) => false
  (provided
    (double-digits :digits) => :digits2
    (sum-ddigits :digits2) => 11))

(facts "IT test on luhn check"
  (cb? [5 6 7 8]) => true
  (cb? [6 7 8 9]) => false)

(defn cc-min-size "Minimal size of cbs"
  [] 14)

(fact (cc-min-size) => 14)

(defn coords "Given a vector of digits, extract the vector of coordinates"
  [digits]
  (for [start (range (cc-min-size))
        end (range 1 (inc (cc-max-size)))]
    [start (+ start end)]))

(fact
  (coords [:d0 :d1 :d2 :d3 :d4 :d5]) => (contains [[0 1] [0 2] [0 3]
                                                   [1 2] [1 3] [1 4]] :in-any-order) 
  (provided
    (cc-max-size) => 3
    (cc-min-size) => 2))

(defn subvec-cb "Given a vector of digits, retrieve a map of {:coord :subvec}"
  [digits]
  (reduce
   (fn [m [start end :as coord-key]] (assoc m coord-key (subvec digits start (inc end))))
   {}
   (coords digits)))

(fact
  (subvec-cb [:d0 :d1 :d2]) => {[0 1] [:d0 :d1], [1 2] [:d1 :d2]}
  (provided
    (coords [:d0 :d1 :d2]) => [[0 1] [1 2]]))

(defn combin-cb
  [digits] (map (fn [[coord subv]]
                  [coord (cb? subv)])
                (subvec-cb digits)))

(fact "combin-cb"
      (combin-cb :digits) => [[:coord1 :anon1]
                              [:coord2 :anon2]]
      (provided
       (subvec-cb :digits) => {:coord1 :subvec1
                               :coord2 :subvec2}
       (cb? :subvec1) => :anon1
       (cb? :subvec2) => :anon2))

(defn anon-bits
  [digits] (split-at (cc-max-size)
                     (merge-combin-cb (combin-cb digits))))

(fact "anon-bits"
      (anon-bits :digits) => [[true true] [false false]]
      (provided
       (cc-max-size) => 2
       (combin-cb :digits)   => :vec-coord-cb
       (merge-combin-cb :vec-coord-cb) => [true true false false]))

(defn merge-anon-bits
  [bits1 bits2] (map #(or %1 %2)
                     bits1 bits2))

(fact "merge-anon-bits"
      (merge-anon-bits [true true  false false]
                       [true false true  false]) => [true true true false])

(defn anon-char [] \X)

(fact (anon-char) => \X)

(defn insert-blanks
  [dgts blks] (second
               (reduce (fn [[idx res] d] (if-let [blk (blks idx)]
                                          [(+ 2 idx) (conj res blk d)]
                                          [(inc idx) (conj res d)]))
                       [0 []]
                       dgts)))

(fact "insert-blanks"
      (insert-blanks [:d1 :d2] {1 :b}) => [:d1 :b :d2])

(defn recompose-out
  [digits first-half-blanks first-half-anon-bits] 
  (let [x (anon-char)] 
    (insert-blanks (map (fn [d anon?] (if anon? x d))
                        digits
                        first-half-anon-bits)
                   first-half-blanks)))

(fact "recompose-out"
      (recompose-out [:d1 :d2 :d3 :d4] :blanks [true false]) => [:x :b :d2]
      (provided
       (anon-char) => :x
       (insert-blanks [:x :d2] :blanks) => [:x :b :d2]))

(defn anon-acc
  [acc old-2nd-half-to-anon] (let [dgts (extract-digits acc)
                      [blk1 blk2] (extract-blanks acc)
                      [abts1 abts2] (anon-bits dgts)
                      first-half-mrgd-anon-bits (merge-anon-bits old-2nd-half-to-anon abts1)]
                  {:out (recompose-out dgts blk1 first-half-mrgd-anon-bits)
                   :acc (insert-blanks dgts blk2)
                   :to-anon abts2}))

(fact "anon-acc"
      (anon-acc :acc :to-anon) => {:out :out,
                                   :acc :acc2,
                                   :to-anon :anon-bits2}
      (provided
       (extract-digits :acc)                                => :digits
       (extract-blanks :acc)                                => [:blk-map1 :blk-map2]
       (anon-bits      :digits)                             => [:anon-bits1 :anon-bits2]
       (merge-anon-bits :to-anon :anon-bits1)               => :merged-anon-bits
       (recompose-out  :digits :blk-map1 :merged-anon-bits) => :out
       (insert-blanks  :digits :blk-map2)                   => :acc2))

(defn anon-partial
  "The accumulation of digits necessary to call anon-acc can be interrupted by an :other char. In this case the buffer is not full *BUT* it could contain credit card numbers.
This incomplete buffer must be anonymised, that's what this function does."
  [o acc0 to-anon0]
  (let [{:keys [acc to-anon out]} (anon-acc acc0 to-anon0)]
    {:acc acc, :to-anon to-anon, :out (conj out o)}))

(fact
  (anon-partial :o :acc0 :to-anon0) => {:acc :acc1, :to-anon :to-anon1, :out [:a0 :a1 :o]}
  (provided
    (anon-acc :acc0 :to-anon0) => {:acc :acc1, :to-anon :to-anon1, :out [:a0 :a1]}))

(defprotocol State
  "when out returns nil, mark the end"
  (nxt [this c])
  (out [this]))

(defrecord HandleDigit [a b c] State (nxt [this c] "*** declare ***"))

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

(defn maybe-add
  [b acc] (if (digit? (last acc))
            [[] (conj acc b)]
            [b  acc]))

(fact "maybe-add : first blank"
      (maybe-add :b [:d1 :d2]) => [[] [:d1 :d2 :b]]
      (provided
       (digit? :d2) => true))

(fact "maybe-add : not first blank"
      (maybe-add :b [:d1 :blk]) => [:b [:d1 :blk]]
      (provided
       (digit? :blk) => false))

(defrecord HandleBlank [b acc to-anon]
  State
  (nxt [this c] (case (char-type c)
                  :digit (HandleDigit. c (second (maybe-add b acc)) to-anon)
                  :blank (HandleBlank. c (second (maybe-add b acc)) to-anon)
                  :other (HandleOther. c (second (maybe-add b acc)) to-anon)
                  :empty (HandleOther. c (second (maybe-add b acc)) to-anon)))
  (out [this] (first (maybe-add b acc))))

(fact "HandleBlank : out"
  (out (HandleBlank. :b :acc :to-anon)) => :out
  (provided
   (maybe-add :b :acc) => [:out :acc2]))

(fact "HandleBlank : nxt digit"
      (nxt (HandleBlank. :b :acc :to-anon) :c) => (HandleDigit. :c :acc2 :to-anon)
      (provided
       (maybe-add :b :acc) => [:out :acc2]
       (char-type :c) => :digit))

(fact "HandleBlank : nxt blank"
      (nxt (HandleBlank. :b :acc :to-anon) :c) => (HandleBlank. :c :acc2 :to-anon)
      (provided
       (maybe-add :b :acc) => [:_ :acc2]
       (char-type :c) => :blank))

(fact "HandleBlank : nxt other"
      (nxt (HandleBlank. :b :acc :to-anon) :c) => (HandleOther. :c :acc2 :to-anon)
      (provided
       (maybe-add :b :acc) => [:_ :acc2]
       (char-type :c) => :other))

(fact "HandleBlank : nxt other"
      (nxt (HandleBlank. :b :acc :to-anon) :c) => (HandleOther. :c :acc2 :to-anon)
      (provided
       (maybe-add :b :acc) => [:_ :acc2]
       (char-type :c) => :empty))

(defn acc-full?
  [acc] (<= (* 2 (cc-max-size)) (count acc)))

(fact "acc-full? not full"
      (acc-full? [:a :b :c]) => false
      (provided
       (cc-max-size) => 2))

(fact "acc-full? full"
      (acc-full? [:a :b :c :d]) => true
      (provided
       (cc-max-size) => 2))

(defn maybe-anon
  [d acc to-anon] (let [conjed (conj acc d)]
                    (if (acc-full? conjed)
                      (anon-acc conjed to-anon)
                      {:out [], :acc conjed, :to-anon to-anon})))

(fact "maybe-anon : acc full"
  (maybe-anon :d [:d1 :d2] :toanon) => {:out :o, :acc :acc2, :to-anon :toanon2}
  (provided
    (acc-full? [:d1 :d2 :d]) => true
    (anon-acc [:d1 :d2 :d] :toanon) => {:out :o, :acc :acc2, :to-anon :toanon2}))

(fact "maybe-anon : acc not full"
      (maybe-anon :d [:d1 :d2] :toanon) => {:out [], :acc [:d1 :d2 :d], :to-anon :toanon}
      (provided
       (acc-full? [:d1 :d2 :d]) => false))

(defrecord HandleDigit [d acc to-anon]
  State
  (nxt [this c] (let [{:keys [acc to-anon]} (maybe-anon d acc to-anon)]
                  (case (char-type c)
                    :digit (HandleDigit. c acc to-anon)
                    :blank (HandleBlank. c acc to-anon)
                    :other (HandleOther. c acc to-anon)
                    :empty (HandleOther. c acc to-anon))))
  (out [this] (:out (maybe-anon d acc to-anon))))

(fact "HandleDigit : out"
  (out (HandleDigit. :d :acc :to-anon)) => :o
  (provided
    (maybe-anon :d :acc :to-anon) => {:out :o, :acc :acc2, :to-anon :to-anon2}))

(fact "HandleDigit : nxt digit"
  (nxt (HandleDigit. :d :acc :to-anon) :c) => (HandleDigit. :c :acc2 :to-anon2)
  (provided
    (maybe-anon :d :acc :to-anon) => {:out :o, :acc :acc2, :to-anon :to-anon2}
    (char-type :c) => :digit))

(fact "HandleDigit : nxt blank"
      (nxt (HandleDigit. :d :acc :to-anon) :c) => (HandleBlank. :c :acc2 :to-anon2)
      (provided
       (maybe-anon :d :acc :to-anon) => {:out :o, :acc :acc2, :to-anon :to-anon2}
       (char-type :c) => :blank))

(fact "HandleDigit : nxt other"
      (nxt (HandleDigit. :d :acc :to-anon) :c) => (HandleOther. :c :acc2 :to-anon2)
      (provided
       (maybe-anon :d :acc :to-anon) => {:out :o, :acc :acc2, :to-anon :to-anon2}
       (char-type :c) => :other))

(fact "HandleDigit : nxt empty"
      (nxt (HandleDigit. :d :acc :to-anon) :c) => (HandleOther. :c :acc2 :to-anon2)
      (provided
       (maybe-anon :d :acc :to-anon) => {:out :o, :acc :acc2, :to-anon :to-anon2}
       (char-type :c) => :empty))

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
