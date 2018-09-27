(ns clj-cal.core
  (:use [com.tmountain seq-peek])
  (:require [clojure.pprint]
            [com.stuartsierra.frequencies :as freq])
  (:gen-class))

(def hours    8)
(def minutes  (* hours 60))
(def start-on 30)
(def padding  15)
(def duration 60)

(defn valid-start?
  [m]
  (or (zero? m) (zero? (mod m start-on))))

(defn bool-to-int
  [x]
  (if x 1 0))

(defn bin-seq
  [len n]
  (map
   (comp bool-to-int
         (fn [x] (bit-test n x)))
   (reverse (range len))))

(defn seq-to-min
  [s]
  (remove empty?
          (map-indexed
           (fn [idx itm]
             (if (not= 0 itm)
               [(* idx start-on),
                (+ duration padding (* idx start-on))]
               [])) s)))

(defn ** [x n] (reduce * (repeat n x)))

; the count of valid-starts defines how many scenarios to consider
; start-on=15 results in 32 valid start times for an 8 hour day
; for 32 valid start times, we have to evaluate 2^32 scenarios
; to exhaustively cover all the possible scheduling outcomes
(def valid-starts (filter valid-start? (range minutes)))

(def slots (count valid-starts))

; binary representation of every scenario
; 0 0 0 0
; 0 0 0 1
; 0 0 1 0
(def scenarios (map (partial bin-seq slots) (range (** 2 slots))))

(defn between?
  [x pair]
  (let [[a b] pair]
    (< a x b)))

(defn overlap?
  "Returns a boolean indicating whether the points in a given
   set of tuples overlap. Comparisons of equal value are considered
   to be non-overlapping."
  [xs]
  (let [[h & tail] xs
        [x y] h
        x-btw (map #(between? x %) tail)
        y-btw (map #(between? y %) tail)
        has-overlap (some true? (concat x-btw y-btw))]
    (if (empty? tail)
      false
      (if has-overlap
        true
        (recur tail)))))

(def not-overlap? (complement overlap?))

(defn rep-scenario
  [idx data]
  (println (format "scenario: idx=%s data=%s" idx data)))

(defn rep-count
  [idx data]
  (println (format "count: idx=%s data=%s" idx data)))

(defn -main
  [& args]
  (let [upper-limit (** 2 slots)
        seeds (range upper-limit) ; not inclusive of top value (intended)
        scenarios (map (comp seq-to-min (partial bin-seq slots)) (seq-peek 1000000 rep-scenario seeds))
        valid-scenarios (filter not-overlap? scenarios)
        valid-counts (map count (seq-peek 1 rep-count valid-scenarios))
        valid-freqs (frequencies valid-counts)]
    (clojure.pprint/pprint valid-freqs)
    (clojure.pprint/pprint (freq/stats valid-freqs))))
