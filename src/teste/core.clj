(ns teste.core
  (:require [clojure.core.reducers :as r]
            [criterium.core :as criterium]))

(set! *warn-on-reflection* true)

(defn sum-of-squares
  "Given a vector v, compute the sum of the squares of elements."
  ^double [^doubles v]
  (r/fold + (r/map #(* % %) v)))

(defn sum-of-squares2
  "This is much faster than above.  Post to stack-overflow to see."
  ^double [^doubles v]
  (loop [val 0.0
         i (dec (alength v))]
    (if (neg? i)
      val
      (let [x (aget v i)]
        (recur (+ val (* x x)) (dec i))))))

(defn sum-of-squares3
  "Given a vector v, compute the sum of the squares of elements."
  [v]
  (r/fold + (r/map #(* % %) v)))

(def a (double-array (range 10)))

(criterium/bench (sum-of-squares a))

(criterium/bench (sum-of-squares2 a))

(criterium/bench (sum-of-squares3 a))