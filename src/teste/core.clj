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

(defn ^double sum-of-squares4 [^doubles v]
  (reduce + (amap v idx _ (let [item (aget v idx)] (* item item)))))

(defn ^double sum-of-squares5 [^doubles v]
  (let [^doubles squares (amap v idx _ (let [item (aget v idx)] (* item item)))]
    (areduce squares idx ret 0 (+ ret (aget squares idx)))))

(def a (double-array (range 10)))

(comment
  (criterium/quick-bench (sum-of-squares a))

  (criterium/quick-bench (sum-of-squares2 a))

  (criterium/quick-bench (sum-of-squares3 a))

  (criterium/quick-bench (sum-of-squares4 a))

  (criterium/quick-bench (sum-of-squares5 a)))
