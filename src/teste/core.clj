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
        (recur (unchecked-add val (unchecked-multiply x x)) (dec i))))))

(defn sum-of-squares3
  "Given a vector v, compute the sum of the squares of elements."
  [v]
  (r/fold + (r/map #(* % %) v)))

(defn sum-of-squares4 [v]
  (transduce (map #(* %1 %1)) + v))

(defn sum-of-squares5 [v]
  (reduce + (map #(* % %) v)))

(defn sum-of-squares6 ^double [^doubles v]
  (reduce + (amap v idx _ (let [item (aget v idx)] (* item item)))))

(defn sum-of-squares7 ^double [^doubles v]
  (let [^doubles squares (amap v idx _ (let [item (aget v idx)] (* item item)))]
    (areduce squares idx ret 0.0 (+ ret (aget squares idx)))))

(defn sum-of-squares7 ^double [^doubles v]
  (areduce v idx ret 0.0
           (let [item (aget v idx)]
             (+ ret (* item item)))))

(defn sum-of-squares8 ^double [^doubles v]
  (areduce v idx ret 0.0
           (let [item (aget v idx)]
             (unchecked-add ret (unchecked-multiply item item)))))

(def a (double-array (range 1000000)))

(comment
  (criterium/quick-bench (sum-of-squares a))

  (criterium/quick-bench (sum-of-squares2 a))

  (criterium/quick-bench (sum-of-squares3 a))

  (criterium/quick-bench (sum-of-squares4 a)))




