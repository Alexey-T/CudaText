; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 908)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 126.462))))

(defn step_9712 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6511 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8820 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_4245 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8672 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8886 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_2089 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_4337 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 430)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 0o561))))

(defn step_4108 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6341 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_2589 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9472 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1616 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_510 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_907 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6470 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 452)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 1245))))

(defn step_6639 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1713 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6067 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8922 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_199 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7640 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9322 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1599 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 115)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 34.10e-3))))

(defn step_6356 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1944 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_5226 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7284 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_711 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7060 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_4774 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_236 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 823)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 75.642))))

(defn step_9939 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_5418 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_3397 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7785 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_5417 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6764 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_5135 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_803 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 998)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 94.3e-9))))

(defn step_7523 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6124 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7186 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1626 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_508 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9659 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6384 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9017 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 510)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 6294))))

(defn step_1511 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8155 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9212 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_223 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6291 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9170 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_4499 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8366 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 930)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 551.413))))

(defn step_2671 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9812 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9585 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6452 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_2703 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_2687 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_5839 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_3033 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 193)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 61.81e7))))

(defn step_7057 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9194 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_5801 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7395 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_911 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_5413 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1347 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_4319 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 249)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 3630))))

(defn step_6997 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_2688 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1091 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6164 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9983 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6651 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8198 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_2091 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 696)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 0o442))))

(defn step_916 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7088 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_5290 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1947 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8604 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7511 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7400 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8887 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 220)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 251.850))))

(defn step_9681 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_2785 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8836 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7211 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1926 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7569 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6610 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_3739 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 46)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 68.52e-4))))

(defn step_3131 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8752 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_4825 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_2713 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_9241 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_6367 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1839 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7789 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

; Sample Clojure source for lexer benchmarking
(ns benchmark.core
(ns benchmark.core
  (:require [clojure.string :as str]))

(def max-items 16)
(def prefix "bench-")

(defn score [entry]
  (let [w (:weight entry)]
    (if (neg? w) 0 (* w 0x246B))))

(defn step_50 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_3900 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7516 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_8270 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_3152 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_7364 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_3094 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(defn step_1222 [seed]
  (loop [i 0 acc 0]
    (if (>= i max-items)
      acc
      (recur (inc i)
             (if (even? i)
               (+ acc (* i seed))
               (- acc (mod i 7)))))))

(def entries
  [{:name "first" :weight 1.5} {:name "second" :weight 2.75}])

(doseq [e entries]
  (println prefix (:name e) (score e)))

