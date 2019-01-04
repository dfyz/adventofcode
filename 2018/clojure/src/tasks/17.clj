(ns tasks.17
  (:require [clojure.set])
  (:import (java.awt.image BufferedImage)
           (javax.imageio ImageIO)
           (java.io File)))

(defn update-field [field line]
  (let [[_ first-coord & nums] (re-find #"(.)=(\d+), .=(\d+)..(\d+)" line)
        [first-val second-start second-end] (map #(Integer/parseInt %) nums)
        clay-positions (for [snd (range second-start (inc second-end))] [first-val snd])
        pos-converter (if (= first-coord "x") identity (comp vec reverse))]
    (apply conj field (map pos-converter clay-positions))))

(defn parse-input [filename]
  (let [lines (clojure.string/split-lines (slurp filename))]
    (reduce update-field #{} lines)))

(def sample (parse-input "inputs\\17_sample.txt"))
(def input (parse-input "inputs\\17.txt"))

(defn taken [clay water pos]
  (or (clay pos) (water pos)))

(defn expand [x y x-inc clay water]
  (loop [x x]
    (let [x-next (+ x x-inc)]
      (cond
        (taken clay water [x-next y]) x
        (not (taken clay water [x-next (inc y)])) x-next
        :else (recur x-next)))))

(defn step-water [clay [water trace]]
  (let [taken (partial taken clay water)
        y-max (apply max (map second clay))]
    (loop [x 500 y 0 trace trace]
      (if (> y y-max)
        [water (clojure.set/difference trace water)]
        (let [y-next (inc y)
              trace-next (conj trace [x y])]
          (if (not (taken [x y-next]))
            (recur x y-next trace-next)
            (let [x-left (expand x y -1 clay water)
                  x-right (expand x y 1 clay water)
                  can-settle-left (taken [x-left y-next])
                  can-settle-right (taken [x-right y-next])
                  settle-candidates (for [x (range x-left (inc x-right))] [x y])]
              (if (and can-settle-left can-settle-right)
                [(apply conj water settle-candidates) trace-next]
                (let [with-settled-in-trace (apply conj trace-next settle-candidates)]
                  (if (or can-settle-left (zero? (rand-int 2)))
                   (recur x-right y with-settled-in-trace)
                   (recur x-left y with-settled-in-trace)))))))))))

(defn find-stable-state [input]
  (loop [state [#{} #{}] iter 1]
    (let [state-next (last (take 100 (drop 1 (iterate #(step-water input %) state))))]
      (if (= state state-next)
        state-next
        (recur state-next (inc iter))))))

(defn solve-both [input]
  (let [y-min (apply min (map second input))
        [water trace] (find-stable-state input)
        count-good-positions (fn [positions] (count (filter #(>= (second %) y-min) positions)))
        good-water-count (count-good-positions water)
        good-trace-count (count-good-positions trace)]
    [(+ good-water-count good-trace-count)
     good-water-count]))

(defn save-image [clay [water trace] iter]
  (let [all-pos (clojure.set/union clay water trace)
        x-min (apply min (map first all-pos))
        x-max (apply max (map first all-pos))
        y-min 0
        y-max 600
        width (inc (- x-max x-min))
        height (inc (- y-max y-min))
        img (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        set-color (fn [x y color] (let [x-real (- x x-min)
                                        y-real (- y y-min)]
                                    (when (<= 0 y-real (dec height)) (.setRGB img x-real y-real color))))]
    (do
      (doseq [x (range width) y (range height)]
        (.setRGB img x y 0xFFFFFF))
      (doseq [[x y] clay]
        (set-color x y 0))
      (doseq [[x y] water]
        (set-color x y 0x0000FF))
      (doseq [[x y] trace]
        (set-color x y 0x8B00FF))
      (set-color 500 0 0x8B00FF)
      (ImageIO/write img "png" (File. (format ".\\images\\%03d.png" iter))))))

(defn visualize [input]
  (loop [state [#{} #{}] iter 0]
    (save-image input state iter)
    (when (< iter 450)
      (recur (step-water input state) (inc iter)))))