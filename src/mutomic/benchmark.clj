(ns mutomic.benchmark
  (:require [mutomic :as mu]
            [mutomic.platform :as p]))

(defn random-name []
  (let [alphabet "abcdefghijklmnopqrstuvwxyz"
        vowels "aeiou"]
    (apply str (concat [(p/to-upper-case (str (rand-nth alphabet)))]
                       (map (fn [_]
                              (if (< (rand) 0.3)
                                (rand-nth vowels)
                                (rand-nth alphabet)))
                            (range (+ (rand-int 7) 3)))))))

(defn random-people [n]
  (reduce (fn [ps facts]
            (apply conj ps facts))
          []
   (map (fn [i]
          (let [id (symbol (str "p" i))]
            [[id :name (random-name)]
             [id :age (rand-int 100)]]))
        (range n))))

(defn rand-pred [pred rand-fn & args]
  (let [n (apply rand-fn args)]
    (if (pred n)
      n
      (recur pred rand-fn args))))

(defn random-friends [people n]
  (let [c (count people)]
    (for [_ (range n)]
      (let [p1 (rand-pred odd? rand-int c)
            p2 (rand-pred odd? rand-int c)]
        [p1 :likes p2]))))

(defn random-society [num-people num-friends]
  (let [people (random-people num-people)]
    (concat people (random-friends people num-friends))))

(defn tiny-benchmark
  "benchmark non-indexed data vs indexed data.

note that the calculation gets quite expensive when both numbers are high, but
stay low when only the number of people is high.

that means our index kind of works and that the cross-product is expensive.

don't try the non-indexed version with high numbers unless you have a lot of time
to spare."
  []
  (let [s1 (random-society 1000000 10000)
        s1i (mu/index-many nil s1)
        query '{:find [?e ?o]
                :where [[?e :likes ?o]
                        [?o :likes ?e]]}]
    (prn "go!")
    ;(time (doall (mu/q query s1)))
    (time (doall (mu/q query s1i)))))
