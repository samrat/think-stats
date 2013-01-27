(ns think-stats.pmf
  "Corresponds to http://greenteapress.com/thinkstats/Pmf.py")

(def hist frequencies)

(defn pmf [histogram]
  (let [n (apply + (vals histogram))]
    (reduce (fn [m kv]
              (assoc m
                (first kv)
                (/ (second kv) n)))
            {} (apply vector histogram))))

(defn mode [histogram]
  (ffirst (reverse (sort-by val histogram))))

;; Exercise 2-5
(defn pmf-mean [pmf]
  (apply + (map #(* (key %) (val %)) pmf)))

(defn pmf-var [pmf]
  (apply + (map #(* (val %)
                    (Math/pow (- (key %) (pmf-mean pmf)) 2))
                pmf)))
