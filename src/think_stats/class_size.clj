(ns think-stats.class-size
  "Example 3-1")

(def class-sizes
  {7  8
   12 8
   17 14
   22 4
   27 6
   32 12
   37 8
   42 3
   47 2})

(defn deans-mean
  "The mean as perceived by the dean."
  []
  (let [total (apply + (vals class-sizes))]
    (float (/ (apply + (map #(* (key %) (val %)) class-sizes))
              total))))

(defn students-mean)