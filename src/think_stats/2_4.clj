(ns think-stats.2-4
  "Example 2-4"
  (:use think-stats.pmf))

(defn remaining-lifetime [pmf age]
  (let [normalize (fn [rl]
                    (let [total (apply + (map second rl))]
                      (apply hash-map (interleave (map first rl)
                                                  (map #(/ % total)
                                                       (map second rl))
                                                  ))))]
    (normalize (remove #(neg? (key %))
                       (apply hash-map (interleave (map #(- (key %) age) pmf)
                                                   (vals pmf)))))))