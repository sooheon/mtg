(ns scratch.stats)

(def NO-OUAT {:trail 4
              :planeswalker 4
              :artifact 4
              :land 24
              :creature 24})

(def W-OUAT {:trail 4
             :ouat 4
             :planeswalker 3
             :artifact 3
             :land 24
             :creature 22})

(defn vec-dissoc
  "Dissoc's given element from vector."
  [v elem]
  (let [v (vec v)]
    (->> (update v (.indexOf v elem) (constantly nil))
         (remove nil?))))

(defn count-cards [decklist]
  (apply + (vals decklist)))

(map count-cards [NO-OUAT W-OUAT])
;; => (60 60)

(defn construct-deck [decklist]
  (reduce (fn [deck [card n]]
            (concat deck (repeat n card)))
          [] decklist))

(defn counts [game]
  (into {} (map (fn [[k v]]
                  [k {:cards (frequencies v)
                      :total (count v)}])
                game)))

(defn in-hand? [game q]
  (let [pred (cond
               (set? q) q
               (keyword? q) #{q}
               :else #{q})]
    (some pred (:hand game))))

(defn draw
  [{:keys [deck] :as game} n]
  (-> game
      (update :hand concat (take n deck))
      (update :deck #(drop n %))))

(defn discard [game card]
  (assert (in-hand? game card))
  (-> game
      (update :graveyard conj card)
      (update :hand vec-dissoc card)))

(defn init-game [decklist]
  (draw {:deck (shuffle (construct-deck decklist))}
        7))

(defn play-ouat [{:keys [deck] :as game}]
  (assert (in-hand? game :ouat))
  (let [g (-> game
              (discard :ouat)
              (update :deck #(drop 5 %)))
        revealed (take 5 deck)
        hits (filter #{:land :creature} revealed)
        misses (remove #{:land :creature} revealed)]
    (if (seq hits)
      (let [chosen (first (shuffle hits))
            bottomed (concat (vec-dissoc hits chosen) misses)]
        (-> g
            (update :hand conj chosen)
            (update :deck concat bottomed)))
      (update g :deck concat revealed))))

(frequencies
 (map
  (fn [m] (in-hand? m :trail))
  (repeatedly
   100000
   #(loop [g (init-game W-OUAT)]
      (if (in-hand? g :ouat)
        (draw (play-ouat g) 1)
        (draw g 1))))))
;; => {nil 55405, :trail 44595}

(frequencies
 (map
  (fn [m] (in-hand? m :trail))
  (repeatedly
   100000
   #(loop [g (init-game NO-OUAT)]
      (if (in-hand? g :ouat)
        (draw (play-ouat g) 1)
        (draw g 1))))))
;; => {nil 55545, :trail 44455}


;;; probabilistically

(defn factorial [n]
  (assert (not (neg? n)))
  (reduce *' (range 1 (inc n))))

(factorial 0)
;; => 1
(factorial 5)
;; => 120

(defn nCr [n r]
  (/ (factorial n)
     (* (factorial r)
        (factorial (- n r)))))

(/ 4 52)

(float (+ (* (/ (* (nCr 48 5) (nCr 4 0)) (nCr 52 5))
             (/ 4 47))
          (* (/ (* (nCr 48 4) (nCr 4 1)) (nCr 52 5))
             (/ 3 47))
          (* (/ (* (nCr 48 3) (nCr 4 2)) (nCr 52 5))
             (/ 2 47))
          (* (/ (* (nCr 48 2) (nCr 4 3)) (nCr 52 5))
             (/ 1 47))
          (* (/ (* (nCr 48 1) (nCr 4 4)) (nCr 52 5))
             0)))
