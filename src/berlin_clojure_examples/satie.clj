(ns berlin-clojure-examples.satie
  (:use [clojure.core.match :only [match]]
        [overtone.polynome]
        [overtone.grid launchpad]
        [overtone.live]
        [overtone.inst sampled-piano]))

;; Based on Sam Aaron's work for monome: https://gist.github.com/1377020
;;Erik Satie Gnossienne No. 1
(def phrase1a [:v :vii :vi# :v :v :iv# :v :iv#])
(def phrase1b [:v :vii :vi# :v :vii# :i+ :vii# :i+])
(def phrase1c [:v :vii :vi# :v :v :iv# :iii :ii :i :ii :i :ii :iii :ii :ii :i])

(def phrase2 [:iii :iv :iii :ii :iii :iv :iii :ii :iii :ii :ii :i])

(def phrase3 [:v :vi# :vii# :i+ :ii+ :iv#+ :ii+ :i+ :ii+ :i+ :ii+ :i+ :i+ :vii# :vi :v :v :iv# :iii :ii :ii :i])

(def phrase1a-reprise [:v :vii :vi# :v :v :iv#])
(def phrase1b-reprise [:v :vii :vi# :v :vii# :i+])

(def phrase1-bass [:i-- [:i :v- :iii-] [:i :v- :iii-]])
(def phrase2-bass [:v-- [:v- :ii- :vii--] [:v- :ii- :vii--]])

(def phrase3-bass [:iv--- [:i- :iv- :vi-] [:i- :iv- :vi-]])


(def right-hand-degrees (concat phrase1a phrase1b phrase1c
                                phrase1a phrase1b phrase1c
                                phrase2
                                phrase2
                                phrase3
                                phrase3
                                phrase2
                                phrase2
                                phrase1a-reprise
                                phrase1b-reprise
                                phrase1a-reprise
                                phrase1b-reprise
                                phrase2
                                phrase2
                                phrase3
                                phrase3
                                phrase2
                                phrase2))


(def left-hand-degrees (concat (apply concat (repeat 6 phrase1-bass))  ;;A
                               phrase2-bass                            ;;B
                               (apply concat (repeat 8 phrase1-bass))  ;;C
                               phrase2-bass                            ;;D
                               (apply concat (repeat 2 phrase1-bass))  ;;E
                               (apply concat (repeat 2 phrase3-bass))  ;;F
                               (apply concat (repeat 2 phrase1-bass))  ;;G
                               (apply concat (repeat 2 phrase3-bass))  ;;H
                               (apply concat (repeat 14 phrase1-bass)) ;;I
                               (apply concat (repeat 2 phrase3-bass))  ;;J
                               (apply concat (repeat 2 phrase1-bass))  ;;K
                               (apply concat (repeat 2 phrase3-bass))  ;;L
                               (apply concat (repeat 10 phrase1-bass)) ;;M
                               (apply concat (repeat 2 phrase3-bass))  ;;N
                               (apply concat (repeat 2 phrase1-bass))  ;;O
                               (apply concat (repeat 2 phrase3-bass))  ;;P
                               (apply concat (repeat 14 phrase1-bass)) ;;Q
                               (apply concat (repeat 2 phrase3-bass))  ;;R
                               (apply concat (repeat 2 phrase1-bass))  ;;S
                               (apply concat (repeat 2 phrase3-bass))  ;;T
                               phrase1-bass                            ;;U
                               ))

(def lh-pitches (degrees->pitches left-hand-degrees :minor :F4))
(def rh-pitches (degrees->pitches right-hand-degrees :minor :F4))

(def cur-pitch-rh (atom -1))
(def cur-pitch-lh (atom -1))

(defn reset-pos
  []
  (reset! cur-pitch-rh -1)
  (reset! cur-pitch-lh -1))

(defn vol-mul
  [vol]
  (* vol 0.002))

(defn play-next-rh
  [vol]
  (let [idx (swap! cur-pitch-rh inc)
        pitch (nth (cycle rh-pitches) idx)]
    (sampled-piano pitch (vol-mul vol))))

(defn play-next-lh
  [vol]
  (let [idx (swap! cur-pitch-lh inc)
        pitch (nth (cycle lh-pitches) idx)]
    (if (sequential? pitch)
      (doseq [p pitch]
        (sampled-piano p (vol-mul vol)))
      (sampled-piano pitch (vol-mul vol)))))

(defn matching-notes
  "Find the matching sample in piano-samples which matches the midi note.
  Assumes the name of the sample contains a string repressntation of the midi
  note i.e. C4."
  [note]
  (filter #(if-let [n (match-note (:name %))]
             (= note (:midi-note n)))
          piano-samples))

(defn pitch-from-coords [x y]
  (let [octave [:i :ii :iii :iv :v :vi :vii :i+]
        roots  [:F0 :F1 :F2 :F3 :F4 :F5 :F6 :F7]
        pitches (degrees->pitches octave :major (nth roots x))]
    (nth pitches y)))

(defn init-polynome [poly]
  (remove-all-callbacks poly)
  (light-led-on-sustain poly)
  (on-press poly "foo" (fn [x y s]
                      (match [x y]
                             [_ 0] (play-next-lh (+ (rand-int 5) (* 12 (+ x 4))))
                             [_ 7] (play-next-rh (+ (rand-int 5) (* 12 (+ x 4))))
                             [7 _] (reset-pos))))
  )



