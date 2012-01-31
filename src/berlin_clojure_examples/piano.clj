(ns berlin-clojure-examples.piano
  (:use [overtone.live]
        [overtone.inst.sampled-piano]
        [overtone.polynome]))

(defn init-polynome [poly]
  (remove-all-callbacks poly)
  (light-led-on-sustain poly)
  (on-press poly "foo" (fn [x y s]
                         (let [degree (get [:i :ii :iii :iv :v :vi :vii :vii#] x)
                               [note]   (degrees->pitches [degree] :minor :a0)
                               octave-offset (* 12 (- 7 y))]
                           (sampled-piano (+ note octave-offset))))))