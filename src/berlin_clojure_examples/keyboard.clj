(ns berlin-clojure-examples.keyboard
  (:use [overtone.live]
        [overtone.polynome]
        [clojure.core.match :only [match]])
  (:require [berlin-clojure-examples.sequencer :as sequencer]
))

(defn init-polynome-keyboard [poly]
  (remove-all-callbacks poly)
  (let [inst-activation (atom {})]
    (on-press poly "foo" (fn [x y s]
                        (match [x y]
                               [_ 7] (sequencer/set-bass-scale (if (zero? x) 7 x) :minor :a1)
                               [_ _] (let [degree (get [:i :ii :iii :iv :v :vi :vii :vii#] x)
                                           [note]   (degrees->pitches [degree] :minor :a0)
                                           octave-offset (* 12 (- 7 y))]
                                       (swap! inst-activation
                                              assoc [x y] (sequencer/bass (midi->hz (+ octave-offset note))))))))
    (on-release poly "bar" (fn [x y s]
                             (match [x y]
                                    [_ 7] nil
                                    [_ _] (do
                                            (kill (@inst-activation [x y]))
                                            (swap! inst-activation
                                                   dissoc [x y])))))))