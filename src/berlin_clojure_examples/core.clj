(ns berlin-clojure-examples.core
  (:use [overtone.grid.launchpad]
        [overtone.polynome]
        [overtone.live])
  (:require [berlin-clojure-examples.sequencer :as sequencer]
            [berlin-clojure-examples.piano :as piano]
            [berlin-clojure-examples.satie :as satie]
            [berlin-clojure-examples.keyboard :as keyboard]
            ))

(defonce lp (make-launchpad))

(defonce poly (init lp))

(comment
  (remove-all-callbacks poly)
  (piano/init-polynome (init lp))
  (satie/init-polynome (init lp))
  (sequencer/init-polynome-normal (init lp))
  )

(comment
  (sequencer/init-polynome-bass (init lp))
  (sequencer/set-bass-scale 0 :minor :a2)
  (sequencer/set-bass-scale 5 :minor :a1)
  (sequencer/set-bass-scale 4 :minor :a1)
  (sequencer/set-bass-scale 0 :major :c3)
  )

(comment
  (keyboard/init-polynome-keyboard (init lp))
  (stop)
  )
