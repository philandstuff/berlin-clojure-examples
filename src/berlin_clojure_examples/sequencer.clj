(ns berlin-clojure-examples.sequencer
  (:use [overtone.live]
        [overtone.polynome]))

(stop)

(definst dub-kick
  [freq  80]
  (let [cutoff-env (perc 0.001 1 freq -20)
        amp-env (perc 0.001 1 1 -8)
        osc-env (perc 0.001 1 freq -8)
        noiz (lpf (white-noise) (+ (env-gen:kr cutoff-env) 20))
        snd  (lpf (sin-osc (+ (env-gen:kr osc-env) 20)) 200)
        mixed (* (+ noiz snd) (env-gen amp-env :action FREE))]
    (* 6 mixed)))

(definst hat []
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 0.7 src env)))

(definst bass [freq 50 volume 1.0 wah 1.0 fatness 0.0]
  (let [son (saw freq)
        detuned-saws (* fatness (saw (* freq [0.99 1.01])))
        son (+ son detuned-saws)
        son (* son wah)
        son (clip:ar son 0 1)]
    (* volume son)))

(definst beep [freq 1100]
  (let [src (sin-osc freq)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 2 src env)))

(def bass1 (bass 50 0))

(def bass2 (bass 75 0))

(def bass3 (bass (* 4/3 50) 0))

(def bass4 (bass 100 0))

(def metro (metronome 500))

(comment
  ;; you can change the metronome with:
  (metro :bpm 400)
  )

(defn sequencer [beat m]
  (at (metro beat)
      (let [mod-beat (mod beat 8)]
        (when-not (zero? (led-activation m mod-beat 0))
          (dub-kick))
        (when-not (zero? (led-activation m mod-beat 1))
          (hat))
        (if (zero? (led-activation m mod-beat 2))
          (ctl bass1 :volume 0)
          (ctl bass1 :volume 1)
          )
        (when-not (zero? (led-activation m mod-beat 3))
          (beep 1000))
        (if (zero? (led-activation m mod-beat 4))
          (ctl bass2 :volume 0)
          (ctl bass2 :volume 1)
          )
        (if-not (zero? (led-activation m mod-beat 5))
          (ctl bass3 :volume 1)
          (ctl bass3 :volume 0))
        (when-not (zero? (led-activation m mod-beat 6))
          (beep (* 6/5 1000)))
        (if-not (zero? (led-activation m mod-beat 7))
          (ctl bass4 :volume 1)
          (ctl bass4 :volume 0))))
  
  (apply-at (metro (inc beat)) #'sequencer (inc beat) [m]))

(def bass-sequencer-insts
  (atom nil))

(def bass-degrees
  [:i :ii :iii :iv :v :vi :vii :i+ :ii+ :iii+ :iv+ :v+ :vi+ :vii+ :i++])

;; a bit ugly with respect to concurrency design -- doing nil checking
;; separately from reseating the atom
(defn set-bass-scale [offset scale root]
  (let [degrees (take 8 (drop offset bass-degrees))
        pitches (degrees->pitches degrees scale root)]
    (if (nil? @bass-sequencer-insts)
      (reset! bass-sequencer-insts
              (vec (map
                    (fn [pitch] (bass (midi->hz pitch) 0))
                    pitches)))
      (doseq [[inst pitch] (map vector @bass-sequencer-insts pitches)]
        (ctl inst :freq (midi->hz pitch))))))

(defn bass-sequencer [beat poly]
  (at (metro beat)
      (let [mod-beat (mod beat 8)
            leds     (led-activation-col poly mod-beat)]
        (doseq [[inst activation] (map vector @bass-sequencer-insts leds)]
          (if (zero? activation)
            (ctl inst :volume 0)
            (ctl inst :volume 1)))))
  
  (apply-at (metro (inc beat)) #'bass-sequencer (inc beat) [poly]))

(defn init-polynome-normal [m]
  (stop)
  (remove-all-callbacks m)
  (on-press m "foo" (fn [x y s]
                      (toggle-led m x y)))
  (sequencer (metro) m)
  )

(defn init-polynome-bass [poly]
  (stop)
  (reset! bass-sequencer-insts nil)
  (remove-all-callbacks poly)
  (on-press poly "foo" (fn [x y s]
                      (toggle-led poly x y)))
  (set-bass-scale 0 :minor :a2)
  (bass-sequencer (metro) poly)
  )



