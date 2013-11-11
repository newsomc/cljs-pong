(ns pong.core
  (:require
    [goog.dom :as dom]
    [goog.events :as events])
  (:use [jayq.core :only [$ css html]]))

(def canvas (.getElementById js/document "canvas"))
(def ctx (.getContext canvas "2d"))
(def sound-left (.getElementById js/document "bounce-left"))
(def sound-right (.getElementById js/document "bounce-right"))
(def sound-wall (.getElementById js/document "bounce-wall"))
(def last-game-time (js/Date.))
(def fps 30)
(def state {
   :player {
     :y (/ (.-height canvas) 2)
     :score 0
   }
   :computer {
     :y (/ (.-height canvas) 2)
     :score 0
     :speed 2
   }
   :ball {
     :x  (/ (.-width canvas) 2) 
     :y  (/ (.-height canvas) 2) 
     :vx (if (rand)  1 -1)  
     :vy (* (rand) (- 4 2))
     :bounces  0
     :radius 3
     :multiplier .2
     :maxspeed 5
   }
   :player-height 80
   :player-width 4
   :paused false
   :sound true
   :canvas-width (.-width canvas)
   :canvas-height (.-height canvas)})

(defn move-mouse [state e]
  (let [-e (.-event js/window)]
    (if (nil? e)
      (-> state (assoc-in [:player :y] (.-offsetY -e)))
      (-> state (assoc-in [:player :y] (.-screenY e)))))
  (-> state (assoc-in [:player :y] (dec (.-offsetTop canvas))))
  (if (and (>= (/ (:player-height state) 2) 0)
        (<= (/ (+ (:y (:player state)) (:player-height state)) 2) (:canvas-height state)))
    (-> state (assoc-in [:player :y] (:y (:player state))))))

(defn set-sound [sound])

(defn play-sound [sound]
  (if (:sound state)
    (try
      (set-sound sound)
      (catch js/Error e
        (if (= (type e) js/ReferenceError)
          (println
            (str "You called a function"
              "that does not exist")))))))

(defn set-velocity-y [func {:keys [ball player] :as state}]
  (let [ph (:player-height state)
        player-velocity (/ (- (:y player) (:y ball)) (* ph (:maxspeed ball)))
        ball-velocity (/ (- (:y ball) (:y player)) (* ph (:maxspeed ball) ))]
    (if (= func +)
      (-> state (assoc-in [:ball :vy] (func player-velocity)))
      (-> state (assoc-in [:ball :vy] (func ball-velocity))))))

(defn change-ball-direction [{:keys [ball player] :as state}]
  (cond 
    (> (:y player) (:y ball)) (set-velocity-y - state)
    (< (:y player) (:y ball)) (set-velocity-y + state))
  (-> state (update-in [:ball :vx] (* -1))))

(defn draw [{:keys [player ball computer] :as state}]
  (let [cw (:canvas-width state)
        ch (:canvas-width state)
        ph (:player-height state)
        pw (:player-width state)]

    (if-not (:paused state)
      (let [size 3
            rb (:radius ball)]
        (.clearRect ctx 0 0 cw ch)

        (set! (. ctx  -fillStyle) "rgb(64,64,64)")

        (dotimes [y (range 0 ch)]
          (.fillRect (/ cw 2) (* y size) size size))

        (set! (. ctx  -fillStyle) "rgba(128, 128, 128, .8)")

        ;; Left Player
        (.fillRect ctx 0 (- (:y computer) (/ ph 2)) pw ph)

        ;; Right Player
        (.fillRect ctx (- cw pw) (- (:y player) ph) pw ph)
        (set! (. ctx  -fillStyle) "rgba(192,192,192,8)")

        ;; Ball
        (.fillRect ctx (- (:x ball) rb) (- (:y ball) rb) (* rb 2) (* rb 2))))))

(defn start-game [state]
  (let [$title-screen ($ :#titleScreen)
        $play-screen ($ :#playScreen)]
    (.hide $title-screen)
    (.show $play-screen)))

(defn pause-game [state]
  (let [$pause-button ($ :#pauseButton)]
    (if-not (:paused state)
      (do
        (.html $pause-button "Continue.")
        (-> state 
         (assoc-in [:paused] true)))
      (do
        (.html $pause-button "Pause.")
        (-> state (assoc-in [:paused] false))))))

(defn intro []
  (let [play  (.getElementById js/document "playButton")
        pause(.getElementById js/document "pauseButton") 
        sound (.getElementById js/document "soundButton")]
    (events/listen play "click" #(start-game state))
    (events/listen pause "click" #(pause-game state))))

(defn computer-up? [{:keys [ball computer] :as state}] 
  (let [cy (:y computer)]
    (and (< (+ cy 20) (:y ball)) 
         (<= (+ cy (/ (:player-height state) 2)) (:canvas-height state)))))

(defn computer-down? [{:keys [ball computer] :as state}]
  (let [cy (:y computer)]
    (and (> (- cy 20) (:y ball))
         (>= (- cy (/ (:player-height state) 2)) 0))))

(defn move-computer [state move-amount func]
  (-> state 
    (update-in [:computer :y] (func (* (:computer-speed state) move-amount)))))

(defn ball-hit-wall? [{:keys [ball] :as state}]
  (or (> (+ (:y ball) (:radius ball)) (:canvas-height state)) 
      (< (- (:y ball) (:radius ball)) 0)))

(defn bounce-ball [{:keys [ball] :as state}]
  (play-sound :sound-wall)
  (if (<= (:y ball) (:radius ball))
    (-> state
      (assoc-in [:ball :y] (:radius ball)))
    (-> state 
      (assoc-in [:ball :y] (- (:canvas-height state) (:radius ball)))))
  (-> state
    (assoc-in [:ball :vy] (* -1))))

(defn ball-hit-player? [{:keys [ball] :as state}]
  (>= (+ (:x ball) (:radius ball)) 
      (- (:canvas-width state) (:player-width state))))

(defn player-collide? [{:keys [ball player] :as state}]
  (let [ph (:player-height state)]
    (and (>= (+ (:y ball) (:radius ball)) (- (:y player) (/ ph 2))) 
         (<= (+ (:y ball) (:radius ball)) (+ (:y player) (/ ph 2))))))

(defn ball-x-velocity [func {:keys [ball] :as state}]
  (let [mult (:multiplier ball)]
    (if (<= (:vx ball) (:maxspeed ball)) 
      (-> state 
        (update-in [:ball :vx] (func (mult)))))))

(defn ball-player-collide [state]
  (if (player-collide? state) 
    (do 
      (play-sound :sound-right) 
      (->> state 
        (ball-x-velocity inc))))
  (change-ball-direction state))

(defn ball-hit-computer? [{:keys [ball] :as state}]
  (let [pw (:player-width state)]
    (<= (- (:x ball) (:radius ball)) pw)))

(defn set-score [state]
  (.innerHTML (.getElementById js/document "playerScore") (:score (:player state)))
  (-> state
    (update-in [:player :score] (inc 1))))

(defn reset-ball [state]
  (let [cw (:canvas-width state)
        ch (:canvas-height state)]
    (-> state
      (assoc-in [:ball :x] (/ cw 2))
      (assoc-in [:ball :y] (/ ch 2))
      (assoc-in [:ball :vy] (* Math/random (- 4 2))))))

(defn ball-computer-collide? [{:keys [ball computer] :as state}]
  (let [ph (:player-height state)]
    (and (>= (+ (:y ball) (:radius ball)) (+ (:y computer) (/ ph 2))) 
         (<= (+ (:y ball) (:radius ball)) (+ (:y computer) (/ ph 2))))))

(defn ball-computer-collide [{:keys [ball computer] :as state}]
  (if (ball-computer-collide? state)
    (do
      (play-sound :sound-left)
      (if (>= (:vx ball) (- (:maxspeed ball)))
        (->> state
            (ball-x-velocity dec)))
      (change-ball-direction state))
    (do
      (-> state
        (set-score)
        (reset-ball)))))

(defn move-ball [{:keys [ball] :as state} move-amount]
  (-> state
    (assoc-in [:ball :x] (inc (* (:vx ball) move-amount)))
    (assoc-in [:ball :y] (inc (* (:vy ball) move-amount)))))

(defn driver [state]
  (let [date-time (js/Date.)
        game-time (if (> (- date-time last-game-time) 0) (- date-time last-game-time) 0)
        move-amount (if (> game-time 0) (/ game-time 10) 1)]
    (draw
      (cond 
        ;; Move CPU player.
        (computer-up? state) (-> state (move-computer move-amount inc))
        (computer-down? state) (-> state (move-computer move-amount dec))
        
        ;; Change direction of ball when hitting wall.
        (ball-hit-wall? state) (bounce-ball state)
        
        ;; Collision between ball and player.
        (ball-hit-player? state) (ball-player-collide state)
        
        ;; Collision beteween ball and CPU.
        (ball-hit-computer? state) (ball-computer-collide state)
        
        ;; If no conditions are met keep ball moving.
        :else (-> state (move-ball move-amount))))))

(defn load []
  (let [init-state state
        interval (/ 1000 fps)]
    (events/listen js/window "mousemove" #(move-mouse state %)) 
    (.setTimeout js/window
      (fn game-loop [s]
        (let [state (or s init-state)
              new-state (driver state)]
        (.setTimeout js/window
            #(game-loop new-state)
            interval)))
      interval)))

(defn init [] (load) (intro))

(.setTimeout js/window (fn [x] (init)) 0)
