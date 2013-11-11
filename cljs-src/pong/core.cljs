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
     :y (/ canvas.height 2)
     :score 0
   }
   :computer {
     :y (/ canvas.height 2)
     :score 0
     :speed 2
   }
   :ball {
     :x  (/ canvas.width 2) 
     :y  (/ canvas.height 2) 
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
  ;(.log js/console "move-mouse called...")
  (let [-e (.-event js/window)]
    (if (nil? e)
      (-> state (assoc-in [:player :y] (.-offsetY -e)))
      (-> state (assoc-in [:player :y] (.-screenY e)))))
  (-> state (assoc-in [:player :y] (dec (.-offsetTop canvas))))
  (if (and (>= (/ (:player-height state) 2) 0)
        (<= (/ (+ (:y (:player state)) (:player-height state)) 2) (:canvas-height state)))
    (-> state (assoc-in [:player :y] (:y (:player state)))))
  state)

(defn set-sound [sound]
  ;(.log js/console sound)
)

(defn play-sound [sound]
  (if (:sound state)
    (try
      (set-sound sound)
      (catch js/Error e
        (if (= (type e) js/ReferenceError)
          (println
            (str "You called a function"
              "that does not exist")))))))

(defn set-velocity-y [func {{player-y :y} :player 
                            {ball-y :y ball-max :maxspeed} :ball 
                            player-height :player-height :as state}]
  (let [player-velocity (/ (- player-y ball-y) (* player-height ball-max))
        ball-velocity (/ (- ball-y player-y) (* player-height ball-max))]
    (if (= func +)
      (-> state (assoc-in [:ball :vy] (func player-velocity)))
      (-> state (assoc-in [:ball :vy] (func ball-velocity)))))
  state)

(defn change-ball-direction [{{ball-y :y} :ball {player-y :y} :player :as state}]
  (cond 
    (> player-y ball-y) (set-velocity-y - state)
    (< player-y ball-y) (set-velocity-y + state))
  (-> state (update-in [:ball :vx] (* -1)))
  state)

(defn draw [{{player-y :y} :player 
             {ball-x :x ball-y :y ball-radius :radius} :ball
             {computer-y :y} :computer 
             canvas-width :canvas-width canvas-height :canvas-height :as state}]

  (if-not (:paused state)
    (let [size 3]

      (.clearRect ctx 0 0 (:canvas-width state) (:canvas-height state))

      (set! (. ctx  -fillStyle) "rgb(64,64,64)")

      (dotimes [y (range 0 canvas-height)]
        (.fillRect (/ canvas-width 2) (* y size) size size))

      (set! (. ctx  -fillStyle) "rgba(128, 128, 128, .8)")

      ;; Left Player
      (.fillRect ctx 0 (- computer-y (/ (:player-height state) 2)) (:player-width state) (:player-height state))

      ;; Right Player
      (.fillRect ctx (- (:canvas-width state) (:player-width state)) (- player-y (:player-height state)) (:player-width state) (:player-height state))
      (set! (. ctx  -fillStyle) "rgba(192,192,192,8)")
      
      ;; Ball
      (.fillRect ctx (- ball-x ball-radius) (- ball-y ball-radius) (* ball-radius 2) (* ball-radius 2)))))

(defn start-game [state]
  (let [$title-screen ($ :#titleScreen)
        $play-screen ($ :#playScreen)]
    (.hide $title-screen)
    (.show $play-screen)))

(defn pause-game [state]
  (let [$pause-button ($ :#pauseButton)]
    (if-not (:paused state)
      (do
        (-> state 
         (assoc-in [:paused] true))
        ;;(.html $pause-button "Continue.")
        )
      (do
        (-> state (assoc-in [:paused] false))
        ;;(.html $pause-button "Pause.")
        )
      )
    )
  state)

(defn intro [state]
  (let [play-button  (.getElementById js/document "playButton")
        pause-button (.getElementById js/document "pauseButton") 
        sound-button (.getElementById js/document "soundButton")]
    (events/listen play-button  "click" #(start-game state))
    (events/listen pause-button "click" #(pause-game state))) 
  state)

(defn computer-up? [{{computer-y :y} :computer 
                     {ball-y :y} :ball player-height :player-height 
                     canvas-height :canvas-height :as state}] 
  (and (< (+ computer-y 20) ball-y) (<= (+ computer-y (/ player-height 2)) canvas-height)))

(defn computer-down? [{{computer-y :y} :computer 
                       {ball-y :y} :ball player-height :player-height :as state}]
  (and  (> (- computer-y 20) ball-y) (>= (- computer-y (/ player-height 2)) 0)))

(defn ball-hit-wall? [{{ball-y :y ball-radius :radius} :ball 
                        canvas-height :canvas-height :as state}]
  (or (> (+ ball-y ball-radius) canvas-height) (< (- ball-y ball-radius) 0)))

(defn bounce-ball [{{ball-y :y ball-radius :radius} :ball 
                    canvas-height :canvas-height :as state}]
  (play-sound :sound-wall)
  (if (<= ball-y ball-radius)
    (-> state
      (assoc-in [:ball :y] ball-radius))
    :else (-> state 
            (assoc-in [:ball :y] (- canvas-height ball-radius))))
  (-> state
    (assoc-in [:ball :vy] (* -1)))
  state)

(defn ball-hit-player? [{{ball-x :x ball-radius :radius} :ball 
                         canvas-width :canvas-width  player-width :player-width :as state}]
  (>= (+ ball-x ball-radius) (- canvas-width player-width)))

(defn player-collide? [{{ball-y :y ball-radius :radius} :ball 
                        {player-y :y}  :player
                        player-height :player-height canvas-height :canvas-height :as state}]
  (and (>= (+ ball-y ball-radius) (- player-y (/ player-height 2))) (<= (+ ball-y ball-radius) (+ (:player (:y state)) (/ player-height 2)))))

(defn ball-x-velocity [func {{vx :vx maxspeed :maxspeed mult :multiplier} :ball :as state}]
  #_(if (<= vx maxspeed) 
    (-> state (update-in [:ball :vx] (func (mult)))))
  state)

(defn ball-player-collide [state]
  (if (player-collide? state) 
    (do 
      (play-sound :sound-right) 
      (->> state 
        (ball-x-velocity inc))))
  (change-ball-direction state)
  state)

(defn ball-hit-computer? [{{ball-x :x ball-radius :radius} :ball player-width :player-width :as state}]
  (<= (- ball-x ball-radius) player-width))

(defn set-score [state]
  (-> state
    (update-in [:player :score] (inc 1)))
  (.innerHTML (.getElementById js/document "playerScore") (:score (:player state)))
  state)

(defn reset-ball [{canvas-width :canvas-width canvas-height :canvas-height :as state}]
  #_(-> state
    (update-in [:ball :x] (/ canvas-width 2))
    (update-in [:ball :y] (/ canvas-height 2))
    (update-in [:ball :vy] (* Math/random (- 4 2))))
  state)

(defn ball-computer-collide [{ {ball-y :y ball-x :x ball-radius :radius ball-vx :vx ball-max :maxspeed} :ball 
                               {computer-y :y} :computer
                               player-height :player-height :as state}]
  (if (and (>= (+ ball-y ball-radius) (+ computer-y (/ player-height 2))) 
           (<= (+ ball-y ball-radius) (+ computer-y (/ player-height 2))))
    (do
      (play-sound :sound-left)
      (if (>= ball-vx (- ball-max))
        (->> state
          (ball-x-velocity dec)))
      (change-ball-direction state))
    (do
      (-> state
        (set-score)
        (reset-ball)))))

(defn move-ball [{{ball-vx :vx ball-vy :vy} :ball :as state} move-amount]
  (.log js/console "move-ball called... ball vx: " ball-vx "ball-vy" ball-vy)
  (-> state
    (assoc-in [:ball :x] (inc (* ball-vx move-amount)))
    (assoc-in [:ball :y] (inc (* ball-vy move-amount))))
  state)

(defn driver [{{computer-y :y} :computer  
               {player-y :y} :player
               {ball-y :y ball-x :y ball-radius :radius} :ball 
               player-height :player-height 
               canvas-height :canvas-height 
               canvas-width  :canvas-width
               computer-speed :computer-speed :as state}]

  (let [date-time (js/Date.)
        game-time (if (> (- date-time last-game-time) 0) (- date-time last-game-time) 0)
        move-amount (if (> game-time 0) (/ game-time 10) 1)]

    (draw
      (cond 
        ;; Move CPU player.
        (computer-up? state) (-> state (update-in [:computer :y] (inc (* computer-speed move-amount))))
        (computer-down? state) (-> state (update-in [:computer :y] (dec (* computer-speed move-amount))))
        
        ;; Change direction of ball when hitting wall.
        (ball-hit-wall? state) (bounce-ball state)
        
        ;; Collision between ball and player.
        (ball-hit-player? state) (ball-player-collide state)
        
        ;; Collision beteween ball and CPU.
        (ball-hit-computer? state) (ball-computer-collide state)
        
        ;; If no conditions are met, Keep ball moving.
        :else (-> state (move-ball move-amount)))))
  state)

(defn load []
  (let [init-state state
        interval   (/ 1000 fps)]
    (events/listen js/window "mousemove" #(move-mouse state %)) 
    (.setTimeout js/window
      (fn game-loop [s]
        (let [state (or s init-state)
              new-state (driver state)]
        (.setTimeout js/window
            #(game-loop new-state)
            interval)))
      interval)))

(defn init []
  (load)
  (-> state 
    (intro)))

(.setTimeout js/window (fn [x] (init)) 0)
