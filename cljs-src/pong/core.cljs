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
     :multiplier 0.2
     :maxspeed 5
   }
   :player-height 80
   :player-width 4
   :sound true
   :canvas-width (.-width canvas)
   :canvas-height (.-height canvas)
   :tick nil })

;; Stateful stuff
(def paused (atom true))

(def mouse-state (atom nil))

(defn handle-mouse-event [e]
    (reset! mouse-state (.-offsetY e))
    (.preventDefault e)
    (.stopPropagation e))

(defn draw [{:keys [player ball computer] :as state}]
  (let [cw (:canvas-width state)
        ch (:canvas-width state)
        ph (:player-height state)
        pw (:player-width state)]

    (if-not @paused 
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
        (.fillRect ctx (- (:x ball) rb) (- (:y ball) rb) (* rb 2) (* rb 2))))
    state))

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
  (-> state 
    (update-in [:ball :vx] (* -1))))

(defn start-game []
  (let [$title-screen ($ :#titleScreen)
        $play-screen ($ :#playScreen)]
    (do 
      (.hide $title-screen)
      (.show $play-screen))
    (reset! paused false)))

(defn pause-game [state]
  (let [$pause-button ($ :#pauseButton)]
    (if-not @paused
      (do
        (.html $pause-button "Continue.")
        (reset! paused true))
      (do
        (.html $pause-button "Pause.")
        (reset! paused false)))))

(defn intro [state]
  (let [play  (.getElementById js/document "playButton")
        pause (.getElementById js/document "pauseButton") 
        sound (.getElementById js/document "soundButton")
        $play-screen ($ :#playScreen)]
    (.hide $play-screen)
    (.addEventListener play "click" #(start-game))
    state))

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
    (assoc-in [:computer :y] (func (* (:computer-speed state) move-amount)))))

(defn ball-hit-wall? [{:keys [ball] :as state}]
  (or (> (+ (:y ball) (:radius ball)) (:canvas-height state)) 
      (< (- (:y ball) (:radius ball)) 0)))

(defn reverse-ball-direction [state]
  (-> state
    (update-in [:ball :vy] #(* -1 %))))

(defn bounce-ball [{:keys [ball] :as state}]
  (let [new-state (reverse-ball-direction state)]
    (if (<= (:y ball) (:radius ball)) 
      (-> new-state
        (assoc-in [:ball :y] (:radius ball)))
      (-> new-state 
        (assoc-in [:ball :y] (- (:canvas-height new-state) (:radius ball)))))))

(defn ball-hit-player? [{:keys [ball] :as state}]
  (>= (+ (:x ball) (:radius ball)) 
      (- (:canvas-width state) (:player-width state))))

(defn player-collide? [{:keys [ball player] :as state}]
  (let [ph (:player-height state)]
    (and (>= (+ (:y ball) (:radius ball)) (- (:y player) (/ ph 2))) 
         (<= (+ (:y ball) (:radius ball)) (+ (:y player) (/ ph 2))))))

(defn ball-x-velocity [func {:keys [ball] :as state}]
  (.log js/console "called with: "  func)
  (let [mult (:multiplier ball)]
    (if (<= (:vx ball) (:maxspeed ball)) 
      (-> state 
        (update-in [:ball :vx] func mult)))))

(defn ball-player-collide [state]
  (.log js/console (clj->js state))
  (if (player-collide? state) 
    (let [new-state (change-ball-direction state)]
      (ball-x-velocity inc new-state))))

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
      ;;(play-sound :sound-left)
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
    (update-in [:ball :x] + (* (:vx ball) move-amount))
    (update-in [:ball :y] + (* (:vy ball) move-amount))))

(defn move-mouse [state]
  (let [ms @mouse-state]
    (reset! mouse-state nil)
      (if (not= @paused true)
        (assoc-in state [:player :y] ms)
        state)))

(defn driver [state]
  (let [date-time (js/Date.)
        game-time  (if (> (- date-time last-game-time) 0) (- date-time last-game-time) 0)
        move-amount 8
        state (-> (if (= @paused true)
                    state
                    (update-in state [:tick] (fnil inc 0)))
                (move-mouse))]    

    (.log js/console "move amount" move-amount)

    (draw 
      (cond 
        ;; Show the intro screen if paused.
        (= @paused true) (intro state)

        ;; (computer-up? state)   (-> state (move-computer move-amount inc))
    
        ;; (computer-down? state) (-> state (move-computer move-amount dec))
    
        ;; ;; Change direction of ball when hitting wall.
        (ball-hit-wall? state) (-> state bounce-ball)
          
        ;; ;; Collision between ball and player.
        (ball-hit-player? state) (-> state ball-player-collide)
        
        ;; ;; Collision beteween ball and CPU.
        ;; (ball-hit-computer? state) (-> state (ball-computer-collide))
        
        ;; If no conditions are met keep ball moving.      
        :else (if-not @paused (-> state (move-ball move-amount)))))))

(defn loaded []
  (let [init-state state
        interval (/ 1000 fps)]    
    (.addEventListener js/document "mousemove" handle-mouse-event true)
    (.setTimeout js/window
      (fn game-loop [s]
        (let [state (or s init-state)
              new-state (driver state)]
          (.setTimeout js/window
            #(game-loop new-state)
            interval)))
      interval)))

(defn init [] (loaded))
(.setTimeout js/window (fn [x] (init)) 0)
