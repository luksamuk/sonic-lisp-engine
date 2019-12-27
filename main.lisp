;;;; main.lisp
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(in-package :sonic-lisp)

(defparameter *player*
  (create-player (gamekit:vec2 100 100)))

(defmethod gamekit:post-initialize ((app sonic-game))
  ;; Resource acquisition
  (gamekit:prepare-resources :gohufont) ; first and foremost
  (gamekit:prepare-resources
   :sonic-sprites
   *level-bgm*
   :sfx-skidding
   :sfx-jump
   :sfx-spindash
   :sfx-release
   :bg-layer0 :bg-layer1 :bg-layer2
   :bg-layer3 :bg-layer4 :bg-layer5)
  (make-default-bindings))

(defmethod gamekit:notice-resources ((app sonic-game) &rest resource-names)
  (if (member :gohufont resource-names)
      (progn
        (setf *basic-rendering-ok* t)
        (setf *font-gohu-11* (gamekit:make-font :gohufont 11)))
      (progn
        (gamekit:play-sound *level-bgm* :looped-p t)
        (setf *game-start* t))))

(defun update-delta-time ()
  (let ((current-time (get-internal-real-time)))
    (setf *dt* (/ (- current-time *last-check*)
                  internal-time-units-per-second)
          *last-check* current-time
          *fps* (+ (* *fps* 0.99)
                   (if (= *dt* 0)
                       0
                       (* (/ 1 *dt*)
                          (- 1.0 0.99)))))))

(defmethod gamekit:act ((app sonic-game))
  (when *game-start*
    (update-delta-time)
    (update-input)
    (update-player *player* *dt*)))

(defun draw-debug-panel ()
  (with-debug-panel
    ("pos >> (~5$, ~5$)"
     (gamekit:x (player-pos *player*))
     (gamekit:y (player-pos *player*)))
    ("spd >> (~5$, ~5$)"
     (gamekit:x (player-spd *player*))
     (gamekit:y (player-spd *player*)))
    ("fps >> ~5$" *fps*)
    ("sta >> ~a" (state *player*))
    ("ani >> ~a" (anim-name (animator *player*)))
    ("inp >> (~{~a ~})"
     (loop for (btn sta) on *input-sync*
        by #'cddr
        while (keywordp btn)
        collect (if (not sta)
                    #\Space
                    (case btn
                      (:up     #\↑)
                      (:down   #\↓)
                      (:left   #\←)
                      (:right  #\→)
                      (:start  #\S)
                      (:select #\s)
                      (:a      #\A)
                      (:b      #\B)
                      (:x      #\X)
                      (:y      #\Y)))))))

(defmethod gamekit:draw ((app sonic-game))
  ;; Prototype loading screen
  (when (and *basic-rendering-ok*
         (not *game-start*))
    (gamekit:with-pushed-canvas ()
      (gamekit:draw-rect (gamekit:vec2 0 0)
                         (gameprop :window-width)
                         (gameprop :window-height)
                         :fill-paint +black+
                         :stroke-paint +black+))
    (gamekit:with-pushed-canvas ()
      (gamekit:translate-canvas
       (- (/ (gameprop :window-width) 2) 50)
       (- (/ (gameprop :window-height) 2) 5))
      (gamekit:draw-text "Now Loading"
                         (gamekit:vec2 0 0)
                         :fill-color +white+
                         :font *font-gohu-11*)))
  ;; Game rendering
  (when *game-start*
    (gamekit:with-pushed-canvas ()
      (mapcar (lambda (img)
                (gamekit:draw-image +origin+ img))
              '(:bg-layer0 :bg-layer1 :bg-layer2
                :bg-layer3 :bg-layer4 :bg-layer5))
      (draw-debug-panel)
      (draw-player *player*))))

(defun start ()
  (gamekit:start 'sonic-game))

(defun reset ()
  (setf (player-pos *player*) (gamekit:vec2 100 100)
        (state *player*)      :none
        (player-spd *player*) (gamekit:vec2 0 0)
        (ground *player*)     t))

(defun stop ()
  (gamekit:stop)
  (reset)
  (setf *game-start*         nil
        *basic-rendering-ok* nil))

(defmethod gamekit:pre-destroy ((app sonic-game))
  (reset)
  (setf *game-start*         nil
        *basic-rendering-ok* nil))
