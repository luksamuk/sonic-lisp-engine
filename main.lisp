;;;; main.lisp
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(in-package :sonic-lisp)

(defclass play-screen (screen)
  ((%player :initform nil
            :accessor player))
  (:metaclass screen-manager))

(defmethod screen-init ((screen play-screen))
  (setf (slot-value screen '%player)
        (create-player (gamekit:vec2 100 100)))
  (gamekit:play-sound *level-bgm* :looped-p t))

(defmethod screen-update ((screen play-screen))
  (update-player (slot-value screen '%player) *dt*))

(defmethod screen-draw ((screen play-screen))
  (gamekit:with-pushed-canvas ()
    (mapcar (lambda (img)
              (gamekit:draw-image +origin+ img))
            '(:bg-layer0 :bg-layer1 :bg-layer2
              :bg-layer3 :bg-layer4 :bg-layer5))
    (draw-player (slot-value screen '%player))))

(defun make-play-screen (level-name)
  (declare (ignore level-name)) ; wip
  (let ((screen (make-instance 'play-screen :layer :playarea)))
    (mapc (lambda (resource)
            (screen-attach-resource screen resource))
          `(:sonic-sprites
            ,*level-bgm*
            :sfx-skidding
            :sfx-jump
            :sfx-spindash
            :sfx-release
            :bg-layer0 :bg-layer1 :bg-layer2
            :bg-layer3 :bg-layer4 :bg-layer5))
    screen))

(defclass menu-screen (screen)
  ((%option :initform 0))
  (:metaclass screen-manager))

(defmethod screen-init ((screen menu-screen))
  (declare (ignore screen)))

(defmethod screen-update ((screen menu-screen))
  (when (pressed-p :start)
    (setf *play-screen* (make-play-screen "level06"))
    (screen-remove screen)))

(defmacro menu-text-draw (string position font)
  `(gamekit:draw-text ,string ,position
                      :fill-color +white+
                      :font ,font))

(defmacro with-game-menu ((font padding &key start-y selector) &body fields)
  `(gamekit:with-pushed-canvas ()
     (gamekit:translate-canvas 20 ,start-y)
     ;; Text fields
     ,@(let ((curr-y 0))
         (loop for field in fields
            unless (equal field '(terpri))
            collect
              `(menu-text-draw
                (format nil ,(first field)
                        ,@(rest field))
                (gamekit:vec2 0 ,curr-y)
                ,font)
            do (decf curr-y padding)))))

(defmethod screen-draw ((screen menu-screen))
  (with-game-menu (*font-gohu-11* 10
                   :start-y 300
                   :selector 0)
    ("Sonic Lisp Engine v0.1")
    ("(c) 2020 Lucas S. Vieira")
    (terpri)
    ("Start")
    ("Quit")))

;; (defparameter *player*
;;   (create-player (gamekit:vec2 100 100)))

(defparameter *play-screen* nil)

(defmethod gamekit:post-initialize ((app sonic-game))
  ;; Resource acquisition
  (gamekit:prepare-resources :gohufont) ; first and foremost
  (make-default-bindings))

(defmethod gamekit:notice-resources ((app sonic-game) &rest resource-names)
  (if (member :gohufont resource-names) ; bad! replace!!!
      (progn
        (setf *basic-rendering-ok* t)
        (setf *font-gohu-11* (gamekit:make-font :gohufont 11))
        (screen-load (make-instance 'menu-screen :layer :menu)))
      (progn
        (screen-manager-flag-loaded resource-names)
        (setf *game-start* t))))

(defmethod gamekit:pre-destroy ((app sonic-game))
  (screen-manager-dispose)
  (reset)
  (setf *game-start*         nil
        *basic-rendering-ok* nil))

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
    (when (pressed-p :select)
      (reset))
    (update-delta-time)
    (update-input)
    (screen-manager-update)
    ;; (update-player *player* *dt*)
))

(defun draw-debug-panel ()
  (with-debug-panel
    ;; ("pos >> (~5$, ~5$)"
    ;;  (gamekit:x (player-pos *player*))
    ;;  (gamekit:y (player-pos *player*)))
    ;; ("spd >> (~5$, ~5$)"
    ;;  (gamekit:x (player-spd *player*))
    ;;  (gamekit:y (player-spd *player*)))
    ;; ("sta >> ~a" (state *player*))
    ;; ("ani >> ~a" (anim-name (animator *player*)))
    ("fps >> ~5$" *fps*)
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
  (gamekit:with-pushed-canvas ()
    ;; Screen fit
    (gamekit:scale-canvas (/ (gamekit:viewport-width)
                             (gameprop :window-width))
                          (/ (gamekit:viewport-height)
                             (gameprop :window-height)))
    ;; Black background, regardless
    (gamekit:with-pushed-canvas ()
      (gamekit:draw-rect (gamekit:vec2 0 0)
                         (gameprop :window-width)
                         (gameprop :window-height)
                         :fill-paint +black+
                         :stroke-paint +black+))
    ;; Prototype loading screen
    (when (or (and *basic-rendering-ok*
                   (not *game-start*))
              (screen-manager-loadingp))
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
        (screen-manager-draw)
        ;; (mapcar (lambda (img)
        ;;           (gamekit:draw-image +origin+ img))
        ;;         '(:bg-layer0 :bg-layer1 :bg-layer2
        ;;           :bg-layer3 :bg-layer4 :bg-layer5))
        ;; (draw-player *player*)
        (draw-debug-panel)))))

(defun start ()
  (gamekit:start 'sonic-game))

(defun reset ()
  (let ((player (slot-value *play-screen* '%player)))
    (when player
      (setf (player-pos player) (gamekit:vec2 100 100)
            (state player)      :none
            (player-spd player) (gamekit:vec2 0 0)
            (ground player)     t))))

(defun stop ()
  (gamekit:stop)
  (reset)
  (setf *game-start*         nil
        *basic-rendering-ok* nil))
