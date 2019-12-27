;;;; player.lisp
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(in-package :sonic-lisp)

(defparameter *player-general-vals*
  '((:normal     . (:gravity      0.21875
                    :accel        0.046875
                    :air-accel    0.09375
                    :friction     0.046875
                    :decel        0.5
                    :max-x-spd    12.0
                    :top-x-spd    6.0
                    :jump-str     6.5
                    :min-jump     4.0
                    :air-drag     0.96875
                    :drag-min-x   0.0125
                    :drag-min-y   4.0
                    :slope-factor 0.125
                    :roll-frict   0.0234375
                    :roll-decel   0.125
                    :roll-top-x   16.0
                    :roll-min-x   1.03125
                    :unroll-min-x 0.046875))
    (:super      . ())
    (:speedshoes . ())))

(defparameter *cur-state-vals*
  (rest (assoc :normal *player-general-vals*)))

(defmacro get-state-val (key)
  `(getf *cur-state-vals* ,key))

(defclass player ()
  ((%anim :initarg  :animator
          :accessor animator
          :initform nil)
   (%pos  :initarg  :position
          :accessor player-pos
          :initform (gamekit:vec2 0 0))
   (%dir  :accessor direction
          :initform 1)
   (%gspd :accessor player-gspd
          :initform 0)
   (%spd  :initarg  :speed
          :accessor player-spd
          :initform (gamekit:vec2 0 0))
   (%ang  :accessor player-angle
          :initform 0)
   (%sta  :accessor state
          :initform :none)
   (%grnd :accessor ground
          :initform t)))

(defgeneric update-player-anim (player dt))
(defgeneric update-player-action (player dt))
(defgeneric update-player-movement (player dt))
(defgeneric update-player-position (player dt))
(defgeneric draw-player (player))

(defmethod update-player-anim ((player player) dt)
  (update-animation (animator player) dt)
  ;; Change animations accordingly
  (let ((xspd (abs (gamekit:x (player-spd player)))))
    (setf (animation (animator player))
          (if (ground player)
              ;; Case: on ground
              (case (state player)
                (:none (cond ((= xspd 0)    :idle)
                             ((< xspd 5.9)  :walk)
                             ((< xspd 9.95) :run)
                             (t :super-run)))
                (:look-up  :look-up)
                (:crouch   :crouch)
                (:skid     :skid)
                (:spindash :roll) ; wip
                (:roll     :roll)
                (otherwise :keep))
              ;; Case: on air
              (case (state player)
                ((:jump :roll) :roll)
                (otherwise :keep))))))

(defmethod draw-player ((player player))
  (gamekit:with-pushed-canvas ()
    ;; Position
    (gamekit:translate-canvas
     (gamekit:x (player-pos player))
     (gamekit:y (player-pos player)))
    (gamekit:with-pushed-canvas ()
      ;; Direction
      (gamekit:scale-canvas (direction player) 1)
      ;; Animation frame
      (draw-animation (animator player)
                      ;; Hotspot
                      (gamekit:vec2 -30 -30)))))

(defgeneric update-player-ground-action (player dt))
(defgeneric update-player-air-action (player dt))

(defmethod update-player-ground-action ((player player) dt)
  (let ((xsp (gamekit:x (player-spd player)))
        (ysp (gamekit:y (player-spd player)))
        (state (state player)))
    (declare (ignore ysp))
    (cond
      ;; Crouch down, look up
      ((and (eq state :none)
            (= xsp 0)
            (or (pressing-p :down)
                (pressing-p :up)))
       (setf (state player)
             (cond ((pressing-p :down) :crouch)
                   ((pressing-p :up) :look-up)
                   (t state))))
      ;; Reset crouch down/look up
      ((and (member state '(:crouch :look-up))
            (not (pressing-p :up))
            (not (pressing-p :down)))
       (setf (state player) :none))
      ;; Jump
      ((and (not (member state '(:crouch :spindash)))
            (pressed-p :a))
       (gamekit:play-sound :sfx-jump)
       (incf (gamekit:y (player-spd player))
             (get-state-val :jump-str))
       (setf (ground player) nil
             (state player)  :jump))
      ;; Skidding
      ((and (eq state :none)
            (or (and (> xsp (get-state-val :decel))
                     (pressing-p :left))
                (and (< xsp (- (get-state-val :decel)))
                     (pressing-p :right))))
       (setf (state player) :skid)
       ;; Sound effect only if at greater speeds
       (unless (<= (abs xsp) 3.0)
         (gamekit:play-sound :sfx-skidding)))
      ;; Skidding cancel when stopped skidding or
      ;; when changing directions
      ((and (eq state :skid)
            (or (and (not (pressing-p :left))
                     (not (pressing-p :right)))
                (= xsp 0)))
       (setf (state player) :none))
      ;; Spindash
      ((and (eq state :crouch)
            (pressed-p :a))
       (gamekit:play-sound :sfx-spindash)
       (setf (state player) :spindash))
      ;; Spindash release
      ;; TODO: Add revolutions!
      ((and (eq state :spindash)
            (not (pressing-p :down)))
       (gamekit:play-sound :sfx-release)
       (setf (state player)
             :roll
             (gamekit:x (player-spd player))
             (* 8 (direction player)))) ; wip. Missing rev
      ;; Uncurl
      ((and (eq state :roll)
            (< (abs xsp)
               (get-state-val :unroll-min-x)))
       (setf (state player) :none
             (gamekit:x (player-spd player)) 0)))))

(defmethod update-player-air-action ((player player) dt)
  (let ((xsp (gamekit:x (player-spd player)))
        (ysp (gamekit:y (player-spd player)))
        (state (state player)))
    (declare (ignore xsp))
    (cond
      ;; Short jump
      ((and (eq state :jump)
            (not (pressing-p :a))
            (> ysp (get-state-val :min-jump)))
       (setf (gamekit:y (player-spd player))
             (get-state-val :min-jump))))))

(defmethod update-player-action ((player player) dt)
  (if (ground player)
      (update-player-ground-action player dt)
      (update-player-air-action player dt)))

(defmethod update-player-movement ((player player) dt)
  (let ((xsp (gamekit:x (player-spd player)))
        (ysp (gamekit:y (player-spd player)))
        (groundp (ground player))
        (state (state player)))
    ;; Acceleration
    (unless (member state '(:look-up  :crouch
                            :spindash :skid))
      (when (or (pressing-p :left)
                (pressing-p :right))
        (setf (direction player)
              (if (pressing-p :left) -1 1))
        (incf (gamekit:x (player-spd player))
              (* (get-state-val :accel) 90.0 dt
                 (direction player)))))
    ;; Deceleration
    (when (or (and groundp
                   (not (or (pressing-p :left)
                            (pressing-p :right))))
              ;; Also apply when skidding
              (eq state :skid))
      ;; Deceleration is stronger for skidding.
      ;; We also pre-calculate according to the direction
      ;; we're moving to (may not be the one we're facing)
      (let* ((decel-factor (if (eq state :skid) 60 10))
             (decel-val (* (if (> xsp 0) -1 1)
                           (get-state-val :decel)
                           decel-factor
                           dt)))
        (setf (gamekit:x (player-spd player))
              ;; Instead of crossing the 0.0 middle mark,
              ;; perform a full stop
              (if (<= (abs xsp) (get-state-val :decel))
                  0.0
                  (+ xsp decel-val)))))
    ;; Gravity
    (unless groundp
      (setf (gamekit:y (player-spd player))
            (- ysp (* (get-state-val :gravity)
                      60
                      dt))))))

(defmethod update-player-position ((player player) dt)
  (declare (ignore dt))
  ;; Apply deltas
  (incf (gamekit:x (player-pos player))
        (gamekit:x (player-spd player)))
  (incf (gamekit:y (player-pos player))
        (gamekit:y (player-spd player))))

(defmethod update-player ((player player) dt)
  (update-player-anim     player dt)
  (update-player-action   player dt)
  (update-player-movement player dt)
  (update-player-position player dt))

(defun create-player (&optional (where (gamekit:vec2 0 0)))
  (let ((player
         (make-instance 'player :position where)))
    ;; Initialize animator and animations for Sonic.
    ;; Needs changing later.
    (setf (animator player)
          (make-instance 'animator :atlas :sonic-sprites))
    ;; Register default animations
    (mapcar (lambda (prop)
              (let ((loopback (or (fourth prop) 0))
                    (tpf (or (third prop) 0.16)))
                (register-animation (animator player)
                                    :name (first prop)
                                    :keyframes (second prop)
                                    :time-per-frame tpf
                                    :loopback-index loopback)))
            '((:idle    (0 0 0 0 0 0 0 0 0 0 1 2 3 3 4 4) 0.24 12)
              (:walk    (5 6 7 8 9 10) 0.12)
              (:run     (11 12 13 14) 0.12)
              (:roll    (15 16 17 18 19 20 21 22) 0.12)
              (:skid    (23))
              (:super-run (24 25 26 27) 0.08)
              (:push    (28 29 30 31) 0.48)
              (:crouch  (32))
              (:look-up (33))
              (:death   (34))))
    ;; Set defaults
    (setf (animation (animator player)) :idle)
    player))
