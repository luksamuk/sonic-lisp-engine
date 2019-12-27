;;;; animations.lisp
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(in-package :sonic-lisp)

(defstruct animation-props
  keyframes
  (time-per-frame 0.16 :type single-float)
  (loopback nil))

(defclass animator ()
  ((%atlas      :initarg  :atlas
                :reader   atlas)
   (%atlas-size :initarg  :atlas-size
                :reader   atlas-size
                :initform (gamekit:vec2 360 360))
   (%fpl        :initarg  :frames-per-line
                :reader   frames-per-line
                :initform 6)
   (%curr-anim  :accessor anim-name
                :initform nil)
   (%anim-timer :accessor anim-timer
                :initform 0)
   (%frame      :accessor frame
                :initform 0)
   (%anims      :initarg :animations
                :accessor animations
                :initform nil)))

(defgeneric (setf animation) (animation-name animator))
(defgeneric register-animation
    (animator &key name keyframes time-per-frame loopback-index))
(defgeneric update-animation (animator dt))
(defgeneric draw-animation (animator position))

(defmethod (setf animation) (animation-name (animator animator))
  ;; Only set to a registered animation
  (when (or (eq animation-name :keep)
            (and (animations animator)
                 (gethash animation-name (animations animator))))
    ;; Reset animation data only when not attributing to
    ;; same animation
    (unless (or (eql animation-name (anim-name animator))
                (eq animation-name :keep))
      (setf (frame animator)      0
            (anim-timer animator) 0
            (anim-name animator) animation-name))))

(defmethod register-animation ((animator animator)
                               &key
                                 name
                                 keyframes
                                 (time-per-frame 0.16)
                                 (loopback-index 0))
  (let ((keyframes (make-array (length keyframes)
                               :initial-contents keyframes)))
    ;; Initialize animations table if not initialized
    (unless (animations animator)
      (setf (animations animator) (make-hash-table)))
    (setf (gethash name (animations animator))
          (make-animation-props
           :keyframes keyframes
           :time-per-frame time-per-frame
           :loopback loopback-index))))

(defmethod update-animation ((animator animator) dt)
  (let ((props (gethash (anim-name animator)
                        (animations animator)))
        (tpf nil))
    (when props
      (incf (anim-timer animator) dt)
      ;; If we surpassed the frame duration for the
      ;; animation, calculate the amount of frames
      ;; to skip and then wrap the timer around.
      (setf tpf (animation-props-time-per-frame props))
      (when (>= (anim-timer animator)
                (animation-props-time-per-frame props))
        (let ((frames-skipped
               (floor (/ (anim-timer animator) tpf)))
              (num-frames
               (length (animation-props-keyframes props))))
          ;; Restore timer
          (setf (anim-timer animator)
                (rem (anim-timer animator) tpf))
          ;; Increment current frame
          (incf (frame animator) frames-skipped)
          ;; If beyond last frame, wrap around
          (when (>= (frame animator) num-frames)
            ;; We need to determine at what frame should we
            ;; stop; take the loopback frame into account
            ;; and consider only the [loopback, last-frame]
            ;; range for another remainder operation.
            (let* ((loopback-frame
                      (animation-props-loopback props))
                   (loopback-range (- num-frames loopback-frame)))
              (setf (frame animator)
                    (+ loopback-frame (rem (frame animator)
                                           loopback-range))))))))))

(defmethod draw-animation ((animator animator) (pos gamekit:vec2))
  (let ((props
         (gethash (anim-name animator) (animations animator))))
    (when props
      ;; Take the index of the frame on the keyfranes, then
      ;; convert it to a proper X and Y position on the texture
      ;; atlas
      (let* ((frame
              (aref (animation-props-keyframes props)
                    (frame animator)))
             (frame-x-index
              (rem frame (frames-per-line animator)))
             (frame-y-index
              (floor (/ frame (frames-per-line animator))))
             (frame-size
              (/ (gamekit:x (atlas-size animator))
                 (frames-per-line animator))))
        (gamekit:draw-image
         ;; Position on matrix
         pos
         ;; Pass on animation atlas
         (atlas animator)
         ;; Position on atlas
         :origin
         (gamekit:vec2
          (* frame-x-index frame-size)
          (- (- (gamekit:y (atlas-size animator))
                frame-size)
             (* frame-y-index frame-size)))
         ;; Size of frame square
         :width frame-size
         :height frame-size)))))
