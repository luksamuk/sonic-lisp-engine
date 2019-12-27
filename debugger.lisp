;;;; debugger.lisp
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(in-package :sonic-lisp)

(defmacro debug-text-draw (string position)
  `(gamekit:draw-text ,string ,position
                      :fill-color +white+
                      :font *font-gohu-11*))

(defmacro with-debug-panel (&body fields)
  `(gamekit:with-pushed-canvas ()
     (gamekit:translate-canvas 10 340)
     ;; Panel background
     (gamekit:draw-rect
      (gamekit:vec2 -10 ,(* -10 (length fields)))
      (gameprop :window-width)
      ,(+ (* 10 (length fields)) 20)
      :fill-paint +black-transp+
      :stroke-paint +black-transp+)
     ;; Text fields
     ,@(let ((curr-y 0))
         (loop for field in fields
            collect
              `(debug-text-draw
                (format nil ,(first field)
                        ,@(rest field))
                (gamekit:vec2 0 ,curr-y))
            do (decf curr-y 10)))))
