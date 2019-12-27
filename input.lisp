;;;; input.lisp
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(in-package :sonic-lisp)

(defvar +input-template+
  '(:up nil :down nil :left nil :right nil
    :start nil :select nil
    :a nil :b nil :x nil :y nil))

(defparameter *input-unsync* (copy-list +input-template+))
(defparameter *input-sync*   (copy-list +input-template+))
(defparameter *input-old*    (copy-list +input-template+))

(defun pressing-p (key)
  (getf *input-sync* key))

(defun pressed-p (key)
  (and (getf *input-sync* key)
       (not (getf *input-old* key))))

(defun update-input ()
  (setf *input-old*  (copy-list *input-sync*)
        *input-sync* (copy-list *input-unsync*)))

(defmacro make-button-bindings (alist)
  `(progn
     ,@(loop for (button key) in alist
          collect `(gamekit:bind-button
                    ,key :pressed
                    (lambda ()
                      (setf (getf *input-unsync* ,button) t)))
          collect `(gamekit:bind-button
                    ,key :released
                    (lambda ()
                      (setf (getf *input-unsync* ,button) nil))))))

(defun make-default-bindings ()
  (make-button-bindings
   ((:up :up) (:down :down) (:left :left) (:right :right)
    (:start :enter) (:select :backspace)
    (:a :s) (:b :d) (:x :a) (:y :w))))
