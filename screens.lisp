;;;; screens.lisp
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(in-package :sonic-lisp)

(defclass screen-manager (standard-class)
  ((menu       :initform nil)
   (foreground :initform nil)
   (playarea   :initform nil)
   (background :initform nil)))

(defmethod closer-mop:validate-superclass
    ((class screen-manager) (superclass standard-class))
  t)

(defgeneric screen-update (screen))

(defmethod screen-update ((screen screen-manager))
  (loop for menu in (slot-value screen 'menu)
     do (screen-update menu))
  (loop for fg in (slot-value screen 'foreground)
     do (screen-update fg))
  (loop for playarea in (slot-value screen 'playarea)
     do (screen-update playarea))
  (loop for bg in (slot-value screen 'background)
     do (screen-update bg)))

(defun screen-manager-update ()
  (screen-update (find-class 'screen)))

(defclass screen ()
  ((%layer :initarg :layer
           :reader  layer-of)
   (%resources :reader  resource-list)
   (%initp :reader initialized-p
           :initform nil))
  (:metaclass screen-manager))

(defmethod initialize-instance :after ((obj screen) &key)
  (push obj
        (slot-value (find-class 'screen)
                    (case (layer-of obj)
                      (:menu       'menu)
                      (:foreground 'foreground)
                      (:playarea   'playarea)
                      (:background 'background)
                      (otherwise
                       (error "Unknown screen layer ~S"
                              (layer-of obj)))))))

(defgeneric screen-load (screen))
(defgeneric screen-dispose (screen))

(defgeneric screen-attach-resource (screen resource))

(defmethod screen-attach-resource ((screen screen) (resource keyword))
  (unless (member resource (slot-value screen '%resources))
    (push resource (slot-value screen '%resources))
    (when (initialized-p screen)
      (gamekit:prepare-resources resource))))

(defmethod screen-load ((screen screen))
  (macrolet ((prepare-resource-list (list)
               `(gamekit:prepare-resources ,@list)))
    (unless (initialized-p screen)
      (prepare-resource-list (resource-list screen)))))

(defmethod screen-update ((screen screen))
  (format t "Default update on ~a, layer ~a~%"
          "Default Screen" (layer-of screen)))
