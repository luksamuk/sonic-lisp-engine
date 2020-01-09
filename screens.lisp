;;;; screens.lisp
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(in-package :sonic-lisp)

(defclass screen-manager (standard-class)
  ((menu       :initform nil)
   (foreground :initform nil)
   (playarea   :initform nil)
   (background :initform nil)
   (loadingp   :initform nil)
   (load-list  :initform nil)))

(defmethod closer-mop:validate-superclass
    ((class screen-manager) (superclass standard-class))
  t)

(defgeneric screen-update  (screen))
(defgeneric screen-draw    (screen))
(defgeneric screen-dispose (screen))

(defgeneric screen-remove  (screen))

(defmethod screen-update ((screen screen-manager))
  (loop for screen-list
     in (mapcar (lambda (x) (slot-value screen x))
                '(background playarea foreground menu))
     do (loop for screen in screen-list
           if (initialized-p screen)
           do (screen-update screen)
           else if (not (init-polled-p screen))
           do (progn
                (format t "Initializing ~a~%" screen)
                (setf (slot-value screen '%pollp) t)
                (screen-load screen)))))

(defmethod screen-draw ((screen screen-manager))
  (loop for screen-list
     in (mapcar (lambda (x) (slot-value screen x))
                '(background playarea foreground menu))
     do (loop for screen in screen-list
           when (initialized-p screen)
           do (screen-draw screen))))

(defun screen-manager-flag-loaded (resource-list)
  (let ((loaded-screen nil)
        (pending-screens (slot-value (find-class 'screen)
                                     'load-list)))
    (setf (slot-value (find-class 'screen) 'load-list)
          (loop for screen in pending-screens
             if (equal (resource-list screen) resource-list)
             do (setf loaded-screen screen)
             else collect screen))
    (pop (slot-value (find-class 'screen) 'loadingp))
    (when loaded-screen
      (screen-init loaded-screen)
      (setf (slot-value loaded-screen '%initp) t))))

(defmethod screen-dispose ((screen screen-manager))
  (loop for screen-list
     in (mapcar (lambda (x) (slot-value screen x))
                '(background playarea foreground menu))
     do (loop for screen in screen-list
           do (screen-remove screen))))

(defun screen-manager-update ()
  (screen-update (find-class 'screen)))

(defun screen-manager-draw ()
  (screen-draw (find-class 'screen)))

(defun screen-manager-dispose ()
  (screen-dispose (find-class 'screen)))

(defun screen-manager-loadingp ()
  (not (null (slot-value (find-class 'screen) 'loadingp))))

(defclass screen ()
  ((%layer     :initarg :layer
               :reader  layer-of)
   (%resources :initform nil
               :reader resource-list)
   (%initp     :reader initialized-p
               :initform nil)
   (%pollp     :reader init-polled-p
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
(defgeneric screen-init (screen))

(defmethod screen-attach-resource ((screen screen) resource)
  (unless (member resource (slot-value screen '%resources))
    (push resource (slot-value screen '%resources))
    (when (initialized-p screen)
      (gamekit:prepare-resources resource))))

;; (defmethod screen-update ((screen screen))
;;   (format t "Default update on ~a, layer ~a~%"
;;           "Default Screen" (layer-of screen)))

(defmethod screen-load ((screen screen))
  (unless (initialized-p screen)
    (push screen (slot-value (find-class 'screen) 'load-list))
    (push t (slot-value (find-class 'screen) 'loadingp))
    (apply #'gamekit:prepare-resources (resource-list screen))))

(defmethod screen-remove ((screen screen))
  (apply #'gamekit:dispose-resources (resource-list screen))
  (let ((slot-name (case (layer-of screen)
                     (:menu       'menu)
                     (:foreground 'foreground)
                     (:playarea   'playarea)
                     (:background 'background)
                     (otherwise
                      (error "Unknown screen layer ~S"
                             (layer-of screen))))))
    (setf (slot-value (find-class 'screen) slot-name)
          (remove screen (slot-value (find-class 'screen) slot-name)))))
