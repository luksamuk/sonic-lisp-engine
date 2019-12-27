;;;; globals.lisp
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(in-package :sonic-lisp)

(defvar +black+  (gamekit:vec4 0 0 0 1))
(defvar +black-transp+ (gamekit:vec4 0 0 0 0.5))
(defvar +white+  (gamekit:vec4 1 1 1 1))
(defvar +cornflower-blue+
  (gamekit:vec4 (/ 100 255)
                (/ 149 255)
                (/ 237 255)))

(defvar +origin+ (gamekit:vec2 0 0))

(defvar *dt*   0)
(defvar *fps* 60)
(defvar *last-check* (get-internal-real-time))

(defparameter *basic-rendering-ok* nil)
(defparameter *game-start* nil)

(defvar *game-properties*
  `(:title "Sonic Engine"
    :window-width  640
    :window-height 360
    :background    ,+cornflower-blue+))

(defmacro gameprop (property)
  `(getf *game-properties* ,property))

(gamekit:defgame sonic-game ()
  ()
  (:viewport-title  (gameprop :title))
  (:viewport-width  (gameprop :window-width))
  (:viewport-height (gameprop :window-height))
  (:prepare-resources nil))

(gamekit:register-resource-package
 :keyword (merge-pathnames "resources/"
                           (asdf:system-relative-pathname
                            :sonic-lisp "")))

(gamekit:define-image :sonic-sprites "sprites/sonic.png")

(gamekit:define-sound :level-music "bgm/level6.ogg")

(gamekit:define-sound :sfx-skidding "sfx/00_skidding.ogg")
(gamekit:define-sound :sfx-jump     "sfx/02_jump.ogg")
(gamekit:define-sound :sfx-spindash "sfx/03_spindash.ogg")
(gamekit:define-sound :sfx-release  "sfx/04_release.ogg")

(gamekit:define-font :gohufont "fonts/gohufont-uni-11.ttf")

(defparameter *font-gohu-11* nil)

(gamekit:define-image :bg-layer0 "bg/level6/parallax/layer0.png")
(gamekit:define-image :bg-layer1 "bg/level6/parallax/layer1.png")
(gamekit:define-image :bg-layer2 "bg/level6/parallax/layer2.png")
(gamekit:define-image :bg-layer3 "bg/level6/parallax/layer3.png")
(gamekit:define-image :bg-layer4 "bg/level6/parallax/layer4.png")
(gamekit:define-image :bg-layer5 "bg/level6/parallax/layer5.png")
