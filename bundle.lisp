;;;; bundle.lisp
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(defpackage #:sonic-lisp.bundle
  (:use #:cl)
  (:export #:deliver))

(in-package :sonic-lisp.bundle)

(defun deliver ()
  (gamekit.distribution:deliver :sonic-lisp
                                'sonic-lisp:sonic-game))
