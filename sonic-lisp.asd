;;;; sonic-lisp.asd
;;;; Copyright (c) 2018-2020 Lucas Vieira <lucasvieira@protonmail.com>
;;;; This file is distributed under the MIT License.
;;;; See LICENSE for details.

(asdf:defsystem #:sonic-lisp
  :description "Clone of Sonic The Hedgehog's Engine, written in Common Lisp"
  :author "Lucas S. Vieira <lucasvieira@protonmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit)
  :components ((:file "package")
               (:file "globals")
               (:file "input")
               (:file "animations")
               (:file "debugger")
               (:file "player")
               (:file "main")))

(asdf:defsystem #:sonic-lisp/bundle
  :description "Bundles sonic-lisp into a standalone executable"
  :author "Lucas S. Vieira <lucasvieira@protonmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit/distribution
               #:sonic-lisp)
  :components ((:file "bundle")))
