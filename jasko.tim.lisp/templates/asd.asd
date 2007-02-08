;;;; ${time}

(defpackage #:${package}-asd
  (:use :cl :asdf))

(in-package :${package}-asd)

(defsystem ${package}
  :name "${package}"
  :version "0.1"
  :components ((:file "defpackage")
               (:file "main" :depends-on ("defpackage")))
  :depends-on ())