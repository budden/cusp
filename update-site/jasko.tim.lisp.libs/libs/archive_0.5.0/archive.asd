;;; -*- mode: lisp -*-
(defpackage :archive-system
  (:use :cl :asdf))
(in-package :archive-system)

(defsystem :archive
  :version "0.5.0"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :description "A package for reading and writing archive (tar, cpio, etc.) files."
  :depends-on (#+sbcl sb-posix trivial-gray-streams flexi-streams)
  :components ((:file "package")
               (:file "generics" :depends-on ("package"))
               (:file "macros" :depends-on ("generics"))
               (:file "formats" :depends-on ("macros"))
               (:file "stream" :depends-on ("package"))
               (:file "archive" :depends-on ("generics" "stream"))
               #+(and lispworks win32)
               (:file "stat")
               (:file "compat" :depends-on ("package"))
               (:file "tar" :depends-on ("compat" "formats" "generics" "archive"))
               (:file "cpio" :depends-on ("compat" "formats" "generics" "archive"))
               (:static-file "README")
               (:static-file "TODO")
               (:static-file "NEWS")
               (:static-file "LICENSE")))
