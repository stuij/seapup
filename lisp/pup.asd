;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :pup
  :description "Pups at sea"
  :depends-on (:alexandria :cl-ppcre :cl-fad :hunchentoot :html-template :split-sequence :log5 :cl-json)
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "mp2eliza")
   (:file "eliza-rules")
   (:file "blog")
   (:file "action-rules")
   (:file "main")))
