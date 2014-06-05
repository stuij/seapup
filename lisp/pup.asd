;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :pup
  :description "Pups at sea"
  :depends-on (:alexandria :cl-ppcre :cl-interpol :cl-fad
               :split-sequence :local-time :cl-who :rss
               :hunchentoot :html-template :cl-json :3bmd)
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "mp2eliza")
   (:file "blog")
   (:file "action-rules")
   (:file "context")
   (:file "rss")
   (:file "main")))
