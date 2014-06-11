;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :pup
  :description "Pups at sea"
  :depends-on (:alexandria :cl-ppcre :cl-interpol :cl-fad
               :split-sequence :local-time :cl-who :rss
               :hunchentoot :html-template :cl-json
               :3bmd :3bmd-ext-wiki-links)
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "mp2eliza")
   (:file "blog")
   (:file "context")
   (:file "rss")
   (:file "main")))
