(in-package :pup)

(make-random-state)
(cl-interpol:enable-interpol-syntax)

(defun punctuation-p (char) (find char ".,;:!*?#-()\\\""))

(defun remove-punctuation (str)
  (remove-if #'punctuation-p str))

(defun format-downcase (control &rest vars)
  (let ((*print-case* :downcase))
    (apply #'format nil control vars)))

(defun symbol>string-downcase (symbol)
  (format-downcase "~A" symbol))

(defun alist-keys (alist)
  "Like HASH-TABLE-KEYS."
  (mapcar #'car alist))

(defun alist-values (alist)
  "Like HASH-TABLE-VALUES."
  (mapcar #'cdr alist))

(defun map-alist (function alist)
  "Map over `alist'. `function' takes the CAR and CDR, and should
   return a new CDR."
  (loop for (car . cdr) in alist
        collect (cons car (funcall function car cdr))))

(defun print-with-spaces (list)
  (format-downcase "~{~A~^ ~}" list))

(defun cave (&optional (path ""))
  (merge-pathnames
   (concatenate 'string "../" path)
   (directory-namestring (asdf:component-pathname (asdf:find-system :pup)))))

(defun logfile (name)
  (merge-pathnames name (cave "volatile/logs/")))

(defun logs (id format &rest args)
  (apply #'tbnl:log-message* id format args))

(defun dbg (&rest vals)
  (logs :debug "~{~A~^, ~}" vals))

(defun www (name)
  (merge-pathnames name (cave "www/")))

(defun load-config ()
  (let ((file (cave "volatile/config/config.lisp")))
    (if (probe-file file)
        (load file)
        (error "Seapup config file doesn't exist. Please copy seapup/lisp/code/config-example.lisp into seapup/volatile/config/config.lisp and edit appropriately."))))

(load-config)


#|
;; log5 is a bit heavy for now. If things ever get complex, We know where to go

(log5:defcategory eliza)
(log5:defcategory blog)
(log5:defcategory app (and blog eliza))

(defun start-app-logging ()
  (log5:start-sender 'app-log 
   (log5:stream-sender :location *app-log-pathname*)  
   :category-spec '(app)  
   :output-spec '(log5:time log5:category log5:message)))

(start-app-logging)
|#
