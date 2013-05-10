(in-package :pup)

(make-random-state)

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

(defun pr (&rest vals)
  (format t "~{~a: ~a~%~}~%" vals))

(defun cave (&optional (path ""))
  (merge-pathnames
   (concatenate 'string "../" path)
   (directory-namestring (asdf:component-pathname (asdf:find-system :pup)))))

(defvar *logging-root*)

(defun logfile (name)
  (merge-pathnames name (cave "volatile/logs/")))

(defun www (name)
  (merge-pathnames name (cave "www/")))

(defun load-config ()
  (let ((file (cave "volatile/config/config.lisp")))
    (if (probe-file file)
        (load file)
        (error "Seapup config file doesn't exist. Please copy seapup/lisp/code/config-example.lisp into seapup/volatile/config/config.lisp and edit appropriately."))))

(load-config)


(log5:defcategory server)
(log5:defcategory ajax-call)
(log5:defcategory net-call (and server ajax-call))

(defun start-connection-logging ()
  (log5:start-sender
   'app-log 
   (log5:stream-sender  :location *app-log-pathname*)  
   :category-spec '(net-call)  
   :output-spec '(log5:time log5:category log5:message)))

(defun start-server-logging ()
  (log5:start-sender
   'general
   (log5:stream-sender  :location *standard-output*)  
   :category-spec '(log5:warn+)  
   :output-spec '(log5:time log5:category log5:message)))

(start-server-logging)