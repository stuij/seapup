(in-package :pup)

(defparameter *host* nil) ;; nil equates to all connections
(defparameter *port* 4242)
(defparameter *ssl-port* 4333)

(setf *session-max-time* 525600)
(defmethod maybe-invoke-debugger ((u usocket:timeout-error)))

(defparameter *debug* t)
(setf *catch-errors-p* nil)
(setf *show-lisp-errors-p* nil)

;; paths
(defparameter *ssl-private-key* (cave "volatile/certificates/CA.key"))
(defparameter *ssl-certificate* (cave "volatile/certificates/CA.crt"))
(defparameter *ssl-pass* nil)

(defparameter *message-log-pathname* (logfile "pup-message.log"))
(defparameter *access-log-pathname*  (logfile "pup-access.log"))
