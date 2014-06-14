(in-package :pup)

(defparameter *host* nil) ;; nil equates to all connections
(defparameter *port* 4242)
(defparameter *ssl-port* 4433)

(defparameter *link-protocol* "http")
(defparameter *link-host* "192.168.130.138")
(defparameter *link-port* 4242)

(setf *session-max-time* 525600)

;; this in effect muffles the specialized upon error when naked like this
(defmethod maybe-invoke-debugger ((u usocket:timeout-error)))
(defmethod maybe-invoke-debugger ((s sb-int:closed-stream-error)))

;; when set to true, errors are caught and logged by hunchentoot
;; when set to nil, they go to the debugger. So for production,
;; set to to true
(setf *catch-errors-p* t)

;; when set, lisp errors are output to html
(setf *show-lisp-errors-p* nil)

(defparameter *debug* t)
(defparameter *debug-js* t)

;; paths
(defparameter *ssl-private-key* (cave "volatile/certificates/CA.key"))
(defparameter *ssl-certificate* (cave "volatile/certificates/CA.crt"))
(defparameter *ssl-pass* nil)

(defparameter *message-log-pathname* (logfile "pup-message.log"))
(defparameter *access-log-pathname*  (logfile "pup-access.log"))
