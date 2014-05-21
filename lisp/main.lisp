(in-package :pup)

(defparameter *acceptor* nil)

(defun start-server* (acceptor)
  (when (and *acceptor* (= (acceptor-port *acceptor*) (acceptor-port acceptor)))
    (cerror "Restart server." "Server already running on port ~D."
            (acceptor-port acceptor))
    (stop *acceptor*))
  (setf (acceptor-message-log-destination acceptor) *message-log-pathname*)
  (setf (acceptor-access-log-destination acceptor)  *access-log-pathname*)
  (format t "Listening on ~A:~D.~%"
          (or (acceptor-address acceptor) "0.0.0.0") 
          (acceptor-port acceptor))
  (setf *acceptor* (start acceptor)))

(defun start-server (&key (host *host*) (port *port*))
  (start-server* (make-instance 'easy-acceptor :address host :port port)))

(defun start-ssl-server (&key (host *host*) (port *ssl-port*)
                           (key *ssl-private-key*) (cert *ssl-certificate*)
                           (pass *ssl-pass*))
  (start-server* (make-instance 'easy-ssl-acceptor
                                :ssl-privatekey-file key
                                :ssl-certificate-file cert
                                :ssl-privatekey-password pass
                                :address host
                                :port port)))

(defun stop-server ()
  (unless *acceptor* (error "No server running."))
  (stop *acceptor*)
  (setf *acceptor* nil))

#- (and) (defun start-buildapp (argv)
           (declare (ignorable argv))
           (teclo:startup :program-name "server" :save-backtrace t :exit-code 1)
           (start-server)  
           (bt:join-thread (hunchentoot::acceptor-process
                            (hunchentoot::acceptor-taskmaster *acceptor*))))


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

(defun regex-dispatchers (regexes/handlers)
  (alist-values (map-alist #'create-regex-dispatcher regexes/handlers)))

(defparameter *dispatch-table*
  (regex-dispatchers '(;; page callbacks
                       ("^/$" . handle-main)
                       ("^/static.*" . handle-static)
                       ("^/ajax.*" . handle-ajax))))

(defun handle-static ()
  (handle-static-pup (subseq (request-uri*) 8))) ;; chop off '/static/'

(defun handle-static-pup (file)
  (handle-static-file (merge-pathnames file (cave "www/"))))

(defun get-locales ()
  (json:encode-json-to-string
   (or (split-sequence #\,
                       (first
                        (split-sequence #\;
                                        (header-in :accept-language *request*))))
       "en-US")))

(defun handle-main ()
  (let ((*string-modifier* #'identity)
        (locales (get-locales))
        (session (start-session)))
    (dbg "static session:" session)
    (with-output-to-string (*default-template-output*)
      (fill-and-print-template (cave "templates/main.tpl")
                               `(:accept-i18ns ,locales
                                 :debug *debug-js*)))))

(defun handle-ajax ()
  (let ((session (start-session))
        (json-data (hunchentoot:raw-post-data :force-text t)))
    (dbg "ajax-session:" session)
    (json-rpc:invoke-rpc json-data)))

(json-rpc:defun-json-rpc eliza :guessing (data)
  (eliza-fun data))

(defun eliza-fun (data)
  (eliza-grok data))
