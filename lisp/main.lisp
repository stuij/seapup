(in-package :pup)

(load-config)

(defparameter *acceptor* nil)
(defparameter *last-session* nil)

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
  (load-config)
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
        (session (start-puppy-session)))
    (with-output-to-string (*default-template-output*)
      (fill-and-print-template (cave "templates/main.tpl")
                               `(:accept-i18ns ,locales
                                 :debug ,(json:encode-json-to-string *debug-js*))))))

(defun handle-ajax ()
  (let ((json-data (hunchentoot:raw-post-data :force-text t)))
    (json-rpc:invoke-rpc json-data)))

(defun start-puppy-session ()
  (let ((session (start-session)))
    (unless (session-value 'context-tree session)
      (context-init session))
    (when *debug*
      (setf *last-session* session))
    session))

(json-rpc:defun-json-rpc eliza :guessing (data)
  (let ((session (start-puppy-session)))
    (eliza-fun data session)))

(defun eliza-fun (data session)
  (eliza-grok data session))
