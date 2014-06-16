(in-package :pup)

(defparameter *puppy-session* "session")
(defparameter *last-acceptor* nil)
(defparameter *last-ssl-acceptor* nil)
(defparameter *last-session* nil)
(defparameter *last-request* nil)

(defun pup-init ()
  (make-random-state)
  (load-config)
  (cl-interpol:enable-interpol-syntax)
  (setf 3bmd-wiki:*wiki-links* t)
  (setf 3bmd-wiki:*wiki-processor* (make-instance 'pup-md))
  (setf cl-who:*downcase-tokens-p* nil)
  (setf *rewrite-for-session-urls* nil)
  (reparse-content))

(defclass puppy-acceptor (easy-acceptor) ())
(defmethod session-cookie-name ((acceptor puppy-acceptor))
  *puppy-session*)

(defclass puppy-ssl-acceptor (easy-ssl-acceptor) ())
(defmethod session-cookie-name ((acceptor puppy-ssl-acceptor))
  *puppy-session*)

(defun start-server* (acceptor &key ssl)
  (let ((last-acceptor (if ssl
                           *last-ssl-acceptor*
                           *last-acceptor*))
        (message-log (if ssl
                         *ssl-message-log-pathname*
                         *message-log-pathname*))
        (access-log (if ssl
                        *ssl-access-log-pathname*
                        *access-log-pathname*)))
    (when (and last-acceptor
               (= (acceptor-port acceptor) (acceptor-port last-acceptor)))
      (cerror "Restarting server." "Server already running on port ~D."
              (acceptor-port last-acceptor))
      (stop last-acceptor))
    (setf (acceptor-message-log-destination acceptor) message-log)
    (setf (acceptor-access-log-destination acceptor)  access-log)
    (format t "Listening on ~A:~D.~%"
            (or (acceptor-address acceptor) "0.0.0.0") 
            (acceptor-port acceptor))
    (if ssl
        (setf *last-ssl-acceptor* (start acceptor))
        (setf *last-acceptor* (start acceptor)))))

(defun start-server (&key (host *host*) (port *port*))
  (start-server* (make-instance 'puppy-acceptor :address host :port port)
                 :ssl nil))

(defun start-ssl-server (&key (host *host*) (port *ssl-port*)
                           (key *ssl-private-key*) (cert *ssl-certificate*)
                           (pass *ssl-pass*))
  (start-server* (make-instance 'puppy-ssl-acceptor
                                :ssl-privatekey-file key
                                :ssl-certificate-file cert
                                :ssl-privatekey-password pass
                                :address host
                                :port port)
                 :ssl t))

(defun stop-server ()
  (unless *last-acceptor* (error "No server running."))
  (stop *last-acceptor*)
  (setf *last-acceptor* nil))

(defun stop-ssl-server ()
  (unless *last-ssl-acceptor* (error "No server running."))
  (stop *last-ssl-acceptor*)
  (setf *last-ssl-acceptor* nil))

#- (and) (defun start-buildapp (argv)
           (declare (ignorable argv))
           (teclo:startup :program-name "server" :save-backtrace t :exit-code 1)
           (start-server)  
           (bt:join-thread (hunchentoot::acceptor-process
                            (hunchentoot::acceptor-taskmaster *last-acceptor*))))

(defun regex-dispatchers (regexes/handlers)
  (alist-values (map-alist #'create-regex-dispatcher regexes/handlers)))

(defparameter *dispatch-table*
  (regex-dispatchers '(;; page callbacks
                       ("^/$" . handle-main)
                       ("^/static.*" . handle-static)
                       ("^/ajax.*" . handle-ajax)
                       ("^/feed.*" . handle-feed)
                       ("^/.*" . handle-void))))

(defun handle-void ()
  "You have wiggled yourself into the crack between space and time. And then you fell through. Without wanting to I'm sure. But now you're here.. FOREVER!!")

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
        (locales (get-locales)))
    (if (should-redirect)
        (redirect (request-uri*)
                  :protocol (if (equal *link-protocol* "https")
                                :https
                                :http)
                  :host *link-host*
                  :port *link-port*
                  :code +http-moved-permanently+)
        (let ((session (start-puppy-session (or (post-parameter "session")
                                                (get-parameter "session"))))
              (input (or (post-parameter "input")
                         (get-parameter "input")
                         "hiya")))
          (with-output-to-string (*default-template-output*)
            (fill-and-print-template
             (cave "templates/main.tpl")
             `(:accept-i18ns ,locales
               :debug ,(json:encode-json-to-string *debug-js*)
               :input-url "/"
               :session-key ,*puppy-session*
               :session-val ,(session-cookie-value session)
               :welcome ,(eliza-grok "quickly" session)
               :input ,input
               :output ,(eliza-grok input session))))))))

(defun should-redirect ()
  (not
   (and (string-equal (first (split-sequence #\: (host)))
                      *link-host*)
        (eql (local-port*) *link-port*))))

(defun handle-ajax ()
  (let ((json-data (hunchentoot:raw-post-data :force-text t)))
    (json-rpc:invoke-rpc json-data)))

(defun handle-feed ()
  (let ((category (get-parameter "cat")))
    (rss (or category "all"))))

(json-rpc:defun-json-rpc eliza :guessing (data)
  (eliza-fun data))

(defun eliza-fun (data)
  (let* ((session (start-puppy-session (cdr (assoc :session data))))
         (result (eliza-grok (cdr (assoc :input data)) session)))
    `((,*puppy-session* . ,(session-cookie-value session))
      (output . ,result))))

(defun start-puppy-session (session-id)
  (when (and session-id
             (not (session *request*)))
    (with-slots (get-parameters session)
        *request*
      (push `(,*puppy-session* . ,session-id) get-parameters)
      (setf session (hunchentoot::session-verify *request*))))
  (let* ((session (start-session))
         (session-cookie (cookie-in (session-cookie-name
                                     *acceptor*) *request*)))
    (unless (session-value 'context-tree session)
      (context-init session))
    (unless session-cookie
      (setf (session-value 'set-session) t))
    (when *debug*
      (setf *last-session* session)
      (setf *last-request* *request*))
    session))

(defun bind-last-request-context ()
  (setf *request* *last-request*)
  (setf *acceptor* *last-acceptor*)
  (setf *session* *last-session*))

(pup-init)
