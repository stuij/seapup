(in-package :pup)

(defparameter *puppy-session* "session")
(defparameter *last-acceptor* nil)
(defparameter *last-ssl-acceptor* nil)
(defparameter *last-session* nil)
(defparameter *last-request* nil)
(defparameter *bot-scanner* (create-scanner "google|msnbot|wget|curl|baiduspider|bingbot|rambler|yahoo|AbachoBOT|accoona|AcioRobot|ASPSeek|Dumbot|GeonaBot|Gigabot|Lycos|MSRBOT|Scooter|AltaVista|IDBot|eStyle|Scrubby|ia_archiver|Sogou web spider|Twitterbot|MJ12bot|facebookexternalhit"
                                            :case-insensitive-mode t))

(defparameter *dummy-session* nil)

(defun make-dummy-session ()
  (let ((session (make-instance 'no-state-session)))
    (with-slots (hunchentoot::session-data) session
      (setf hunchentoot::session-data
            (make-no-state-session-data session)))
    session))

(defclass no-state-session (session)
  ((hunchentoot::session-id     :initform "0:000000")
   (hunchentoot::session-string :initform "dummy")
   (hunchentoot::user-agent     :initform "dummy")
   (hunchentoot::remote-addr    :initform "0.0.0.0")
   (hunchentoot::session-start  :initform "0")
   (hunchentoot::last-click     :initform "0")
   (hunchentoot::session-data)))

(defun make-no-state-session-data (session)
  (let ((base-context (make-base-context)))
    (setf (session-of base-context) session)
    (list (cons 'set-session  nil)
          (cons 'context-tree
                (list (cons 'base-context base-context)))
          (cons 'context-catch-all
                (list (cons 'base-context
                            (cons (catch-all-hook-of base-context)
                                  base-context))))
          (cons 'crawler t))))

(defun pup-init ()
  (make-random-state)
  (load-config)
  (cl-interpol:enable-interpol-syntax)
  ;; (setf 3bmd-wiki:*wiki-links* t)
  ;; (setf 3bmd-wiki:*wiki-processor* (make-instance 'pup-md))
  (setf cl-who:*downcase-tokens-p* nil)
  (setf *rewrite-for-session-urls* nil)
  (setf *session-max-time* (* 60 60 4))
  (reparse-content)
  (load-contexts)
  (setf *dummy-session* (make-dummy-session)))

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
    ;;(should-redirect)
    #++(redirect (request-uri*)
                 :protocol (if (equal *link-protocol* "https")
                               :https
                               :http)
                 :host *link-host*
                 :port *link-port*
                 :code +http-moved-permanently+)
    (let* ((session (start-puppy-session (post-parameter "session")))
           (input (or (get-input)
                      "hiya"))
           (output (eliza-grok input session)))
      (add-conversation-sliver session input output)
      (with-output-to-string (*default-template-output*)
        (fill-and-print-template
         (cave "templates/main.tpl")
         `(:accept-i18ns ,locales
           :debug        ,(json:encode-json-to-string *debug-js*)
           :input-url    ,(format nil "/?~A=~A" *puppy-session*
                               (session-cookie-value session))
           :welcome      ,(lrep "Confused? Type [[help|help]]")
           :input        ,input
           :output       ,output))))))

(defun add-conversation-sliver (session input output)
  (when *log-conversations*
    (let* ((file (get-conversation-file session))
           (first (not (cl-fad:file-exists-p file))))
      (with-output-to-file (strm file :if-exists :append
                                      :if-does-not-exist :create)
        (when first
          (prin1 (list (referer) (remote-addr *request*) (referer) (request-uri *request*)
                       (host) (user-agent) (when (cookie-in "session") t)) strm)
          (format strm "~%"))
        (prin1 (list input output) strm)
        (format strm "~%")))))

(defun get-conversation-file (session)
  (let* ((sid (hunchentoot::session-string session))
         (time (local-time:universal-to-timestamp (session-start session))))
    (format nil "~A~A_~A"
            (cave "volatile/conversations/")
            (format-timestring nil time :format +conversation-format+)
            sid)))

(defun botp (&optional (request *request*))
  (scan *bot-scanner* (user-agent request)))

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
  (let ((input (cdr (assoc :input data))))
    ;; bit of a dirty hack here, but it's so silly to not use a perfectly
    ;; sane mechanism. Sign this one up to HTML's wonky mix of communication means
    (with-slots (get-parameters post-parameters) *request*
      (push `("input" . ,input) get-parameters))
    (let* ((session (start-puppy-session (cdr (assoc :session data))))
           (result (eliza-grok input session)))
      (add-conversation-sliver session input result)
      `((,*puppy-session* . ,(session-cookie-value session))
        (output . ,result)))))

(defun start-puppy-session (post-session-id)
  (when (botp)
    (when *debug*
      (setf *last-session* *dummy-session*)
      (setf *last-request* *request*))
    (return-from start-puppy-session *dummy-session*))
  (when (and post-session-id
             (not (session *request*)))
    (with-slots (get-parameters session)
        *request*
      (push `(,*puppy-session* . ,post-session-id) get-parameters)
      (setf session (hunchentoot::session-verify *request*))))
  (let* ((session (start-session))
         (session-cookie (cookie-in (session-cookie-name
                                     *acceptor*) *request*)))
    (unless (session-value 'context-tree session)
      (context-init session))
    (if (or post-session-id session-cookie)
        (setf (session-value 'set-session) nil)
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
