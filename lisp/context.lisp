(in-package :pup)

;; catch-all
(defclass pup-catch-all ()
  ((context :initarg :context :accessor context-of)
   (freq :initarg :freq :accessor freq-of)
   (res-fn :initarg :res-fn :accessor res-fn-of)))

(defun make-catch-all (context freq res-fn)
  (make-instance 'pup-catch-all
                 :context context
                 :freq freq
                 :res-fn res-fn))

(defun find-catch-all (key session)
  (assoc key (session-value 'context-catch-all session) :test #'equal))

(defun add-catch-all (key catch-all context freq session)
  (push `(,key  . ,(make-catch-all context freq catch-all))
        (session-value 'context-catch-all session)))

(defun remove-catch-all (key session)
  (setf (session-value 'context-catch-all session)
        (remove key (session-value 'context-catch-all session) :key #'car)))

;; pup-context
(defclass pup-context ()
  ((id :initarg :id :accessor id-of)
   (ttl :initarg :ttl :accessor ttl-of :initform nil)
   (rules :initarg :rules :accessor rules-of)
   (catch-all-hook :initarg :catch-all-hook
                   :accessor catch-all-hook-of
                   :initform nil)
   (catch-all-freq :initarg :catch-all-freq :accessor catch-all-freq-of)
   (input-hook :initarg :input-hook :accessor input-hook-of)
   (session :initarg :session :accessor session-of)))

(defun find-context (key session)
  (assoc key (session-value 'context-tree session) :test #'equal))

(defun add-context (key context session)
  (setf (session-of context) session)
  (push `(,key  . ,context) (session-value 'context-tree session))
  (when (catch-all-hook-of context)
    (add-catch-all key (catch-all-hook-of context) context
                   (catch-all-freq-of context) session))
  context)

(defun remove-context (key session)
  (when (find-catch-all key session)
    (remove-catch-all key session))
  (setf (session-value 'context-tree session)
        (remove key (session-value 'context-tree session)
                :key #'car
                :test #'equal)))

(defun refresh-context (key context session)
  (remove-context key session)
  (add-context key context session))

(defun clear-context (session)
  (setf (session-value 'context-tree session) '())
  (setf (session-value 'context-catch-all session) '()))

;; place-holder
;; rule return options can be strings or fns
(defparameter *base-rules*
  `((((%* %x) name (%* %y))
     "i am not interested in names")
    (((%* %x) blog (%* %y))
     ,#'(lambda (bindings context)
          (declare (ignorable bindings context))
          (blog)))))

(defparameter *base-catch-all*
  (lambda (input context)
    (declare (ignorable input context))
    "I'm a sea-dragon, not an eel-cat!"))

(defparameter *base-catch-all-freq* 50)

;; machinations
(defun context-init (session)
  (let ((base-context (make-base-context)))
    (add-context 'base-context base-context session)
    (add-blog-catch-all session base-context)))

(defun make-base-context ()
  (make-instance 'pup-context
                 :id 'base-context
                 :rules (lambda () *base-rules*)
                 :catch-all-hook (lambda () *base-catch-all*)
                 :catch-all-freq *base-catch-all-freq*))

(defun reset-context (session)
  (clear-context session)
  (context-init session))

(defun eliza-grok (line session)
  "Respond to user input using pattern matching rules."
  (let* ((input (line-to-eliza (url-decode (url-encode line))))
         (context (session-value 'context-tree session))
         (output (find-response input context)))
    (or output (get-catch-all line session))))

(defun find-response (input context-tree)
  (loop for (key . context) in context-tree
        do (let* ((rules (funcall (rules-of context)))
                  (output (use-eliza-rules input rules context)))
             (if output
                 (return output)))))

(defun get-catch-all (input session)
  (let ((catch-all (choose-catch-all (session-value 'context-catch-all session))))
    (if catch-all
        (funcall (funcall (res-fn-of catch-all)) input (context-of catch-all))
        "no catch all")))

(defun choose-catch-all (catch-alls)
  (when catch-alls
    (let* ((tot (loop for c in catch-alls
                      sum (freq-of (cdr c))))
           (rnd (random tot)))
      (loop with i = 0
            for c in catch-alls
            do (progn
                 (setf i (+ i (freq-of (cdr c))))
                 (if (< rnd i)
                     (return (cdr c))))
            finally (return nil)))))

(defun load-contexts ()
  (cl-fad:walk-directory
   (cave "content/context")
   (lambda (file)
     (load file))
   :test (lambda (file)
           (and (equal (pathname-type file)
                       "lisp")
                (not (find #\# (namestring file)))))))

