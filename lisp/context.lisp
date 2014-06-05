(in-package :pup)

;; catch-all
(defun find-catch-all (key session)
  (assoc key (session-value 'context-catch-all session) :test #'equal))

(defun add-catch-all (key catch-all context session)
  (push `(,key  . ,(cons catch-all context))
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
   (input-hook :initarg :input-hook :accessor input-hook-of)
   (session :initarg :session :accessor session-of)))

(defun find-context (key session)
  (assoc key (session-value 'context-tree session) :test #'equal))

(defun add-context (key context session)
  (setf (session-of context) session)
  (push `(,key  . ,context) (session-value 'context-tree session))
  (when (catch-all-hook-of context)
    (add-catch-all key (catch-all-hook-of context) context session)))

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

(defun load-contexts ()
  (cl-fad:walk-directory
   (cave "content/context")
   (lambda (file)
     (load file))
   :test (lambda (file)
           (equal (pathname-type file)
                  "lisp"))))

;; place-holder
;; rule return options can be strings or fns
(defparameter *base-rules*
  `((((%* %x) name (%* %y))
     "i am not interested in names")
    (((%* %x) blog (%* %y))
     ,#'(lambda (bindings context)
          (declare (ignorable context))
          (blog bindings)))))

(defparameter *base-catch-all*
  (lambda (input context)
    (declare (ignorable input context))
    "I'm a sea-dragon, not an eel-cat!"))

;; machinations
(defun context-init (session)
  (load-contexts)
  (let ((base-context (make-instance 'pup-context
                                     :id 'base-context
                                     :rules *base-rules*
                                     :catch-all-hook *base-catch-all*)))
    (add-context 'base-context base-context session)))

(defun reset-context (session)
  (clear-context session)
  (context-init session))

(defun eliza-grok (line session)
  "Respond to user input using pattern matching rules."
  (let* ((input (line-to-eliza line))
         (context (session-value 'context-tree session))
         (output (find-response input context)))
    (or output
        (get-catch-all line session))))

(defun find-response (input context-tree)
  (loop for (key . context) in context-tree
        do (let* ((rules (rules-of context))
                  (output (use-eliza-rules input rules context)))
             (if output
                 (return output)))))

(defun get-catch-all (input session)
  (let ((catch-alist (cdr (random-elt
                           (session-value 'context-catch-all session)))))
    (funcall (car catch-alist) input (cdr catch-alist))))