(in-package :pup)

;; globals
(defparameter *catch-all-actions* ()
  "Choose one at random from these options to serve as reply.")

(defparameter *pup-context-tree* ()
  "From where all pup-thoughts sprout. Is also no programming tree.")

;; catch-all
(defun find-catch-all (key)
  (assoc key *catch-all-actions* :test #'equal))

(defun add-catch-all (key catch-all)
  (push `(,key  . ,catch-all) *catch-all-actions*))

(defun remove-catch-all (key)
  (setf *catch-all-actions* (remove key *catch-all-actions* :key #'car)))

;; pup-context
(defclass pup-context ()
  ((name :initarg :name :accessor name-of)
   (ttl :initarg :ttl :accessor ttl-of :initform nil)
   (rules :initarg :rules :accessor rules-of)
   (catch-all-hook :initarg :catch-all-hook :accessor catch-all-hook-of)
   (input-hook :initarg :input-hook :accessor input-hook-of)))

(defun find-context (key)
  (assoc key *pup-context-tree* :test #'equal))

(defun add-context (key context)
  (when (catch-all-hook-of context)
    (add-catch-all key (catch-all-hook-of context)))
  (push `(,key  . ,context) *pup-context-tree*))

(defun remove-context (key)
  (when (find-catch-all key)
    (remove-catch-all key))
  (setf *pup-context-tree* (remove key *pup-context-tree*
                                   :key #'car
                                   :test #'equal)))

(defun refresh-context (key context)
  (remove-context key)
  (add-context key context))

(defun clear-context ()
  (setf *pup-context-tree* '())
  (setf *catch-all-actions* '()))

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
     ,#'(lambda (bindings)
          (blog bindings)))))

(defparameter *base-catch-all*
  (lambda ()
    "I'm a sea-dragon, not an eel-cat!"))

;; machinations
(defun context-init ()
  (load-contexts)
  (let ((base-context (make-instance 'pup-context
                                     :name "base-context"
                                     :rules *base-rules*
                                     :catch-all-hook *base-catch-all*)))
    (add-context "base-context" base-context)))

(defun reset-context ()
  (clear-context)
  (context-init))

(defun eliza-grok (line)
  "Respond to user input using pattern matching rules."
  (let* ((input (line-to-eliza line))
         (output (find-response input)))
    (or output
        (get-catch-all))))

(defun find-response (input)
  (loop for (car . cdr) in *pup-context-tree*
        do (let* ((rules (rules-of cdr))
                  (output (use-eliza-rules input rules)))
             (if output
                 (return output)))))

(defun get-catch-all ()
  (funcall (cdr (random-elt *catch-all-actions*))))

