;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File mp2eliza.lisp: Norvig's Eliza code for EECS 492 Machine Problem 2.
;;;    assembled from auxfns.lisp, eliza.lisp, eliza1.lisp, patmatch.lisp,
;;;    and eliza-pm.lisp

;;; auxfns

(in-package :pup)

;;; patmatch: Pattern matcher from section 6.2

(defconstant fail nil "Indicates pat-match failure")

(defparameter no-bindings '((t . t))
  "Indicates pat-match success, with no variables.")

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((or
          (and (null pattern)
               (null input))
          (and (stringp pattern)
               (stringp input)
               (string-equal pattern input)))
         bindings)
        ((segment-pattern-p pattern)                
         (segment-matcher pattern input bindings))  
        ((single-pattern-p pattern)                 ; ***
         (single-matcher pattern input bindings))   ; ***
        ((and (consp pattern) (consp input)) 
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) 
                               bindings)))
        (t fail)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `%')?"
  (and (stringp x) (equal (char x 0) #\%)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings :test 'string-equal))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun make-binding (var val) (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((string-equal input (binding-val binding)) bindings)
          (t fail))))

(defparameter *single-match*
  '(("%is"  . match-is)
    ("%or"  . match-or)
    ("%and" . match-and)
    ("%not" . match-not)))

(defparameter *segment-match*
  '(("%*"  . segment-match)
    ("%+"  . segment-match+)
    ("%%"  . segment-match?)
    ("%if" . match-if)))

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((%* var) . pat)?"
  (and (consp pattern) (consp (first pattern)) 
       (stringp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (%is x predicate) (%and . patterns) (%or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x, 
  if it is a symbol that has one."
  (when (stringp x) (cdr (assoc x *segment-match* :test 'string-equal))))

(defun single-match-fn (x)
  "Get the single-match function for x, 
  if it is a symbol that has one."
  (when (stringp x) (cdr (assoc x *single-match* :test 'string-equal))))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
        fail
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns) 
                                     input bindings)))
        (if (eq new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  (if (match-or patterns input bindings)
      fail
      bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((%* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        (let ((pos (first-match-pos (first pat) input start)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                         pat (subseq input pos)
                         (match-variable var (subseq input 0 pos)
                                         bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  (cond ((and (stringp pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'string-equal))
        ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
        (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
  The pattern looks like ((%if code) . rest)."
  ;; *** fix, rjf 10/1/92 (used to eval binding values)
  (and (progv (mapcar #'car bindings)
           (mapcar #'cdr bindings)
         (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev) 
        (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-pat-match-abbrev (first pat))
                 (expand-pat-match-abbrev (rest pat))))))

(defun rule-based-translator 
    (input rules &key (matcher #'pat-match) 
                   (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in rules that matches input,
  and apply the action to that rule."
  (some
   #'(lambda (rule)
       (let ((result (funcall matcher (funcall rule-if rule) 
                              input)))
         (if (not (eq result fail))
             (funcall action result (funcall rule-then rule)))))
   rules))

;;;;
;;;;  eliza1: Basic version of the Eliza program

;;; ==============================

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defparameter *viewpoint* '(("I" . "you") ("you" . "me") ("me" . "you") ("am" . "are") ("yourself" . "myself") ("your" . "my") ("my" . "your")))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on."
  (sublis *viewpoint*
          words :test (lambda (a b)
                        (when (and (stringp a)
                                   (stringp b))
                          (string-equal a b)))))

;;; ==============================

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
      (list x)))



;;;;
;;;;  eliza: Advanced version of Eliza.
;;; Has more rules, and accepts input without parens.

;;; ==============================

(defun chop-line (line)
  (split-sequence:split-sequence #\space line :remove-empty-subseqs t))

(defun line-to-eliza (line)
  (chop-line (remove-punctuation line)))

(defun first-chop (rule)
  (chop-list (first rule)))

(defun chop-list (thing)
  (first (chop-list* thing)))

(defun chop-list* (thing)
  (cond ((consp thing)
         (list (loop for th in thing
                     append (chop-list* th))))
        ((stringp thing)
         (split-sequence:split-sequence #\space thing))
        ((symbolp thing)
         (list (symbol>string-downcase thing)))
        (t thing)))

(defun reg-match (bind-pair response)
  (let ((result response)) 
    (unless (eql bind-pair no-bindings)
      (loop for (term . replacement) in bind-pair
            do (let ((r (if replacement
                            (print-with-spaces replacement)
                            "")))
                 (setf result
                       (ppcre:regex-replace-all term result r)))))
    result))

(defvar *eliza-rules* nil)

;;;; eliza-pm: use advanced pattern matcher

(defun use-eliza-rules (input rules)
  "Find some rule with which to transform the input."
  (rule-based-translator input rules :rule-if #'first-chop
                                             :action #'eliza-action))

(defun eliza-action (bindings responses)
  (let ((a-response (random-elt responses)))
    (let ((output (typecase a-response
                    (string (reg-match (switch-viewpoint bindings)
                                       a-response))
                    (function (funcall a-response bindings)))))
      (format nil "~A" output))))
