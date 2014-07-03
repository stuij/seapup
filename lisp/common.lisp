(in-package :pup)

(defun punctuation-p (char) (find char ".,;:!'*?#-()\\\""))

(defun remove-punctuation (str)
  (remove-if #'punctuation-p str))

(defun format-downcase (control &rest vars)
  (let ((*print-case* :downcase))
    (apply #'format nil control vars)))

(defun symbol>string-downcase (symbol)
  (format-downcase "~A" symbol))

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

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

(defun print-with-spaces (list)
  (format-downcase "~{~A~^ ~}" list))

(defun cave (&optional (path ""))
  (merge-pathnames
   (concatenate 'string "../" path)
   (directory-namestring (asdf:component-pathname (asdf:find-system :pup)))))

(defun logfile (name)
  (merge-pathnames name (cave "volatile/logs/")))

(defun logs (id format &rest args)
  (apply #'tbnl:log-message* id format args))

(defun dbg (&rest vals)
  (logs :debug "~{~A~^, ~}" vals))

(defun err (&rest vals)
  (if *debug*
      (error "~{~A~^, ~}" vals)
      (apply #'dbg "error: " vals)))

(defun www (name)
  (merge-pathnames name (cave "www/")))

(defun load-config ()
  (let ((file (cave "volatile/config/config.lisp")))
    (if (probe-file file)
        (load file)
        (error "Seapup config file doesn't exist. Please copy seapup/lisp/code/config-example.lisp into seapup/volatile/config/config.lisp and edit appropriately."))))

(defun cmd-link (cmd label)
  (let* ((trail (format nil "?input=~A~{&session=~A~}"
                        (escape-for-html (url-encode cmd)) 
                        (when (and (boundp 'hunchentoot:*session*)
                                   (session-value 'set-session))
                          (list (session-cookie-value *session*))))))
    (site-link label trail "termLink")))

(defun site-link (label trail class)
  (format nil "<a class='~A' href='~A'>~A</a>"
          class
          (site-href trail)
          label))

(defun site-href (trail)
  (let ((port (if (eql *link-port* 80)
                  ""
                  (format nil ":~A" *link-port*))))
    (format nil "~A://~A~A/~A" *link-protocol* *link-host* port trail)))

(defun img-link (img desc)
  (register-groups-bind (dirs file)
      ("(.*/)(.*)" (strcat "static/img/content/" img))
    (let* ((img-trail-site  (strcat dirs "site/" file))
           (img-trail-orig  (strcat dirs "orig/" file))
           (href (site-href img-trail-site))
           (img (format nil "<img class='site-img' src='~A' alt='~A' />"
                        href
                        desc)))
      (format nil "
<div class='img-container'>
  <div class='img-div'>~A</div>
  <div class='img-txt'>~A</div>
</div>"
              (site-link img img-trail-orig "img-link")
              desc))))


(defun get-input ()
  (or (post-parameter "input")
      (get-parameter "input")))


;; parsing and replacing output strings

;; Markdown for all markup but links.
;; It's a bit calculation-intensive, so we try to do as much
;; as possible as early as possible
(defun md (txt)
  (with-output-to-string (str)
    (3bmd:parse-string-and-print-to-stream txt str)))

(defmacro md-pre (txt)
  "Since this Markdown is so expensive, just pre-render common format strings
   at compile-time."
  (with-output-to-string (str)
    (3bmd:parse-string-and-print-to-stream txt str)))

;; regex replacing for links. Costs little time, and we can't
;; predict in advance if we for example need to add session strings.
(defun lrep (str)
  "replace wiki links"
  (regex-replace-all "\\[\\[([^|]*)\\|([^\\]]*)\\]\\]" str
                     #'(lambda (match &rest regs)
                         (declare (ignorable match))
                         (let ((label (first regs))
                               (link  (second regs))
                               (scanner (ppcre:create-scanner
                                         ".*\.(jpg|png|gif)$"
                                         :case-insensitive-mode t)))
                           (if (ppcre:scan scanner link )
                               (img-link link label)
                               (cmd-link link label))))
                     :simple-calls t))


(defun md-rep (txt)
  (lrep (md txt)))




;; make 3bmd handle pup-style wiki-like links (depreciated)
(defclass pup-md () ())

(defmethod 3bmd-wiki::process-wiki-link ((p pup-md) nt ft args stream)
  (declare (ignorable p))
  (let ((link (first args))
        (scanner (ppcre:create-scanner ".*\.(jpg|png|gif)$"
                                       :case-insensitive-mode t)))
    (if (ppcre:scan scanner link)
        (format stream (img-link link ft))
        (format stream (cmd-link link ft)))))


#++ (with-output-to-string (s)
      (3bmd:parse-string-and-print-to-stream "[[bla bla|blog post]]" s))


#|
;; log5 is a bit heavy for now. If things ever get complex, We know where to go

(log5:defcategory eliza)
(log5:defcategory blog)
(log5:defcategory app (and blog eliza))

(defun start-app-logging ()
  (log5:start-sender 'app-log 
                     (log5:stream-sender :location *app-log-pathname*)  
                     :category-spec '(app)  
                     :output-spec '(log5:time log5:category log5:message)))

(start-app-logging)
|#
