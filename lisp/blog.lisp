(in-package :pup)

(defparameter *blog-posts* '())
(defvar *tmp-blog-posts*)
(defparameter *year-list* '())
(defvar *tmp-year-list*)
(defparameter *blog-post-context-token* 'blog-post)
(defparameter *blog-comment-context-token* 'blog-comment)
(defparameter *comment-sign-context-token* 'comment-sign)
(defparameter *tag-hash* (make-hash-table :test 'equal))

;; blog context
(defparameter *blog-post-rules*
  `((((%* %x) name (%* %y))
     "i am not interested in names")
    (((%* %x) comment (%* %y))
     ,#'(lambda (bindings context)
          (declare (ignorable bindings))
          (format nil "You are about to make a dummy comment on blog post
~A."
                  (post-title (post-of context)))))))

(defclass blog-post-context (pup-context)
  ((post :initarg :post :accessor post-of)))

(defun make-blog-post-context (post id)
  (make-instance 'blog-post-context
                 :post post
                 :id id
                 :rules (lambda () *blog-post-rules*)))

(defun add-blog-post-context (post session)
  (let ((id *blog-post-context-token*))
    (remove-context id session)
    (add-context id (make-blog-post-context post id) session)))

(defclass blog-comment-context (blog-post-context)
  ((comment :initarg :comment :accessor comment-of)))

(defun make-blog-comment-context (post id)
  (make-instance 'blog-comment-context
                 :post post
                 :comment (make-blog-post)
                 :id id
                 :rules (lambda () *blog-comment-rules*)))


(defun add-blog-comment-context (post session)
  (let ((id *blog-comment-context-token*))
    (remove-context id session)
    (add-context id (make-blog-comment-context post id) session)))

(defclass comment-sign-context (blog-comment-context)
  ())

(defun make-comment-sign-context (post comment id)
  (make-instance 'blog-comment-context
                 :post post
                 :comment comment
                 :id id
                 :rules (lambda () *comment-sign-rules*)))

(defun add-comment-sign-context (post comment session)
  (let ((id *comment-sign-context-token*))
    (remove-context id session)
    (add-context id (make-comment-sign-context post comment id) session)))

(defun start-comment (context)
  (let ((comment-context (add-blog-comment-context (post-of context)
                                                   (session-of context))))
    (comment-first comment-context)))

(defun comment-sign-start ()
  (lrep "Write your name, or something like it, and I'll save your comment. Or write [[quit|quit]] or [[redo|redo]]."))

(defun comment-sign-done (context)
  (let* ((session (session-of context))
         (post (post-of context))
         (post-path (post-path post))
         (post-dir (directory-namestring post-path))
         (post-name (post-title post))
         (name (get-input))
         (comment-name (format nil "~A - ~A"
                               post-name name))
         (comment (comment-of context)))
    (setf (post-author comment) name)
    (setf (post-deleted comment) 0)
    (setf (post-format comment) "markdown")
    (setf (post-created comment) (now))
    (write-single-comment comment post-dir comment-name)
    (remove-context *comment-sign-context-token* session)
    (remove-context *blog-comment-context-token* session)
    (finalise-post comment)
    (push comment (post-comments post))
    (send-comment-mail post comment)    
    (format nil "Yay, comment written :)<br/>
Here's the post again plus your comment:<br/>
<br/>
~A"
            (eliza-grok (strcat "blog post " (post-title post)) *session*))))

(defun send-comment-mail (post comment)
  (let ((cmd-string (format nil "mail -s 'new comment from ~A, on post: ~A' ties@stuij.se < /dev/null"
                            (post-author comment)
                            (post-title post))))
    (pup-shell-cmd cmd-string)))

(defun comment-quit (context)
  (remove-context *blog-comment-context-token* (session-of context))
  "Well, I guess that was that. No more commenting. You want a cup of tea?")

(defun comment-sign-quit (context)
  (remove-context *comment-sign-context-token* (session-of context))
  (comment-quit context))

(defun comment-first (context)
  (format nil "So you wanna make a comment on blog post '~A'? Ok, cool :)
</br></br>
~A"
          (post-title (post-of context))
          (show-comment-body)))

(defun comment-done (context)
  (add-comment-sign-context (post-of context)
                            (comment-of context)
                            (session-of context))
  (comment-sign-start))

(defun comment-more (context)
  (let* ((input (get-input))
         (old-body (post-body (comment-of context)))
         (new-body (if (and input (not (equal input "")))
                       (string-trim '(#\Space #\Tab #\Newline)
                                    (format nil "~A~%~%~A"
                                            (or old-body "")
                                            (get-input)))
                       old-body)))
    (when new-body
      (setf (post-body (comment-of context)) new-body))
    (add-comment-sign-context (post-of context)
                              (comment-of context)
                              (session-of context))
    (comment-sign-start)))

(defun comment-redo (context)
  (setf (post-body (comment-of context)) nil)
  (format nil "Ok, we're all clean as a baby's bottom again. Let's try once more.

~A"
          (show-comment-body)))

(defun comment-sign-redo (context)
  (remove-context *comment-sign-context-token* (session-of context))
  (comment-redo context))

(defun show-comment-body ()
  (format nil (lrep (md-pre "

Type your comment or type [[quit|quit]]."))))

;; post
(defstruct post
  created
  published
  deleted
  format
  type
  tags
  title
  author
  email
  summary
  body
  comments
  path)

;; parse
(defun make-blog-post ()
  (make-post :created nil :published nil :deleted nil
             :format nil :type nil :tags nil
             :title nil :author nil :summary nil
             :body nil :comments nil :path nil))

(defun parse-content (path)
  (with-open-file (stream path)
    (let ((post (make-blog-post)))
      (setf (post-path post) path)
      (loop for line = (read-line stream nil)
            while line do (parse-content-line post line))
      (finalise-post post)
      post)))

(defun finalise-post (post)
  (when (post-body post)
    (let* ((body (post-body post))
           (clean-body (string-trim '(#\Space #\Tab #\Newline)
                                    body)))
      (unless (post-summary post)
        (setf (post-summary post)
              (subseq clean-body 0 (position #\newline clean-body))))
      (setf (post-body post) (md clean-body)))))

(defun parse-content-line (post line)
  (if (post-body post)
      (setf (post-body post)
            (strcat (post-body post) "
" line))
      (parse-content-item post line)))

(defun parse-content-item (post line)
  (loop for i = 0 then (1+ j)
        as j = (position #\space line :start i)
        do (let ((item (switch ((subseq line i j) :test 'string-equal)
                         ("created:" 'parse-created)
                         ("deleted:" 'parse-deleted)
                         ("format:" 'parse-format)
                         ("type:" 'parse-type)
                         ("title:" 'parse-title)
                         ("author:" 'parse-author)
                         ("tags:" 'parse-tags)
                         ("summary:" 'parse-summary)
                         ("body:" 'parse-body-head))))
             (when item
               (let ((tail (when j (subseq line (+ j 1)))))
                 (funcall item post tail))
               (return)))
        while j)
  post)

(defun parse-created (post tail)
  (when tail
    (setf (post-created post)
          (parse-timestring tail :date-time-separator #\space))))

(defun parse-deleted (post tail)
  (when (and tail (string-equal tail "1"))
    (setf (post-deleted post) t)))

(defun parse-format (post tail)
  (when tail
    (setf (post-format post) tail)))

(defun parse-type (post tail)
  (when tail
    (setf (post-type post) tail)))

(defun parse-author (post tail)
  (when tail
    (setf (post-author post) tail)))

(defun parse-tags (post tail)
  (when tail
    (setf (post-tags post)
          (split-sequence #\space tail :remove-empty-subseqs t))))

(defun parse-title (post tail)
  (when tail
    (setf (post-title post) tail)))

(defun parse-summary (post tail)
  (when tail
    (setf (post-summary post) tail)))

(defun parse-body-head (post tail)
  (if tail
      (setf (post-body post) tail)
      (setf (post-body post) "")))

(defun walk-blog-dirs (dir)
  (let ((post nil)
        (comments '()))
    (dolist (path (cl-fad:list-directory dir))
      (cond ((cl-fad:directory-pathname-p path)
             (walk-blog-dirs path))
            ((string-equal (pathname-type path) "post")
             (if post
                 (err "There already exists a post in the dir of " path)
                 (setf post (parse-content path))))
            ((string-equal (pathname-type path) "comment")
             (let ((comment (parse-content path)))
               (when (not (post-deleted comment))
                 (push comment comments))))))
    (if (and post (not (post-deleted post)))
        (progn
          (push post *tmp-blog-posts*)
          (when comments
            (setf (post-comments post)
                  (sort comments #'timestamp>
                        :key #'get-post-date))))
        (when (and comments (not post))
          (err "there were comments, but no blog-post in dir " dir)))))

(defun reparse-blog ()
  (let ((*tmp-blog-posts* '()))
    (walk-blog-dirs (cave "content/text/blog"))
    (setf *blog-posts* (sort *tmp-blog-posts* #'timestamp<
                             :key #'get-post-date)))
  nil)

(defun reparse-year-list ()
  (let ((*tmp-year-list* '()))
    (make-year-list)
    (setf *year-list* *tmp-year-list*)))

(defun reparse-content ()
  (reparse-blog)
  (reparse-year-list)
  (make-tag-hash)
  nil)


;; blogtacular
;; blog summary
(defun blog ()
  (let ((min-items (min (length (get-posts)) 3)))
    (if (> min-items 0)
        (format nil (md-pre "
You can browse the blog by ~A:
~A  
Or browse by ~A:  
~A
<br/><br/>

And these are the latest posts, as far as I can tell. Have fun I guess.. If they wouldn't all be so dreary:

~A
")
                (cmd-link "blog tags" "tags")
                (print-all-tags)
                (cmd-link "blog years" "years")
                (print-just-years)
                (print-posts (subseq (reverse (get-posts)) 0 min-items)))
        "Got no blog posts for ya..")))

(defun get-post-summary (post)
  (let* ((date (print-blog-date (post-created post)))
         (tit-link (blog-link post))
         (summary (get-summary post))
         (no-comments (length (post-comments post))))
    (format nil "<span class='timestamp'>~A</span>
~A
~A < ... >
<span class='comments'>~R comment~:P</span><br/><br/><br/>"
            date tit-link summary no-comments)))

(defun print-posts (posts)
  (let* ((out ""))
    (loop for post in posts
          do (setf out (strcat out (get-post-summary post))))
    out))

(defun blog-link (post)
  (let* ((title (post-title post))
         (cmd (strcat "blog post " title)))
    (cmd-link cmd title)))

(defun get-summary (post)
  (lrep (post-summary post)))

(defun get-body (post)
  (lrep (post-body post)))

(defun remove-paragraph (p)
  (register-groups-bind (middle)
      ("<p>(.*)</p>" p)
    middle))

;; tags
(defun make-tag-hash ()
  (let ((hash (make-hash-table :test 'equal)))
    (loop for post in (get-posts)
          do (add-to-tag-hash post hash))
    (setf *tag-hash* hash)))

(defun add-to-tag-hash (post hash)
  (loop for tag in (post-tags post)
        do (let* ((dtag (string-downcase (remove-punctuation tag)))
                  (val (gethash dtag hash)))
             (if val
                 (push post (cdr val))
                 (setf (gethash dtag hash)
                       (cons tag (list post)))))))


(defun print-all-tags ()
  (let ((tags (loop for (tag . posts) being the hash-value in *tag-hash*
                    unless (or (string-equal tag "fallen-frukt")
                               (string-equal tag "jaded-puppy")
                               (string-equal tag "tales-from-the-underbelly"))
                      collect (cmd-link (format nil "blog tag ~A" tag)
                                        (format nil "~A(~A)" tag (length posts))))))
    (lrep (format nil (md-pre "Ex-blogs from other places revamped here for your pleasure:

- [[tales-from-the-underbelly|blog tag tales-from-the-underbelly]] - One Laptop Per Child/Nepal stuff, and some travelly/culturally things from other places - 2008-2011
- [[fallen-frukt|blog tag fallen-frukt]] - Common Lisp programming - 2005-2008
- [[jaded-puppy|blog tag jaded-puppy]] - Jaded puppy's adventures in Sweden - 2005-2007


and the rest:
 ~{<span class='some-spacing'>~A</span>~^ ~}") tags))))

(defun get-posts-by-tag (tag)
  (if (string-equal tag "all")
      (reverse (get-posts))
      (cdr (gethash (string-downcase (remove-punctuation tag)) *tag-hash*))))

(defun print-blog-tag-posts (bindings)
  (bind-eliza-vars (%y) bindings
    (let ((posts (get-posts-by-tag (first %y))))
      (if posts
          (print-posts posts)
          "Sorry, found no posts for that tag.."))))

;; years
(defun make-year-list ()
  (loop for post in (get-posts)
        do (let ((y (timestamp-year (get-post-date post))))
             (push-year-list y post))))

(defun push-year-list (key val)
  (let ((lst (assoc key *tmp-year-list*)))
    (if lst
        (setf (cdr (assoc key *tmp-year-list*)) (append (cdr lst) (list val)))
        (setf *tmp-year-list* (acons key (list val) *tmp-year-list*)))))

(defun print-just-years ()
  (let ((years (loop for y in *year-list*
                     collect (cmd-link (format nil "blog year ~A" (car y))
                                       (format nil "~A*(~A)" (car y) (length (cdr y)))))))
    (format nil "Blog years (pick one): ~{<span class='some-spacing'>~A</span>~^ ~}" years)))

(defun print-blog-year-posts (bindings)
  (bind-eliza-vars (%y) bindings
    (let* ((year (parse-integer (print-with-spaces %y) :junk-allowed t))
           (posts (reverse (cdr (assoc year *year-list*)))))
      (if posts
          (print-posts posts)
          "Sorry, found no posts for that year.."))))


;; single post
(defun blog-post (bindings context)
  (bind-eliza-vars (%y) bindings
    (print-post (find-post %y) context)))

(defun print-post (post context)
  (if post
      (let ((session (session-of context)))
        (when (not (session-value 'crawler session))
          (add-blog-post-context post session))
        (print-post-proper post))
      "Sorry, couldn't find your post."))

(defun print-post-proper (post)
  (format nil "
<br/>
<div class='title'>~A</div>
<div class='timestamp'>~A</div>
<br/>
~A
~A<br/>
~A
<br/>
~A"
          (post-title post)
          (print-tags post)
          (print-blog-date (post-created post))
          (lrep (post-body post))
          (print-comments post)
          (lrep "Want to comment? Type [[comment|comment]].")))

(defun print-tags (post)
  (lrep (format nil "tags: ~{[[~A|blog tag ~:*~A]]~^ ~}" (post-tags post))))

(defun print-blog-date (date)
  (format-timestring nil date :format +puppy-date-format+))

(defun print-comments (post)
  (let ((out ""))
    (when (post-comments post)
      (setf out "comments</br>
"))
    (dolist (c (reverse (post-comments post)) out)
      (setf out (strcat out (print-single-comment c))))))

(defun print-single-comment (post)
  (format nil "
<br/><div class='timestamp'>on ~A ~A said:</div>
~A"
          (print-blog-date (post-created post))
          (post-author post)
          (lrep (post-body post))))

(defun find-post (msg)
  (loop for post in (get-posts)
        when (post-match-p post msg)
          return post))

(defun post-match-p (post msg)
  (let ((title (line-to-eliza (post-title post))))
    (tree-equal title msg :test #'string-equal)))


;; general stuff
(defparameter +puppy-date-format+
  ;; Sun, 06 Nov 1994 08:49:37
  '(:short-weekday ", " (:day 2) #\space
    :short-month #\space (:year 4) #\space
    (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defparameter +post-identifier-format+
  '((:year 4) "-" (:month 2) "-" (:day 2) #\space
    (:hour 2) #\: (:min 2) #\: (:sec 2) "." :nsec))

(defparameter +parsable-timestring-format+
  '((:year 4) "-" (:month 2) "-" (:day 2) #\space
    (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defun get-posts ()
  *blog-posts*)

(defun get-post-date (post)
  (if (post-published post)
      (post-published post)
      (post-created post)))

;; write posts
(defun write-posts (output-root-path post-list)
  (loop for post in post-list
        do (write-single-post post output-root-path)))

(defun write-single-post (post root)
  (multiple-value-bind (post-dir post-file-prefix name)
      (get-post-dir post root)
    (ensure-directories-exist post-dir)
    (write-post-proper post (strcat post-file-prefix ".post"))
    (write-comments post post-dir name)))

(defun write-comments (post post-dir name)
  (loop for c in (post-comments post)
        do (write-single-comment c post-dir name)))

(defun write-single-comment (post post-dir name)
  (let* ((date (format-timestring nil (post-created post)
                                  :format +post-identifier-format+))
         (post-file (strcat (namestring post-dir) date
                            " - " name ".comment")))
    (with-open-file (s post-file
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format s "~
- author: ~A
- created: ~A
- format: ~A
- deleted: ~A
- type: ~A
- email: ~A
- body:
~A"
              (post-author post)
              (format-timestring nil (get-post-created post)
                                 :format +parsable-timestring-format+)
              (post-format post)
              (post-deleted post)
              "blog-comment"
              (or (post-email post) "")
              (post-body post)))))

(defun get-post-created (post)
  (post-created post))

(defun write-post-proper (post post-file)
  (with-open-file (s post-file
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (format s "~
- created: ~A
- format: ~A
- deleted: ~A
- type: ~A
- title: ~A
- tags:~{ ~A~}
- body:
~A"
            (post-created post)
            (post-type post)
            (post-deleted post)
            "blog-post"
            (post-title post)
            (get-post-tags post)
            (post-body post))))

(defun get-post-tags (post)
  (post-tags post))

(defun get-post-dir (post root)
  (let* ((root* (concatenate 'string root "/"))
         (root-path (make-pathname :directory (pathname-directory root*)))
         (title-sanitized (regex-replace-all #\? (post-title post) ""))
         (post-date (post-created post))
         (post-name (concatenate 'string post-date " - " title-sanitized))
         (post-dir (merge-pathnames (concatenate 'string post-name "/") root-path))
         (post-file-prefix (namestring (merge-pathnames post-name post-dir))))
    (assert (probe-file root-path) nil "root path ~A doesn't exist" root-path)
    (values post-dir post-file-prefix title-sanitized)))
