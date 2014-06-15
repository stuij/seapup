(in-package :pup)

(defparameter *blog-posts* '())
(defparameter *year-list* '())
(defparameter *blog-post-context-token* 'blog-post)

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
  summary
  body
  comments
  path)

;; parse
(defun parse-content (path)
  (with-open-file (stream path)
    (let ((post (make-post)))
      (setf (post-path post) path)
      (loop for line = (read-line stream nil)
            while line do (parse-content-line post line))
      (when (post-body post)
        (let* ((body (md (post-body post)))
               (clean-body (string-trim '(#\Space #\Tab #\Newline)
                                        body)))
          (setf (post-body post) clean-body)))
      post)))

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
    (setf (post-deleted post) T)))

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
             (push (parse-content path) comments))))
    (if post
        (progn
          (push post *blog-posts*)
          (when comments
            (setf (post-comments post)
                  (sort comments #'timestamp<
                        :key #'get-post-date))))
        (when comments
          (err "there were comments, but no blog-post in dir " dir)))))

(defun reparse-content ()
  (setf *blog-posts* '())
  (walk-blog-dirs (cave "content/text/blog"))
  (setf *blog-posts*
        (sort *blog-posts* #'timestamp<
              :key #'get-post-date))
  (setf *year-list* '())
  (make-year-list))


;; blogtacular
;; blog summary
(defun blog (bindings)
  (declare (ignorable bindings))
  (let ((min-items (min (length (get-posts)) 5)))
    (if (> min-items 0)
        (strcat "The latest posts, as far as I can tell. Have fun I guess.. If they wouldn't all be so dreary:
<br/>
<br/>
"
                (print-posts (subseq (reverse (get-posts)) 0 min-items)))
        "Got no blog posts for ya..")))

(defun get-post-summary (post)
  (let* ((tit-link (blog-link post))
         (summary (get-summary post))
         (no-comments (length (post-comments post))))
    (format nil "
~A
~A < ... >
<span class='comments'>~R comment~:P</span><br/><br/><br/>"
            tit-link summary no-comments)))

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
  (let* ((body (post-body post))
         (sub (subseq body 0 (position #\newline body))))
    (remove-paragraph sub)))

(defun remove-paragraph (p)
  (register-groups-bind (middle)
      ("<p>(.*)</p>" p)
    middle))


;; years
(defun make-year-list ()
  (loop for post in (get-posts)
        do (let* ((y (timestamp-year (get-post-date post)))
                  (deletedp (post-deleted post)))
             (unless deletedp
               (push-year-list y post)))))

(defun push-year-list (key val)
  (let ((lst (assoc key *year-list*)))
    (if lst
        (setf (cdr (assoc key *year-list*)) (append (cdr lst) (list val)))
        (setf *year-list* (acons key (list val) *year-list*)))))

(defun print-just-years ()
  (let ((years (loop for y in *year-list*
                     collect (cmd-link (format nil "blog year ~A" (car y))
                                       (car y)))))
    (format nil "Blog years (pick one):<br/><br/>
~{~A~^<br/>~}" years)))

(defun print-blog-year-posts (bindings)
  (bind-eliza-vars (%y) bindings
    (let ((year (parse-integer (print-with-spaces %y) :junk-allowed t)))
      (if year
          (print-posts (cdr (assoc year *year-list*)))
          "Sorry, found no posts for that year.."))))


;; single post
(defun blog-post (bindings context)
  (bind-eliza-vars (%y) bindings
    (print-post (find-post %y) context)))

(defun print-post (post context)
  (if post
      (progn
        (add-blog-post-context post (session-of context))
        (print-post-proper post))
      "Sorry, couldn't find your post."))

(defun print-post-proper (post)
  (format nil "
<br/>
<div class='title'>~A</div>
<div class='timestamp'>~A</div>
~A<br/>
~A"
          (post-title post)
          (print-blog-date (post-created post))
          (post-body post)
          (print-comments post)))

(defun print-blog-date (date)
  (format-timestring nil date :format +puppy-date-format+))

(defun print-comments (post)
  (let ((out ""))
    (when (post-comments post)
      (setf out "comments</br>
"))
    (dolist (c (post-comments post) out)
      (setf out (strcat out (print-single-comment c))))))

(defun print-single-comment (post)
  (format nil "
<br/><div class='timestamp'>on ~A ~A said:</div>
~A<br/>"
          (print-blog-date (post-created post))
          (post-author post)
          (regex-replace "<p>" (post-body post) "<p class='comment-head'>")))

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

(defun get-posts ()
  *blog-posts*)

(defun get-post-date (post)
  (if (post-published post)
      (post-published post)
      (post-created post)))

(defun get-posts-by-tag (tag)
  (declare (ignorable tag))
  (get-posts))
