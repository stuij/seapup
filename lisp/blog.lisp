(in-package :pup)

(defparameter *blog-posts* '())
(defparameter *year-list* '())

(defmacro bind-eliza-vars (vars bindings &body body)
  (let ((letforms (loop for v in vars
                        collect `(,v (get-eliza-var ,(symbol>string-downcase v) ,bindings)))))
    `(let ,letforms
       ,@body)))

(defun get-eliza-var (var bindings)
  (rest (assoc var bindings :test 'string-equal)))

(defun blog (bindings)
  (declare (ignorable bindings))
  (let ((min-items (min (length *blog-posts*) 5)))
    (if (> min-items 0)
        (concatenate 'string
                     "The latest posts, as far as I can tell. Have fun I guess.. If they wouldn't all be so dreary:
</br>"
                     (print-posts (subseq *blog-posts* 0 min-items)))
        "Got no blog posts for ya..")))

(defun print-posts (posts)
  (let* ((out ""))
    (loop for post in posts
          do (setf out (concatenate 'string out
                                    (get-post-summary post))))
    out))

(defun get-post-summary (post)
  (let* ((title (post-title post))
         (link-cmd (concatenate 'string "blog post " title)))
    (concatenate 'string "<br/>_-" title "-__-"  link-cmd "-_")))

(defun make-year-list ()
  (setf *year-list* '())
  (loop for post in *blog-posts*
        do (let* ((y (timestamp-year (get-post-date post))))
             (push-year-list y post))))

(defun push-year-list (key val)
  (let ((lst (assoc key *year-list*)))
    (if lst
        (setf (cdr (assoc key *year-list*)) (append (cdr lst) (list val)))
        (setf *year-list* (acons key (list val) *year-list*)))))

(defun print-just-years ()
  (let ((years (loop for y in *year-list*
                     collect (format nil "_-~A-__-blog year ~A-_" (car y) (car y)))))
    (format nil "Blog years (pick one):</br></br>
~{~A~^</br>~}" years)))

(defun print-blog-year-posts (bindings)
  (bind-eliza-vars (%y) bindings
    (let ((year (parse-integer (print-with-spaces %y) :junk-allowed t)))
      (if year
          (print-posts (cdr (assoc year *year-list*)))
          "Sorry, found no posts for that year.."))))

(defun blog-post (bindings)
  (bind-eliza-vars (%y) bindings
    (print-post (find-post (print-with-spaces %y)))))

(defun print-post (post)
  (if post
      (post-body post)
      "Sorry, couldn't find your post."))

(defun find-post (msg)
  (loop for post in *blog-posts*
        when (post-match-p post msg)
          return post))

(defun post-match-p (post msg)
  (let ((title (remove-punctuation (post-title post))))
    (string-equal title msg)))

(defun load-posts ()
  (setf *blog-posts* '())
  (cl-fad:walk-directory (cave "content") #'parse-post
                         :test (lambda (path)
                                 (string-equal (pathname-type path) "post"))
                         :directories nil)
  (setf *blog-posts*
        (sort *blog-posts* #'timestamp<
              :key #'get-post-date))
  nil)

(defun get-post-date (post)
  (if (post-published post)
      (post-published post)
      (post-created post)))

;; post
(defstruct post
  created
  published
  format
  type
  tags
  title
  body
  comments
  path)

(defun parse-post (path)
  (with-open-file (stream path)
    (let ((post (make-post)))
      (setf (post-path post) path)
      (loop for line = (read-line stream nil)
            while line do (parse-post-line post line))
      (setf *blog-posts* (append *blog-posts* (list post))))))

(defun parse-post-line (post line)
  (if (post-body post)
      (setf (post-body post)
            (concatenate 'string (post-body post) "
</br>" line))
      (parse-post-item post line)))

(defun parse-post-item (post line)
  (loop for i = 0 then (1+ j)
        as j = (position #\space line :start i)
        do (let ((item (switch ((subseq line i j) :test 'string-equal)
                         ("created:" 'parse-created)
                         ("format:" 'parse-format)
                         ("type:" 'parse-type)
                         ("title:" 'parse-title)
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

(defun parse-format (post tail)
  (when tail
    (setf (post-format post) tail)))

(defun parse-type (post tail)
  (when tail
    (setf (post-type post) tail)))

(defun parse-tags (post tail)
  (when tail
    (setf (post-tags post) tail)))

(defun parse-title (post tail)
  (when tail
    (setf (post-title post) tail)))

(defun parse-body-head (post tail)
  (if tail
      (setf (post-body post) tail)
      (setf (post-body post) "")))

(load-posts)
(make-year-list)