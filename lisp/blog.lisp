(in-package :pup)

;; general
(defun blog ()
  (let* ((out ""))
    (loop for pos from 0 to 5
          do (setf out (concatenate 'string out
                                    (let ((post (nth pos *blog-posts*)))
                                      (if post
                                          (let ((title (post-title post)))
                                            (concatenate 'string "<br/>_-" title "-__-" (concatenate 'string "blog post " title) "-_")))))))
    (format nil "The latest posts, as far as I can tell. Have fun I guess.. If they wouldn't all be so dreary:
</br>
~A" out)))

(defparameter *blog-posts* '())

(defun load-posts ()
  (setf *blog-posts* '())
  (cl-fad:walk-directory (cave "content") #'parse-post
                         :test (lambda (path)
                                 (string-equal (pathname-type path) "post"))
                         :directories nil)
  (setf *blog-posts*
        (sort *blog-posts* #'string-greaterp :key (lambda (post)
                                                 (if (post-published post)
                                                     (post-published post)
                                                     (post-created post)))))
  nil)


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
    (setf (post-created post) tail)))

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
