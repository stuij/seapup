(require 'asdf)

(defmethod asdf:perform :around ((o asdf:load-op) (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    (#+sbcl sb-ext:invalid-fasl 
     #+allegro excl::file-incompatible-fasl-error
     #+lispworks conditions:fasl-error
     #+cmu ext:invalid-fasl
     #-(or sbcl allegro lispworks cmu) error ()
      (asdf:perform (make-instance 'asdf:compile-op) c)
      (call-next-method))))

(require 'sb-bsd-sockets)

(defun sbcl-sourcedir ()
  (let ((alist (logical-pathname-translations "SYS")))
    (second (assoc "SYS:SRC;**;*.*.*" alist :test #'equal))))

(defun set-sbcl-sourcedir (pathname)
  (let* ((alist (logical-pathname-translations "SYS"))
         (cell (assoc "SYS:SRC;**;*.*.*" alist :test #'equal)))
    (prog1
        (setf (second cell) (merge-pathnames "**/*.*" (truename pathname))))
    (setf (logical-pathname-translations "SYS") alist)))

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       "/home/zeno/")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require 'swank)
(swank:create-server)
(ql:quickload "pup")
(pup:start-server)
(bt:join-thread (hunchentoot::acceptor-process
                 (hunchentoot::acceptor-taskmaster pup::*last-acceptor*)))
