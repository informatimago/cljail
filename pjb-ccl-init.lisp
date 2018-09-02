(in-package "COMMON-LISP-USER")

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


(ql:quickload :com.informatimago.common-lisp.interactive)
(use-package  :com.informatimago.common-lisp.interactive.interactive)

(ql:quickload :com.informatimago.common-lisp.ed)
(handler-bind ((package-error (lambda (err)
                                (declare (ignore err))
                                (invoke-restart (find-restart 'ccl::resolve-conflict)))))
  (import 'com.informatimago.common-lisp.ed.ed:ed))

(let ((lispdir (merge-pathnames (make-pathname :directory '(:relative "lisp")) (user-homedir-pathname))))
  (ensure-directories-exist lispdir)
  (cd lispdir))
