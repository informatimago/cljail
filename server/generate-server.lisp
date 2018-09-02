(in-package "COMMON-LISP-USER")

#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(load #P"~/quicklisp/setup.lisp")

(setf *print-right-margin* 80
      *print-pretty* t
      *print-case* :downcase)

(defun dirpath  (path) (make-pathname :name nil   :type nil   :version nil :defaults path))
(defun wildpath (path) (make-pathname :name :wild :type :wild :version nil :defaults path))
(defun fasldir  (system component)
  (first (asdf:output-files
          (make-instance 'asdf:compile-op)
          (asdf:find-component (asdf:find-system system) component))))

(setf *default-pathname-defaults* (dirpath (or *load-truename*
                                               *compile-file-truename*)))
(pushnew *default-pathname-defaults* asdf:*central-registry* :test 'equal)

(defparameter *program-name* "iplwcl-server")
(defparameter *program-system*  :com.informatimago.ipl.server)


;; (let ((dir (funcall (function #+windows wildpath #-windows dirpath)
;;                     (fasldir :com.informatimago.manifest "manifest"))))
;;   (format t "~%~A~%" dir) (finish-output)
;;   #+windows (mapc 'delete-file (directory dir))
;;   #-windows (asdf:run-shell-command "rm -rf ~S" (namestring dir)))


(ql:quickload *program-system*)
;; (ql:quickload :com.informatimago.manifest)
;; (shadow 'date)
;; (use-package "COM.INFORMATIMAGO.MANIFEST")


;;;---------------------------------------------------------------------
;;; Let's generate the target.

(in-package "COMMON-LISP-USER")
(format t "~%Generating ~A~%" *program-name*;; (executable-filename *program-name*)
        )
(finish-output)

;; (write-manifest *program-name* *program-system*)


#+ccl (progn (princ "ccl:save-application will exit.") (terpri) (finish-output))
#+ccl (ccl:save-application
        *program-name* ;; (executable-filename *program-name*)
       :toplevel-function (function com.informatimago.ipl.user-manager:main)
       :init-file nil
       :error-handler :quit-quietly
       ;; :application-class ccl:lisp-development-system
       ;; :clear-clos-cache t
       :purify nil
       ;; :impurify t
       :mode #o755
       :prepend-kernel t
       ;; :native t
       ) 

#+clisp (ext:saveinitmem
         (executable-filename *program-name*)
         :quiet t
         :verbose t
         :norc t
         :init-function (lambda ()
                          (ext:exit (handler-case
                                        (com.informatimago.lse.cli:main)
                                      (error ()
                                        1))))
         :script t
         :documentation "Système & Interpréteur L.S.E"
         :start-package "COMMON-LISP-USER"
         :keep-global-handlers nil
         :executable t)
#+clisp (ext:quit)


;; (print (list (find :swank *features*) (find-package "SWANK")))
;; (terpri)
;; (finish-output)

#|
    (cd "/home/pjb/src/pjb/nasium-lse/src/")
    (load "generate-server.lisp")
|#
;;;; THE END ;;;;

