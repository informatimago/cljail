;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               user-manager.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Manages users.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-05-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(defpackage "COM.INFORMATIMAGO.IPL.USER-MANAGER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.IPL.CONFIGURATION"
        "COM.INFORMATIMAGO.IPL.LOCALIZATION"
        "COM.INFORMATIMAGO.IPL.USER"
        "COM.INFORMATIMAGO.IPL.BOX")
  (:import-from "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
                "CAT")
  (:export "MAIN"))
(in-package "COM.INFORMATIMAGO.IPL.USER-MANAGER")


(defun trim-spaces   (string) (string-trim #(#\space #\tab) string))
(defun empty         (string) (zerop (length string)))
(defun user-exists   (login)  (user-exists-p login))

(defun evaluate-validate (validate input)
  (cond
    ((symbolp validate) (funcall validate input))
    ((functionp validate) (funcall validate input))
    ((atom validate) (error 'invalid-validate-expression :form validate))
    (t (case (first validate)
         ((not)
          (when (cddr validate)
            (error 'invalid-validate-expression :form validate))
          (not (evaluate-validate (second validate) input)))
         ((and)
          (every (lambda (validate) (evaluate-validate validate input))
                 (rest validate)))
         ((or)
          (some (lambda (validate) (evaluate-validate validate input))
                (rest validate)))
         (otherwise (error 'invalid-validate-expression :form validate))))))


(defun ask (prompt &key filter validate abortable)
  (loop
    :with input
    :do (format t prompt)
        (finish-output)
        (setf input (read-line))
    :when filter
      :do (setf input (funcall filter input))
    :when (and abortable (string= input "ABORT"))
      :do (throw 'petite-gazongue 'aborted)
    :unless validate
      :do (loop-finish)
    :when (evaluate-validate validate input)
      :do (loop-finish)
    :do (format t (_ "~&Invalid input, it should be ~A") validate)
    :finally (return input)))


(defun interactive-create-user ()
  (loop
    :do (let ((login (ask (_ "~&Enter your new login: ") :filter 'trim-spaces
                                                         :validate '(not empty)
                                                         :abortable t))
              (token (ask (_ "~&Enter a secret token: ") :filter 'trim-spaces
                                                         :validate nil
                                                         :abortable t)))
          (when (string-equal login "ABORT")
            (throw 'petites-gazongues nil))
          (handler-case
              (return-from interactive-create-user
                (with-user-database
                  (create-user login token)))
            (user-already-exists (err)
              (format t "~&~A~%" err))))))


(defun interactive-log-user-in ()
  (loop
    :for user = (handler-case
                    (progn
                      (let* ((login (ask  (_ "~2%(Type \"new\" if you're a new user)~%Login: ")
                                          :filter 'trim-spaces :abortable t))
                             (command (string-upcase login)))
                        (cond
                          ((string= command "ABORT")
                           (throw 'petites-gazongues nil))
                          ((string= command "NEW")
                           (setf *language* :english)
                           (interactive-create-user))
                          ((string= command "NOUVEAU")
                           (setf *language* :french)
                           (interactive-create-user))
                          ((string= command "NUOVO")
                           (setf *language* :italian)
                           (interactive-create-user))
                          ((string= command "NUEVO")
                           (setf *language* :spanish)
                           (interactive-create-user))
                          ((string= command "NEUE")
                           (setf *language* :german)
                           (interactive-create-user))
                          (t
                           (let ((token (ask (_ "~&Token: ") :filter 'trim-spaces :abortable t)))
                             (with-user-database
                               (validate-user login token)))))))
                  (error (err)
                    (format t (_ "~&Error ~A~%") err)
                    nil))
    :until user
    :finally (return user)))


(defun banner ()
  (format t (_ "~5%Welcome to IPLwCL!~5%"))
  (when (probe-file *banner-file*)
    (cat *banner-file*))
  (finish-output))

(defun motd (user)
  (format t (_ "~&Welcome ~A!~%") (user-login user))
  (when (probe-file *motd-file*)
    (cat *motd-file*))
  (finish-output))


(defmacro letf ((&rest bindings) &body body)
  (let ((saves (mapcar (lambda (binding)
                         (assert (and (listp binding) (= 2 (length binding))))
                         (gensym))
                       bindings))
        (vars  (mapcar (function first)  bindings))
        (vals  (mapcar (function second) bindings)))
    `(let (,@(mapcar (lambda (save var) `(,save ,var)) saves vars))
       (unwind-protect
            (progn
              (setf ,@(mapcan (function list) vars vals))
              ,@body)
         (setf  ,@(mapcan (function list) vars saves))))))


(defmacro with-abort-disabled (&body body)
  #-ccl `(progn
           ,@body)
  #+ccl `(letf ((ccl::*quit-interrupt-hook* t)
                (ccl::*inhibit-abort* t)
                (ccl::*break-hook* (lambda (cnd hook)
                                     (declare (ignore cnd hook))
                                     ;; doesn't work: (format t "~&break~%") (finish-output)
                                     (invoke-restart 'continue))))
            ,@body))


(defun main ()
  (with-abort-disabled
   (catch 'grandes-gazongues
     (catch 'petites-gazongues
       (handler-case
           (progn
             (banner)
             (let ((user (interactive-log-user-in)))
               (motd user)
               (ensure-box-exists user)
               (EXECUTE-BOX user))) ; this doesn't exit, we call exec(2).
         (error (err)
                (format t "~&~A~%" err)
                (finish-output)))
       (return-from main)))
   (format t (_ "~&Aborted~%"))
   (finish-output)))

