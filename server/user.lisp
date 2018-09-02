;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               user.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines a user database.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-05-23 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.IPL.USER"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.IPL.LOCALIZATION"
        "COM.INFORMATIMAGO.IPL.OS")
  (:export "CREATE-USER" "INVALID-VALIDATE-EXPRESSION"
           "MAKE-USER-ID" "PROGRAM-ERROR-FORM" "USER"
           "USER-EXISTS-P"
           "USER-ALREADY-EXISTS" "USER-DOES-NOT-EXIST" "USER-ERROR-ENTRY"
           "USER-ERROR-LOGIN" "USER-ERROR-TOKEN" "USER-ID" "USER-LOGIN" "USER-P"
           "USER-TOKEN" "USER-TOKEN-DOES-NOT-MATCH"
           "WITH-USER-DATABASE" "VALIDATE-USER"))
(in-package "COM.INFORMATIMAGO.IPL.USER")

;; TODO: This is a prototype, we need to access a persistent data structure.
(defvar *users* (make-hash-table :test (function equal)))

(defun load-users (db)
  (file-position db 0)
  (with-standard-io-syntax
    (let ((*package* (find-package "COM.INFORMATIMAGO.IPL.USER")))
      (loop
        :with table = (make-hash-table :test (function equal))
        :for user = (read db nil nil)
        :while user
        :do (setf (gethash (user-login user) table) user)
        :finally (return table)))))

(defun save-users (users db)
  (file-position db 0)
  (with-standard-io-syntax
    (let ((*package* (find-package "COM.INFORMATIMAGO.IPL.USER")))
      (maphash (lambda (key user)
                 (declare (ignore key))
                 (print user db))
               users)
      (print nil db)))
  (finish-output db))

(defun call-with-user-database (thunk)
  (with-open-file (db com.informatimago.ipl.configuration:*user-database*
                      :direction :io
                      :if-exists :append
                      :if-does-not-exist :create)
    (with-exclusive-access db
      (let ((*users* (load-users db)))
        (prog1 (funcall thunk)
          (save-users *users* db))))))

(defmacro with-user-database (&body body)
  `(call-with-user-database (lambda () ,@body)))

(defun user-exists-p (login)
  (gethash login *users*))

(defstruct user
  id
  login
  token)

(defun make-user-id (login token)
  (format nil "~:@(~{~2,'0X~}~)" (coerce (md5:md5sum-string (concatenate 'string login #(#\Newline) token)) 'list)))

(define-condition user-already-exists (error)
  ((entry :initarg :entry :reader user-error-entry)
   (login :initarg :login :reader user-error-login)
   (token :initarg :token :reader user-error-token))
  (:report (lambda (condition stream)
             (format stream (_ "There is already a user login ~S (id ~A).")
                     (user-error-login condition)
                     (user-id (user-error-entry condition))))))

(define-condition user-does-not-exist (error)
  ((login :initarg :login :reader user-error-login)
   (token :initarg :token :reader user-error-token))
  (:report (lambda (condition stream)
             (format stream (_ "There is no user with login ~S.")
                     (user-error-login condition)))))

(define-condition user-token-does-not-match (error)
  ((login :initarg :login :reader user-error-login)
   (token :initarg :token :reader user-error-token))
  (:report (lambda (condition stream)
             (format stream (_ "The token for user login ~S does not match.")
                     (user-error-login condition)))))

(define-condition invalid-validate-expression (program-error)
  ((form :initarg :form :reader program-error-form))
  (:report (lambda (condition stream)
             (format stream (_ "Invalid validate expression ~S.")
                     (program-error-form condition)))))

(defun create-user (login token)
  (let ((entry (gethash login *users*)))
    (when entry
      (error 'user-already-exists :entry entry :login login :token token))
    (setf (gethash login *users*) (make-user :id (make-user-id login token)
                                             :login login
                                             :token token))))

(defun validate-user (login token)
  (let ((entry (gethash login *users*)))
    (unless entry
      (error 'user-does-not-exist :login login :token token))
    (unless (string= token (user-token entry))
      (error 'user-token-does-not-match :login login :token token))
    entry))


#-(and) (progn
          (handler-case (create-user "pjb" "popo") (error (err) (princ-to-string err)))
          (handler-case (validate-user "pjb" "popo") (error (err) (princ err)))
          (handler-case (validate-user "pjb" "pipi") (error (err) (princ err)))
          (handler-case (validate-user "opo" "pipi") (error (err) (princ err)))
          )

;;;; THE END ;;;;
