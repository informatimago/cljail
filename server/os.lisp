;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               lockf.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    OS interface.
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
(defpackage "COM.INFORMATIMAGO.IPL.OS"
  (:use "COMMON-LISP"
        "CFFI")
  (:export "WITH-EXCLUSIVE-ACCESS" "EXECV"))
(in-package "COM.INFORMATIMAGO.IPL.OS")


(defcfun "lockf" :int (fd :int) (cmd :int) (len :long))

(defun file-descriptor (stream)
  #+ccl (ccl:stream-device stream :output)
  #-ccl (error "file-descriptor not defined in ~A" (lisp-implementation-type)))


(defconstant ULOCK 0)  ; Unlock a previously locked region.
(defconstant LOCK  1)  ; Lock a region for exclusive use.  
(defconstant TLOCK 2)  ; Test and lock a region for exclusive use. 
(defconstant TEST  3)  ; Test a region for other processes locks.  



(defun call-with-exclusive-access (stream thunk)
  (unwind-protect
       (progn
         (lockf (file-descriptor stream) LOCK 1)
         (funcall thunk))
    (lockf (file-descriptor stream) ULOCK 1)))

(defmacro with-exclusive-access (stream &body body)
  `(call-with-exclusive-access ,stream (lambda () ,@body)))




(defcfun (sys-execv "execv") :int (path :string) (argv (:pointer :string)))

(defun execv (program args)
  "Replace current executable with another one."
  (check-type program string)
  (assert (every (function stringp) args))
  ;; (format t "~&execv ~S ~S~%" program args)
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (with-foreign-objects ((argv :string (+ 2 (length args))))
    (loop :for index :from 0 :by 1
          :for arg :in (cons program args)
          :do (setf (mem-aref argv :string index) (foreign-string-alloc arg :encoding :utf-8 :null-terminated-p t))
          :finally (setf (mem-aref argv :string index) (null-pointer)))
    (when (minusp (sys-execv (foreign-string-alloc program :encoding :utf-8 :null-terminated-p t)
                             argv))
      (error "execv(3) returned."))))

;;;; THE END ;;;;
