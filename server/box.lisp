;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               box.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Manages chroot jails.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-05-22 <PJB> Created
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
(defpackage "COM.INFORMATIMAGO.IPL.BOX"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.IPL.LOCALIZATION"
        "COM.INFORMATIMAGO.IPL.CONFIGURATION"
        "COM.INFORMATIMAGO.IPL.USER"
        "COM.INFORMATIMAGO.IPL.OS")
  (:export "BOX-EXISTS-P" "ENSURE-BOX-EXISTS" "EXECUTE-BOX"))
(in-package "COM.INFORMATIMAGO.IPL.BOX")


(defun make-box-directory (user)
  (let* ((user-id (user-id user))
         (prefix (subseq user-id 0 *boxes-prefix*)))
    (merge-pathnames (make-pathname :directory (list :relative prefix user-id))
                     *boxes-directory*)))

(defun box-exists-p (user)
  (probe-file (merge-pathnames (make-pathname :directory '(:relative "etc")
                                              :name "lisp" :type "conf")
                               (make-box-directory user))))


(defun create-chroot-jail (user)
  (let ((chroot-dir (make-box-directory user)))
    (ensure-directories-exist (merge-pathnames (make-pathname :name "test" :type "text")
                                               chroot-dir))
    (asdf:run-shell-command
     "~S ~S ~S ~S"
     (namestring (make-pathname :name "make-chroot" :defaults *ipl-scripts-directory*))
     (namestring chroot-dir) *ipl-user* *ipl-group*)))

(defun execute-chroot-jail (user)
  (execv (namestring (make-pathname :name "run" :defaults *ipl-scripts-directory*))
         (list (namestring (make-box-directory user)) *ipl-user* *ipl-group*)))

(defun ensure-box-exists (user)
  (unless (box-exists-p user)
    (format t (_  "~&Setting up your environment...~%"))
    (finish-output)
    (create-chroot-jail user)))

(defun execute-box (user)
  (when (box-exists-p user)
    (execute-chroot-jail user)))
