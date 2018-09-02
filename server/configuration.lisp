;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               configuration.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Configuration.
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
(defpackage "COM.INFORMATIMAGO.IPL.CONFIGURATION"
  (:use "COMMON-LISP")
  (:export
   "*IPL-SCRIPTS-DIRECTORY*"
   "*IPL-USER*"
   "*IPL-GROUP*"
   "*BOXES-DIRECTORY*"
   "*BOXES-PREFIX*"
   "*USER-DATABASE*"
   "*MOTD-FILE*" "*BANNER-FILE*" "*EXTERNAL-FORMAT*"))
(in-package "COM.INFORMATIMAGO.IPL.CONFIGURATION")

(defvar *ipl-prefix*            #P"/srv/ipl/")
(defvar *ipl-scripts-directory* (merge-pathnames #P"scripts/" *ipl-prefix* nil))
(defvar *ipl-user*              "iplwcl")
(defvar *ipl-group*             "iplwcl")
(defvar *user-database*         (merge-pathnames #P"etc/users.data" *ipl-prefix* nil))
(defvar *motd-file*             (merge-pathnames #P"etc/motd"       *ipl-prefix* nil))
(defvar *banner-file*           (merge-pathnames #P"etc/banner"     *ipl-prefix* nil))
(defvar *external-format*       :utf-8)
(defvar *boxes-directory*       (merge-pathnames #P"users/"         *ipl-prefix* nil))
(defvar *boxes-prefix*          3)

;;;; THE END ;;;;
