(defpackage "COM.INFORMATIMAGO.IPL.GDBM"
  (:use "COMMON-LISP"
        "CFFI-GDBM")
  (:export "READER" "WRITER" "WRCREAT" "NEWDB"
           "FAST" "SYNC" "NOLOCK" 
           "INSERT" "REPLACE"))
(in-package "COM.INFORMATIMAGO.IPL.GDBM")

(defconstant READER  0)     ; A reader. 
(defconstant WRITER  1)		; A writer. 
(defconstant WRCREAT 2)     ; A writer.  Create the db if needed. 
(defconstant NEWDB   3)     ; A writer.  Always create a new db. 
(defconstant FAST    #x10)  ; Write fast! => No fsyncs.  OBSOLETE. 
(defconstant SYNC    #x20)  ; Sync operations to the disk. 
(defconstant NOLOCK  #x40)  ; Don't do file locking operations. 
(defconstant INSERT  0)     ; Never replace old data with new.
(defconstant REPLACE 1)     ; Always replace old data with new.


#-(and) (progn
          (ensure-directories-exist com.informatimago.ipl.configuration:*user-database*)
          (defparameter *db* (db-open com.informatimago.ipl.configuration:*user-database*
                                      :flags wrcreat :mode sync))
          (loop
              :with iterator = (make-key-iterator :string *db*)
              :for key = (funcall iterator)
              :while key :collect key)
          
          (store "key" "value" REPLACE *db*)
          (fetch "key" :string *db*)

          )



(defun user-id (login)
  (let ((entry (find *ipl-user* (com.informatimago.common-lisp.unix.passwd:read-passwd)
                     :key (function com.informatimago.common-lisp.unix.passwd:entry-login))))
    (when entry
      (com.informatimago.common-lisp.unix.passwd:entry-uid entry))))

(defun group-id (login)
  (let ((entry (find *ipl-user* (com.informatimago.common-lisp.unix.group:read-group)
                     :key (function com.informatimago.common-lisp.unix.group:entry-name))))
    (when entry
      (com.informatimago.common-lisp.unix.passwd:entry-gid entry))))

(defun change-to-ipl-user ()
  (let ((uid (user-id *ipl-user*))
        (gid (group-id *ipl-group*)))
    (if (and uid gid)
        (progn 
          #+ccl (ccl::setuid uid)
          #+ccl (ccl::setgid gid)
          #-ccl (error "Cannot setuid in ~A" (lisp-implementation-type)))
        (error (_ "Cannot find IPL user ~S or group ~S." *ipl-user* *ipl-group*)))))
