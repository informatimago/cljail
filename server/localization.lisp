(defpackage "COM.INFORMATIMAGO.IPL.LOCALIZATION"
  (:use "COMMON-LISP")
  (:export "_" "*LANGUAGE*"))
(in-package "COM.INFORMATIMAGO.IPL.LOCALIZATION")

(defvar *language* :english)
(defun _ (string) string)
