(defpackage :wtf
  (:use :cl))

(in-package :wtf)

(defvar *world* nil)
(defvar *graph* nil)

(defun graph (decl)
  (setf *graph* decl)
  (format t "~&[WTF] Graph initialized.~%")
  decl)

(defun genesis ()
  (format t "~&[EOTW] Genesis begins.~%")
  (setf *world*
        (list :depth 0
              :nodes nil
              :events nil))
  (format t "[EOTW] World created.~%")
  *world*)
