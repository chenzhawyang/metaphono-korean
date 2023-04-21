(defpackage :pword
  (:use :cl)
  (:export :all)
  (:local-nicknames (:re :cl-ppcre)))

(in-package :pword)

(defun split-by-hyphen (str)
  (re:split "(\\-)" str))

(defun string->pword (str)
  (mapcar #'syllable::string->syllable 
          (split-by-hyphen str)))
