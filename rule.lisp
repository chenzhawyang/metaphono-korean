(defpackage :rule
  (:use :cl)
  (:export :all))

(in-package :rule)

;; (defstruct rule
;;   change)

;; (defmacro defrule (rule)
;;   nil)

(defun map-seg (seg-1 seg-2)
  (lambda (seg)
    (if (equal seg-1 seg)
        seg-2
        seg-1)))






