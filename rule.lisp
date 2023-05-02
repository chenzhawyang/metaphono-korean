(defpackage :rule
  (:use :cl)
  (:export :all))

(in-package :rule)

(defstruct seg-rule
  pred
  change)

(defun seg-rule->fn (rule)
  (declare (type seg-rule rule))
  ;;
  (let ((pred (seg-rule-pred rule))
        (change (seg-rule-change rule)))
    (lambda (seg)
      (if (funcall pred seg)
          (funcall change seg)
          seg))))

(defun map-seg (seg-1 seg-2)
  (declare (type (or segment::consonant segment::vowel) 
                 seg-1 seg-2))
  ;;
  (lambda (seg)
    (if (equalp seg-1 seg)
        seg-2
        seg-1)))

(defun update-alist (sym val alist)
  (setf (cdr (assoc sym alist)) val)
  alist)

(defun update-alist-with-fn (sym fn alist)
  (setf (cdr (assoc sym alist)) (funcall fn (cdr (assoc sym alist))))
  alist)

(defun rewrite-seg-slot (slot val seg)
  (declare (type (or segment::consonant segment::vowel) seg))
  ;;
  (case (type-of seg)
    (segment::consonant
     (let ((alist (update-alist slot val (segment::consonant->alist seg))))
       (segment::alist->consonant alist)))
    (segment::vowel
     (let ((alist (update-alist slot val (segment::vowel->alist seg))))
       (segment::alist->vowel alist)))))

(defun rewrite-seg-slot-with-fn (slot fn seg)
  (declare (type (or segment::consonant segment::vowel) seg))
  ;;
  (case (type-of seg)
    (segment::consonant
     (let ((alist (update-alist-with-fn slot fn (segment::consonant->alist seg))))
       (segment::alist->consonant alist)))
    (segment::vowel
     (let ((alist (update-alist-with-fn slot fn (segment::vowel->alist seg))))
       (segment::alist->vowel alist)))))

;;

(defun apply-seg-rule (rule &optional seg)
  (declare (type seg-rule rule)
           (type (or segment::consonant segment::vowel) seg))
  ;;
  (if seg
      (funcall (seg-rule->fn rule) seg)
      (lambda (seg)
        (funcall (seg-rule->fn rule) seg))))

;;

(defstruct syll-rule
  pred
  change)

(defun rewrite-syll-slot (slot val syll)
  (declare (type syllable::syllable))
  nil)

(defun syll-rule->fn (rule)
  (declare (type syll-rule rule))
  (let ((pred (syll-rule-pred rule))
        (change (syll-rule-change rule)))
    (lambda (syll)
      (if (funcall pred syll)
          (funcall change syll)
          syll))))

(defun apply-syll-rule (rule &optional syll)
  (declare (type syll-rule rule)
           (type syllable::syllable syll))
  ;;
  (if syll
      (funcall (syll-rule->fn rule) syll)
      (lambda (seg)
        (funcall (syll-rule->fn rule) syll))))

;; historical sound changes in korean

(defun coda-neutralization (syllable)
  syllable)

