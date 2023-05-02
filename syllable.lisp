(defpackage :syllable
  (:use :cl)
  (:export :all)
  (:local-nicknames (:re :cl-ppcre)))

(in-package :syllable)

(defstruct syllable
  (onset nil :type list)
  (nucleus nil :type list)
  (coda nil :type list)
  accent)

(defun syllable->alist (syllable)
  nil)

(defun alist->syllable (alist)
  nil)
 
;;

(defun string->nucleus (str)
  (let ((monophthong (segment::string->monophthong str))
        (polyphthong (segment::string->polyphthong str)))
   (if monophthong
       (list monophthong)
       polyphthong)))

(defun nucleus->string (nucleus)
  (if (> (length nucleus) 1)
      (segment::polyphthong->string nucleus)
      (segment::monophthong->string (car nucleus))))

(defun string->accent (str)
  (cond
    ((string-equal str "(L)") :L)
    ((string-equal str "(H)") :H)
    ((string-equal str "(R)") :R)
    (t :X)))

(defun accent->string (accent)
  (case accent
    (:L "(L)")
    (:H "(H)")
    (:R "(R)")
    (:X "(X)")))

(defun split-at-lparen (str)
  (re:split "(?=\\()" str))

(defun split-at-substr (substr str)
  (let ((idx (search substr str)))
    (if (null idx)
        (list str)
        (list (subseq str 0 idx) (subseq str idx)))))

(defun ->constituent-strs (str)
  (let* ((vowel-chars (coerce "aeiouwy" 'list)))
    (labels ((member? (element lst) 
               (if (member element lst) t nil))
             (first-vowel-pos (str)
               (position-if (lambda (char) (member? char vowel-chars)) str))
             (first-consonant-pos (str)
               (position-if (lambda (char) (not (member? char vowel-chars))) str))
             (split-by-dot (str) (re:split "(\\.)" str)))
      (let* ((segs (first (split-at-lparen str)))
             (accent-s (second (split-at-lparen str)))
             (onset-s (split-by-dot (subseq segs 0 (first-vowel-pos segs))))
             (rime-s (subseq segs (first-vowel-pos segs)))
             (nucleus-s (subseq rime-s 0 (first-consonant-pos rime-s)))
             (coda-s (if (first-consonant-pos rime-s)
                         (split-by-dot (subseq rime-s (first-consonant-pos rime-s)))
                         nil)))
        (list onset-s nucleus-s coda-s accent-s)))))
 
(defun string->constituents (str)
  (let* ((strs (->constituent-strs str))
         (onset (mapcar #'segment::string->consonant (first strs)))
         (nucleus (string->nucleus (second strs)))
         (coda (mapcar #'segment::string->consonant (third strs)))
         (accent (string->accent (fourth strs))))
    (list onset nucleus coda accent)))

(defun string->syllable (str)
  (let* ((constituents (string->constituents str)))
    (make-syllable 
     :onset (first constituents)
     :nucleus (second constituents)
     :coda (third constituents)
     :accent (fourth constituents))))

(defun syllable->string (syllable)
  (let* ((onset 
           (str:join "." (mapcar #'segment::consonant->string (syllable-onset syllable))))
         (nucleus (nucleus->string (syllable-nucleus syllable)))
         (coda 
           (str:join "." (mapcar #'segment::consonant->string (syllable-coda syllable))))
         (accent (accent->string (syllable-accent syllable))))
    (str:join "" (list onset nucleus coda accent))))
