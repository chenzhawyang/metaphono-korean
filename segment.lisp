(defpackage :segment
  (:use :cl)
  (:export :all))

(in-package :segment)

(defstruct consonant
  voice
  place
  manner)

(defun consonant->alist (consonant)
  `((voice . ,(consonant-voice consonant))
    (place . ,(consonant-place consonant))
    (manner . ,(consonant-manner consonant))))

(defun alist->consonant (alist)
  (flet ((assoc-get (sym) (cdr (assoc sym alist))))
    (make-consonant
     :voice (assoc-get 'voice)
     :place (assoc-get 'place)
     :manner (assoc-get 'manner))))

(defvar korean-consonants 
  '((/p . '(:lax :bilabial :stop))
    (/ph . '(:aspirated :bilabial :stop))
    (/pp . '(:tense :bilabial :stop))
    (/m . '(:voiced :bilabial :nasal))
    (/t . '(:lax :alveolar :stop))
    (/th . '(:aspirated :alveolar :stop))
    (/tt . '(:tense :alveolar :stop))
    (/s . '(:lax :alveolar :fricative))
    (/ss . '(:tense :alveolar :fricative))
    (/n . '(:voiced :alveolar :nasal))
    (/l . '(:voiced :alveolar :leteral))
    (/k . '(:lax :velar :stop))
    (/kh . '(:aspirated :velar :stop))
    (/kk . '(:tense :velar :stop))
    (/h . '(:lax :velar :fricative))
    (/ng . '(:lax :velar :nasal))))

(defun ->consonant-features (symbols)
  (let* ((symbols (eval symbols))
         (voice (first symbols))
         (place (second symbols))
         (manner (third symbols)))
    (list :voice voice
          :place place
          :manner manner)))
 
(defun pair->consonant (pair)
  (let ((name (car pair))
        (features (->consonant-features (cdr pair))))
    `(defvar ,name (make-consonant ,@features))))

(defun consonants-init ()
  (loop :for pair :in korean-consonants
        :do (eval (pair->consonant pair))))

(eval-when (:load-toplevel)
  (consonants-init))

(defun pair->string&consonant (pair)
  (let* ((str (string-downcase (subseq (symbol-name (car pair)) 1)))
         (features (->consonant-features (cdr pair)))
         (consonant (apply #'make-consonant features)))
    (list* str consonant)))

(defvar string&consonant-assoc-lst 
  (mapcar #'pair->string&consonant korean-consonants))

(defun string->consonant (str)
  (cdr (assoc str string&consonant-assoc-lst :test #'string=)))

(defun consonant->string (consonant)
  (flet ((transpose (pair)
           (list* (cdr pair) (car pair))))
    (let ((consonant&string-assoc-lst 
            (mapcar #'transpose string&consonant-assoc-lst)))
      (cdr (assoc consonant consonant&string-assoc-lst :test #'equalp)))))

(defstruct vowel
  height
  centrality
  roundedness)

(defun vowel->alist (vowel)
  `((height . ,(vowel-height vowel))
    (centrality . ,(vowel-centrality vowel))
    (roundedness . ,(vowel-roundedness vowel))))

(defun alist->vowel (alist)
  (flet ((assoc-get (sym) (cdr (assoc sym alist))))
    (make-vowel
     :height (assoc-get 'height)
     :centrality (assoc-get 'centrality)
     :roundedness (assoc-get 'roundedness))))

(defvar korean-monophthongs
  '((/a . '(:low :central :unrounded))    
    (/o . '(:low :back :rounded))
    (/e . '(:mid :central :unrounded))
    (/wo . '(:mid :back :rounded))
    (/i . '(:high :front :unrounded))
    (/u . '(:high :central :unrounded))
    (/wu . '(:high :back :rounded))))

(defun ->monophthong-features (symbols)
  (let* ((symbols (eval symbols))
         (height (first symbols))
         (centrality (second symbols))
         (roundedness (third symbols)))
    (list :height height
          :centrality centrality
          :roundedness roundedness)))

(defun pair->monophthong (pair)
  (let ((name (car pair))
        (features (->monophthong-features (cdr pair))))
    `(defvar ,name (make-vowel ,@features))))

(defun monophthongs-init ()
  (loop :for pair :in korean-monophthongs
        :do (eval (pair->monophthong pair))))

(eval-when (:load-toplevel)
  (monophthongs-init))

(defun pair->string&monophthong (pair)
  (let* ((str (string-downcase (subseq (symbol-name (car pair)) 1)))
         (features (->monophthong-features (cdr pair)))
         (vowel (apply #'make-vowel features)))
    (list* str vowel)))

(defvar string&monophthong-assoc-lst
  (mapcar #'pair->string&monophthong korean-monophthongs))

(defun string->monophthong (str)
  (cdr (assoc str string&monophthong-assoc-lst :test #'string=)))

(defun monophthong->string (monophthong)
  (flet ((transpose (pair)
           (list* (cdr pair) (car pair))))
    (let ((monophthong&string-assoc-lst
            (mapcar #'transpose string&monophthong-assoc-lst)))
      (cdr (assoc monophthong monophthong&string-assoc-lst :test #'equalp)))))

(defvar korean-polyphthongs
  '((/ay . '(/a /i))
    (/ey . '(/e /i))
    (/woy . '(/wo /i))
    (/wuy . '(/wu /i))
    (/oy . '(/o /i))
    (/uy . '(/u /i))
    (/ya . '(/i /a))
    (/yay . '(/i /a /i))
    (/ye . '(/i /e))
    (/yey . '(/i /e /i))
    (/ywo . '(/i /wo))
    (/yu . '(/i /wu))
    (/yo . '(/i /o))
    (/wa . '(/wo /a))
    (/way . '(/wo /a /i))
    (/we . '(/wu /e))
    (/wey . '(/wu /e /i))))

(defun pair->polyphthong (pair)
  (let ((name (car pair))
        (monophthongs (mapcar #'eval (eval (cdr pair)))))
    `(defvar ,name ',monophthongs)))

(defun polyphthongs-init ()
  (loop :for pair :in korean-polyphthongs
        :do (eval (pair->polyphthong pair))))

(eval-when (:load-toplevel)
  (polyphthongs-init))

(defun pair->string&polyphthong (pair)
  (let* ((str (string-downcase (subseq (symbol-name (car pair)) 1)))
         (monophthongs (mapcar #'eval (eval (cdr pair)))))
    (list* str monophthongs)))

(defvar string&polyphthong-assoc-lst
  (mapcar #'pair->string&polyphthong korean-polyphthongs))

(defun string->polyphthong (str)
  (cdr (assoc str string&polyphthong-assoc-lst :test #'string=)))

(defun polyphthong->string (polyphthong)
  (flet ((transpose (pair)
           (list* (cdr pair) (car pair))))
    (let ((polyphthong&string-assoc-lst
            (mapcar #'transpose string&polyphthong-assoc-lst)))
      (cdr (assoc polyphthong polyphthong&string-assoc-lst :test #'equalp)))))

;;

