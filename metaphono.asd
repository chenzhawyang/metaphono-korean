(defsystem :metaphono
  :depends-on (:cl-ppcre :str :alexandria)
  :components ((:file "segment")
               (:file "syllable")
               (:file "pword")
               (:file "rule")))

