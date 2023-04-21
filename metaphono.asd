(defsystem :metaphono
  :depends-on (:cl-ppcre :str)
  :components ((:file "segment")
               (:file "syllable")
               (:file "pword")
               (:file "rule")))

