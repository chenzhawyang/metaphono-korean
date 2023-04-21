(defun metaphono-load ()
  (pushnew (uiop:getcwd) asdf:*central-registry*)
  (asdf:load-system :metaphono))
