;;;; remember-win.asd

(asdf:defsystem #:remember-win
  :serial t
  :description "Remember window"
  :author "Anonymous"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "utils")
               (:file "parse")
               (:file "remember-win" :depends-on ("utils" "parse"))))

