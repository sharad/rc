;;;; package.lisp

(defpackage #:remember-win
  ;; (:use #:cl :stumpwm)
  (:nicknames :rememwin)
  (:use #:cl
        :common-lisp
        :stumpwm
        ;; :cl-ppcre
                                        ; :sb-ext
                                        ; :xlib
        ;; :sb-thread
        )
  ;; (:shadowing-import-from #:in.net.sharad.utils #:getenv #:concat)
  (:shadowing-import-from #:stumpwm #:run-shell-command))
  

