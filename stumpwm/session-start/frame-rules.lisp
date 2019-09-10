
(in-package :stumpwm)

;; (define-frame-preference ".scratchpad"
;;     (1                                  ;frame-number
;;      t                                  ;raise
;;      t                                  ;lock
;;      t                                  ;create
;;      ;; &keys
;;      nil                                ;restore
;;      "class"                            ;window's class
;;      "instance"                         ;window's instance/resource name
;;      "type"                             ;window's type
;;      "role"                             ;window's role
;;      "title"                            ;window's title
;;      ))

;; (setf *window-placement-rules* nil)

(defun lotus-frame-preference-setup ()
  (let ((frame-num (if (> (screen-heads (current-screen)) 3 0))))
    (define-frame-preference
        ;; frame raise lock (lock AND raise == jumpto)
        ".scratchpad"
        ;; (0 t t :class "Empathy" :instance "empathy" :title nil ;"Contact List"
        ;;    :role nil)
        ;; (0 nil t ::title "xterm") ;; test
        ((+ 0 frame-num) nil t
         :class "Pidgin"
         :instance "Pidgin"
         :title "Buddy List"
         :role "buddy_list")

        ((+ 1 frame-num) t nil
         :class "Pidgin"
         :instance "Pidgin"
         :title  nil ;"FN LN"
         :role "conversation")

        ((+ 1 frame-num) nil t
         :class "Skype"
         :instance "skype"
         :title    nil ;"skypeid - Skype? Beta"
         :role "MainWindow"))))


(defun lotus-new-window-preferred-frame-setup ()
  (setf
   stumpwm::*new-window-preferred-frame*
   '(:focused
     :last
     :empty
     :unfocused)))
