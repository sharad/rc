
(require 'frame-utils)

(progn
  (defun frame-utils-config ()

    (setq frame-utils-notify 'message-notify)

    (defun make-mail-chat-frame (&optional force)
      (interactive "P")
      (frame-launcher "mail-chat"
                      '("gnus" "erc")
                      (if force
                          #'(lambda (group)
                              (toggle-ibuffer-group group t))
                          #'toggle-ibuffer-group)))


    (defun make-mail-compose-frame ())

    (require 'sessions-mgr)               ;*frame-session-restore*

    (defadvice frame-launcher (around frame-launcher activate)
      (let ((*frame-session-restore* nil)  ;not need to restore elsession for frames
            (org-donot-try-to-clock-in t)) ;no clock require to be clocked-in.
        ad-do-it
        ))
    )

  (frame-utils-config))

(provide 'frame-config)
;;; frame-config.el ends here
