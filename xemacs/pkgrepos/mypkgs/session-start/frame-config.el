
(require 'frame-utils)

(defun frame-utils-config ()
  (defun make-mail-chat-frame (&optional force)
    (interactive "P")
    (frame-launcher "mail-chat"
                    '("gnus" "erc")
                    (if force
                        #'(lambda (group)
                            (toggle-ibuffer-group group t))
                      #'toggle-ibuffer-group)))


  (defun make-mail-compose-frame ())

  (require 'sessions-mgr)

  ;; (frame-launcher)
  )

(frame-utils-config)

(provide 'frame-config)
;;; frame-config.el ends here
