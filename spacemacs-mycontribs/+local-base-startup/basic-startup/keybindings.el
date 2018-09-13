
(when (configuration-layer/package-usedp 'sessions-mgr)
  (defun spacemacs/sessions-mgr-enable ()
    (progn ;; "Keybinding: Elscreen"
      ;;{{ elscreen
      ;; https://github.com/syl20bnr/spacemacs/issues/7372
      (define-key evil-emacs-state-map (kbd "C-z") nil)
      (global-unset-key [C-z])
      ;; (global-set-key [C-z c] 'elscreen-create)
      (funcall
       #'(lambda (symbol value)
           (when (boundp 'elscreen-map)
             (elscreen-set-prefix-key value))
           (custom-set-default symbol value))
       'elscreen-prefix-key "\C-z")
      (global-set-key-if-unbind [s-right] 'elscreen-next)
      (global-set-key-if-unbind [s-left]  'elscreen-previous)
      (global-set-key-if-unbind [H-right] 'elscreen-move-right)
      (global-set-key-if-unbind [H-left]  'elscreen-move-left)
      (global-set-key-if-unbind [M-H-right]    'elscreen-swap)
      ;; (global-set-key-if-unbind [H-down]  'elscreen-previous)
      ;;}}
      ))
  (defun spacemacs/sessions-mgr-disable ()
    (deh-section "Keybinding: Elscreen"
      ;;{{ elscreen
      (global-unset-key [C-z])
      (setq elscreen-prefix-key "\C-z")
      (global-set-key [C-z c] 'elscreen-create)
      (global-unset-key [s-right])
      (global-unset-key [s-left])
      (global-unset-key [H-right])
      (global-unset-key [H-left])
      (global-unset-key [M-H-right])
      ;; (global-set-key-if-unbind [H-down]  'elscreen-previous)
      ;;}}
      ))

  (spacemacs/sessions-mgr-enable))
