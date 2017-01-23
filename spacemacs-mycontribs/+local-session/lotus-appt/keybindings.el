
(when (configuration-layer/package-usedp 'PACKAGE)
  (defun spacemacs/PACKAGE-enable ()
    (deh-section "Keybinding: Elscreen"
      ;;{{ elscreen
      ;; https://github.com/syl20bnr/spacemacs/issues/7372
      (define-key evil-emacs-state-map (kbd "C-z") nil)
      ;; (global-unset-key [C-z])
      (global-set-key [C-z c] 'elscreen-create)
      (global-set-key-if-unbind [s-right] 'elscreen-next)
      (global-set-key-if-unbind [s-left]  'elscreen-previous)
      (global-set-key-if-unbind [H-right] 'elscreen-move-right)
      (global-set-key-if-unbind [H-left]  'elscreen-move-left)
      (global-set-key-if-unbind [M-H-right]    'elscreen-swap)
      ;; (global-set-key-if-unbind [H-down]  'elscreen-previous)
      ;;}}
      ))
  (defun spacemacs/PACKAGE-disable ()
    (deh-section "Keybinding: Elscreen"
      ;;{{ elscreen
      ;; (global-set-key [C-z c] 'elscreen-create)
      (global-unset-key [s-right])
      (global-unset-key [s-left])
      (global-unset-key [H-right])
      (global-unset-key [H-left])
      (global-unset-key [M-H-right])
      ;; (global-set-key-if-unbind [H-down]  'elscreen-previous)
      ;;}}
      ))

  (spacemacs/PACKAGE-enable))
