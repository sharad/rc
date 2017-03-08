
(when (configuration-layer/package-usedp 'newcomment)
  (defun spacemacs/newcomment-enable ()
    (progn ;; "Keybinding: Newcomment"
      ;;{{ newcomment
      (global-set-key-if-unbind (kbd "H-c H-c") 'comment-or-uncomment-region)))

  (defun spacemacs/newcomment-disable ()
    (progn ;; "Keybinding: Newcomment"
      (global-unset-key-if-bound (kbd "H-c H-c") 'comment-or-uncomment-region)))

  (spacemacs/newcomment-enable))
