
(when (configuration-layer/package-usedp 'boxes)
  (defun spacemacs/boxes-enable ()
    (deh-section "Keybinding: Boxes"
      ;;{{ boxes
      ;; https://github.com/syl20bnr/spacemacs/issues/7372
      (global-set-key-if-unbind (kbd "H-c q") 'boxes-create)
      (global-set-key-if-unbind (kbd "H-c r") 'boxes-remove)
      ;;}}
      ))
  (defun spacemacs/boxes-disable ()
    (deh-section "Keybinding: Boxes"
      ;;{{ boxes
      (global-unset-key-if-bound (kbd "H-c q") 'boxes-create)
      (global-unset-key-if-bound (kbd "H-c r") 'boxes-remove)
      ;;}}
      ))

  (spacemacs/boxes-enable))
