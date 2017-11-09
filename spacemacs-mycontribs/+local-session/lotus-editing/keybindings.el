
(when (configuration-layer/package-usedp 'corral)
  (defun spacemacs/corral-enable ()
    (progn ;; "Keybinding: Elscreen"
      (global-set-key-if-unbind (kbd "M-9") 'corral-parentheses-backward)
      (global-set-key-if-unbind (kbd "M-0") 'corral-parentheses-forward)
      (global-set-key-if-unbind (kbd "M-[") 'corral-brackets-backward)
      (global-set-key-if-unbind (kbd "M-]") 'corral-brackets-forward)
      (global-set-key-if-unbind (kbd "M-{") 'corral-braces-backward)
      (global-set-key-if-unbind (kbd "M-}") 'corral-braces-forward)
      (global-set-key-if-unbind (kbd "M-\"") 'corral-double-quotes-backward)))

  (defun spacemacs/corral-disable ()
    (progn ;; "Keybinding: Elscreen"
      (global-unset-key-if-bind (kbd "M-9") 'corral-parentheses-backward)
      (global-unset-key-if-bind (kbd "M-0") 'corral-parentheses-forward)
      (global-unset-key-if-bind (kbd "M-[") 'corral-brackets-backward)
      (global-unset-key-if-bind (kbd "M-]") 'corral-brackets-forward)
      (global-unset-key-if-bind (kbd "M-{") 'corral-braces-backward)
      (global-unset-key-if-bind (kbd "M-}") 'corral-braces-forward)
      (global-unset-key-if-bind (kbd "M-\"") 'corral-double-quotes-backward)))

  (spacemacs/corral-enable))



(global-set-key-if-unbind (kbd "H-SPC") '(lambda () (interactive) (insert "    ")))
;; (global-set-key-if-unbind (kbd "S-SPC") '(lambda () (interactive) (insert "            ")))
