
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
      (global-unset-key-if-bound (kbd "M-9") 'corral-parentheses-backward)
      (global-unset-key-if-bound (kbd "M-0") 'corral-parentheses-forward)
      (global-unset-key-if-bound (kbd "M-[") 'corral-brackets-backward)
      (global-unset-key-if-bound (kbd "M-]") 'corral-brackets-forward)
      (global-unset-key-if-bound (kbd "M-{") 'corral-braces-backward)
      (global-unset-key-if-bound (kbd "M-}") 'corral-braces-forward)
      (global-unset-key-if-bound (kbd "M-\"") 'corral-double-quotes-backward)))

  (spacemacs/corral-enable))



(global-set-key-if-unbind (kbd "H-SPC") '(lambda () (interactive) (insert "    ")))
;; (global-set-key-if-unbind (kbd "S-SPC") '(lambda () (interactive) (insert "            ")))

(when (configuration-layer/package-usedp 'highlight-symbol)
  (defun spacemacs/highlight-symbol-enable ()
    (progn ;; "Keybinding: Elscreen"
      (global-set-key-if-unbind (kbd "M-s-s") 'highlight-symbol)
      (global-set-key-if-unbind (kbd "M-s-n") 'highlight-symbol-next)
      (global-set-key-if-unbind (kbd "M-s-p") 'highlight-symbol-prev)
      (global-set-key-if-unbind (kbd "M-s-r") 'highlight-symbol-query-replace)))

  (defun spacemacs/highlight-symbol-disable ()
    (progn ;; "Keybinding: Elscreen"
      (global-unset-key-if-bound (kbd "M-s-s") 'highlight-symbol)
      (global-unset-key-if-bound (kbd "M-s-n") 'highlight-symbol-next)
      (global-unset-key-if-bound (kbd "M-s-p") 'highlight-symbol-prev)
      (global-unset-key-if-bound (kbd "M-s-r") 'highlight-symbol-query-replace)))

  (spacemacs/highlight-symbol-enable))

(when (configuration-layer/package-usedp 'symbol-overlay)
  (defun spacemacs/symbol-overlay-enable ()
    (progn
      (global-set-key-if-unbind (kbd "s-s") symbol-overlay-map)))

  (defun spacemacs/symbol-overlay-disable ()
    (progn
      (global-unset-key-if-bound (kbd "s-s") symbol-overlay-map)))

  (spacemacs/symbol-overlay-enable))
