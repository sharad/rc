
(when (configuration-layer/package-usedp 'auto-yasnippet)
  (defun spacemacs/auto-yasnippet-enable ()
    (global-set-key (kbd "H-w") 'create-auto-yasnippet)
    (global-set-key (kbd "H-y") 'expand-auto-yasnippet))
  (defun spacemacs/auto-yasnippet-disable ()
    (global-unset-key (kbd "H-w"))
    (global-unset-key (kbd "H-y")))

  (spacemacs/auto-yasnippet-enable))
