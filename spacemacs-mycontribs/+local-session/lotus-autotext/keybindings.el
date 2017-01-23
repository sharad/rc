
(when (configuration-layer/package-usedp 'auto-yasnippet)
  (defun spacemacs/sessions-mgr-enable ()
    (global-set-key (kbd "H-w") 'create-auto-yasnippet)
    (global-set-key (kbd "H-y") 'expand-auto-yasnippet))
  (defun spacemacs/sessions-mgr-disable ()
    (global-unset-key (kbd "H-w"))
    (global-unset-key (kbd "H-y")))

  (spacemacs/auto-yasnippet-enable))
