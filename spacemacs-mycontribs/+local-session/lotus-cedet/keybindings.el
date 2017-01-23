
(when (configuration-layer/package-usedp 'semantic)
  (defun spacemacs/semantic-enable ()
    (progn ;; "Keybinding: Elscreen"
      (defun my-cedet-hook ()
        (local-set-key [(control return)] 'semantic-ia-complete-symbol)
        (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
        (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
        (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
      (add-hook 'c-mode-common-hook 'my-cedet-hook)

      (defun my-c-mode-cedet-hook ()
        (local-set-key "." 'semantic-complete-self-insert)
        (local-set-key ">" 'semantic-complete-self-insert))
      (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)))

  (defun spacemacs/semantic-disable ()
    (progn ;; "Keybinding: Elscreen"
      (remove-hook 'c-mode-common-hook 'my-cedet-hook)
      (remove-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)))

  (spacemacs/semantic-enable))
