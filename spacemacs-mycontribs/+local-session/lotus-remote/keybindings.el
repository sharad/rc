
(when (configuration-layer/package-usedp 'tramp)
  (defun spacemacs/tramp-enable ()
    (progn ;; "Keybinding: Elscreen"
      (global-set-key-if-unbind (kbd "C-c C-r") 'find-alternative-file-with-sudo)
      (global-set-key-if-unbind (kbd "C-c C-r") 'sudo-edit-current-file)))

  (defun spacemacs/tramp-disable ()
    (progn ;; "Keybinding: Elscreen"
      (global-unset-key-if-bound (kbd "C-c C-r") 'find-alternative-file-with-sudo)
      (global-unset-key-if-bound (kbd "C-c C-r") 'sudo-edit-current-file)))

  (spacemacs/tramp-enable))
