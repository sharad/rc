
(when (configuration-layer/package-usedp 'PACKAGE)
  (defun spacemacs/PACKAGE-enable ()
    (progn ;; "Keybinding: Elscreen"
      ;;{{ elscreen
      (define-key evil-emacs-state-map (kbd "") nil)
      ;; (global-unset-key [C-z])
      (global-set-key [] 'elscreen-create)))

  (defun spacemacs/PACKAGE-disable ()
    (progn ;; "Keybinding: Elscreen"
      (define-key evil-emacs-state-map nil)
      (global-unset-key [])))

  (spacemacs/PACKAGE-enable))
