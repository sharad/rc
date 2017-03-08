
(when (configuration-layer/package-usedp 'recentf)
  (defun spacemacs/recentf-enable ()
    (progn ;; "Keybinding: Recentf"
      ;;{{ recentf
      (global-set-key-if-unbind (kbd "C-c C-o") 'recentf-open-files)))

  (defun spacemacs/recentf-disable ()
    (progn ;; "Keybinding: Recentf"
      (global-unset-key-if-bound (kbd "C-c C-o") 'recentf-open-files)))

  (spacemacs/recentf-enable))

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
