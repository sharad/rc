
(when (configuration-layer/package-usedp 'PACKAGE)
  (defun spacemacs/PACKAGE-enable ()
    (progn ;; "Keybinding: Package"
      ;;{{ package
      (define-key evil-emacs-state-map (kbd "") nil)
      ;; (global-unset-key [C-z])
      (global-set-key [] 'package-create)))

  (defun spacemacs/PACKAGE-disable ()
    (progn ;; "Keybinding: Package"
      (define-key evil-emacs-state-map nil)
      (global-unset-key [])))

  (spacemacs/PACKAGE-enable))


(when (configuration-layer/package-usedp 'folding)
  (defun spacemacs/folding-enable ()
    (progn ;; "Keybinding: Folding"
      ;;{{ folding
      (global-set-key-if-unbind (kbd "C-+") 'toggle-hiding)
      (global-set-key-if-unbind (kbd "C-\\") 'toggle-selective-display)))

  (defun spacemacs/folding-disable ()
    (progn ;; "Keybinding: Folding"
      (global-unset-key-if-bound (kbd "C-+") 'toggle-hiding)
      (global-unset-key-if-bound (kbd "C-\\") 'toggle-selective-display)))

  (spacemacs/folding-enable))


(when (configuration-layer/package-usedp 'hideshow)
  (defun spacemacs/hideshow-enable ()
    (progn ;; "Keybinding: Hideshow"
      ;;{{ hideshow
      ;; note S is for Shift
      ;; note s is for Super
      (global-set-key-if-unbind (kbd "s-c")   'hs-hide-all-but-at-point)
      (global-set-key-if-unbind (kbd "s-b")   'hs-hide-block-at-point)
      (global-set-key-if-unbind (kbd "s-u")   'hs-show-all)))

  (defun spacemacs/hideshow-disable ()
    (progn ;; "Keybinding: Hideshow"
      ;; note S is for Shift
      ;; note s is for Super
      (global-unset-key-if-bound (kbd "s-c")   'hs-hide-all-but-at-point)
      (global-unset-key-if-bound (kbd "s-b")   'hs-hide-block-at-point)
      (global-unset-key-if-bound (kbd "s-u")   'hs-show-all)))

  (spacemacs/hideshow-enable))
