
(when (configuration-layer/package-usedp 'project-buffer-occur)
  (defun spacemacs/project-buffer-occur-enable ()
    (progn ;; "Keybinding: project-buffer-occur"
      (use-package project-buffer-mode
          :defer t
          :config
          (progn
            ;;{{ project-buffer-occur
           ;; http://www.emacswiki.org/emacs/ProjectBufferOccur
           (define-key project-buffer-mode-map [(control ?f)] 'project-buffer-occur)))))

  (defun spacemacs/project-buffer-occur-disable ()
    (use-package project-buffer-mode
        :defer t
        :config
        (progn
          ;;{{ project-buffer-occur
          ;; http://www.emacswiki.org/emacs/ProjectBufferOccur
          (define-key project-buffer-mode-map [(control ?f)] nil))))

  (spacemacs/project-buffer-occur-enable))

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
