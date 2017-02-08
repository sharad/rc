
(when (configuration-layer/package-usedp 'devel-notes)
  (defun spacemacs/devel-notes-enable ()
    (progn ;; "Keybinding: devel note"
      ;; http://www.emacswiki.org/emacs/DevelNotes
      (global-set-key "\C-cza" 'develnotes-add-annotation)
      (global-set-key "\C-czv" 'develnotes-visit-file)
      (global-set-key "\C-czt" 'develnotes-add-TODO)
      (global-set-key "\C-czf" 'develnotes-add-FIXME)))

  (defun spacemacs/devel-notes-disable ()
    (progn ;; "Keybinding: devel note"
      ;; http://www.emacswiki.org/emacs/DevelNotes
      (global-unset-key "\C-cza" 'develnotes-add-annotation)
      (global-unset-key "\C-czv" 'develnotes-visit-file)
      (global-unset-key "\C-czt" 'develnotes-add-TODO)
      (global-unset-key "\C-czf" 'develnotes-add-FIXME)))

  (spacemacs/devel-notes-enable))



(when (configuration-layer/package-usedp 'outline-magic)
  (defun spacemacs/outline-magic-enable ()
    (progn ;; "Keybinding: outline-magic"
      (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))

  (defun spacemacs/outline-magic-disable ()
    (progn ;; "Keybinding: outline-magic"
      (define-key outline-minor-mode-map (kbd "<C-tab>") nil)))

  (spacemacs/outline-magic-enable))



(when (configuration-layer/package-usedp 'disaster)
  (defun spacemacs/disaster-enable ()
    (progn ;; "Keybinding: disaster"
      ;;{{ disaster
      (with-eval-after-load "cc-mode"
        (defun disaster-c-mode-common-keybinding-setup ()
          (define-key c-mode-base-map (kbd "C-c C-d") 'disaster)
          (define-key c-mode-base-map (kbd "C-c C-c") 'compile))
        (add-hook 'c-mode-common-hook 'disaster-c-mode-common-keybinding-setup))))

  (defun spacemacs/disaster-disable ()
    (progn ;; "Keybinding: disaster"
      ;;{{ disaster
      (with-eval-after-load "cc-mode"
        (when (fboundp 'disaster-c-mode-common-keybinding-setup)
            (remove-hook 'c-mode-common-hook 'disaster-c-mode-common-keybinding-setup)))))

  (spacemacs/disaster-enable))

(when (configuration-layer/package-usedp 'srefactor)
  (defun spacemacs/srefactor-enable ()
    (progn ;; "Keybinding: srefactor"
      ;; Very helpful mode
      (define-key c-mode-map   (kbd "M-RET") 'srefactor-refactor-at-point)
      (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)))

  (defun spacemacs/srefactor-disable ()
    (progn ;; "Keybinding: srefactor"
      ;; Very helpful mode
      (define-key c-mode-map   (kbd "M-RET") nil)
      (define-key c++-mode-map (kbd "M-RET") nil)))

  (spacemacs/srefactor-enable))

(when (configuration-layer/package-usedp 'srefactor-lisp)
  (defun spacemacs/srefactor-lisp-enable ()
    (progn ;; "Keybinding: srefactor-lisp"
      ;;{{ srefactor-lisp
      (global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
      (global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
      (global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
      (global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)))

  (defun spacemacs/srefactor-lisp-disable ()
    (progn ;; "Keybinding: srefactor-lisp"
      (global-unset-key (kbd "M-RET o") 'srefactor-lisp-one-line)
      (global-unset-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
      (global-unset-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
      (global-unset-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)))

  (spacemacs/srefactor-lisp-enable))
