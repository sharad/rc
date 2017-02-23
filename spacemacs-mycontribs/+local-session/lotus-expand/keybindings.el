
(when (configuration-layer/package-usedp 'auto-complete)
  (defun spacemacs/auto-complete-enable ()
    (progn ;; "Keybinding: Auto-Complete"
      ;;{{ auto-complete
      (use-package auto-complete
          :defer t
          :config
          (progn
            (define-key ac-mode-map (kbd "C-M-<tab>") 'auto-complete)))

      ;; (global-unset-key [C-z])
      ;; (global-set-key [] 'auto-complete-create)

      (use-package aut-complete-clang
          :defer t
          :config
          (progn
            (defun my-ac-clang-mode-common-hook ()
              (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang))

            (use-package cc-vars
                :defer t
                :config
                (add-hook 'c-mode-common-hook 'my-ac-clang-mode-common-hook))))))

  (defun spacemacs/auto-complete-disable ()
    (progn ;; "Keybinding: Auto-Complete"
      (when (and
             (boundp 'ac-mode-map)
             (keymapp ac-mode-map))
       (define-key ac-mode-map (kbd "C-M-<tab>") nil))

      (use-package cc-vars
          :defer t
          :config
          (remove-hook 'c-mode-common-hook 'my-ac-clang-mode-common-hook))
      ;; (global-unset-key [])
      ))

  (spacemacs/auto-complete-enable))

(when (configuration-layer/package-usedp 'pde-indent-dwim)
  (defun spacemacs/pde-indent-dwim-enable ()
    (progn ;; "Keybinding: Pde-Indent-Dwim"
      ;;{{ pde-indent-dwim
      (global-set-key-if-unbind (kbd "C-M-=") 'pde-indent-dwim)))

  (defun spacemacs/pde-indent-dwim-disable ()
    (progn ;; "Keybinding: Pde-Indent-Dwim"
      (global-unset-key-if-bind (kbd "C-M-=") 'pde-indent-dwim)))

  (spacemacs/pde-indent-dwim-enable))

(when (configuration-layer/package-usedp 'comint)
  (defun spacemacs/comint-enable ()
    (progn ;; "Keybinding: Comint"
      ;;{{ comint
      (global-set-key-if-unbind (kbd "M-;") 'comment-dwim)
      (global-set-key-if-unbind (kbd "C-c f") 'comint-dynamic-complete)))

  (defun spacemacs/comint-disable ()
    (progn ;; "Keybinding: Comint"
      (global-unset-key-if-bind (kbd "M-;") 'comment-dwim)
      (global-unset-key-if-bind (kbd "C-c f") 'comint-dynamic-complete)))

  (spacemacs/comint-enable))

(when (configuration-layer/package-usedp 'hippie-exp)
  (defun spacemacs/hippie-exp-enable ()
    (progn ;; "Keybinding: Hippie-Exp"
      ;;{{ hippie-exp
      (global-set-key-if-unbind (kbd "<C-tab>") 'hippie-expand)))

  (defun spacemacs/hippie-exp-disable ()
    (progn ;; "Keybinding: Hippie-Exp"
      (global-unset-key-if-bind (kbd "<C-tab>") 'hippie-expand)))

  (spacemacs/hippie-exp-enable))
