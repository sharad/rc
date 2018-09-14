;;; packages.el --- lotus-expand layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-expand-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-expand/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-expand/pre-init-PACKAGE' and/or
;;   `lotus-expand/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-expand-packages
  '(
    ;; (PACKAGE :location local)
    pabbrev
    auto-complete
    (pde-indent-dwim :location local)
    comint
    yasnippet
    hippie-exp
    (yas-suggest :location local)
    expand-region
    )
  "The list of Lisp packages required by the lotus-expand layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-expand/init-pabbrev ()


  (when nil (require 'template-simple))



  ;; Actually TAB is originally binded to indent-for-tab-command from indent.el
  ;; But Pabbrev mode override it to pabbrev-expand-maybe that call
  ;; pabbrev-get-previous-binding -> indent-for-tab-command

  ;; M-SPC not available, window manager take it away

  ; (global-set-key-if-unbind (kbd "M-'") 'just-one-space)
  (use-package pabbrev
      :defer t
      :config
      (progn
        (progn
          (setq pabbrev-read-only-error t))
        (progn
          (with-eval-after-load "sessions-mgr"
            (add-to-list 'desktop-minor-mode-handlers
                         (cons 'pabbrev-mode
                               (desktop-get-readonly-proof-mode pabbrev-mode))))))))

(defun lotus-expand/post-init-auto-complete ()
  (use-package auto-complete
      :defer t
      :config
      (progn
        (progn
          (setq ac-comphist-file (auto-config-file "auto-complete/ac-comphist.dat"))
          ;; (define-key ac-mode-map (kbd "C-M-<tab>") 'auto-complete)
          (global-auto-complete-mode t)

          (defun my-c-mode-cedet-hook ()
            ;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
            (add-to-list 'ac-sources 'ac-source-gtags)
            (add-to-list 'ac-sources 'ac-source-semantic))
          (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook))

        (progn ;; "config autocomplete"
          ;; http://root42.blogspot.hu/2012/07/nice-c-autocomplete-configuration-for.html
          (defcustom mycustom-system-include-paths '("./include/" "/opt/local/include" "/usr/include" )
            "This is a list of include paths that are used by the clang auto completion."
            :group 'mycustom
            :type '(repeat directory))

          ;; (add-to-list 'load-path "~/bin/emacs/auto-complete")
          (use-package auto-complete-config
              ;; (add-to-list 'ac-dictionary-directories "~/bin/emacs/auto-complete/ac-dict")
              :defer t
              :config
              (progn
                (progn
                  (ac-config-default)
                  (use-package auto-complete-clang
                      :defer t
                      :config
                      (progn
                        (progn
                          (setq
                           clang-completion-suppress-error 't
                           ac-clang-flags (mapcar #'(lambda (item) (concat "-I" item)) (append mycustom-system-include-paths)))

                          (defun my-ac-clang-mode-common-hook ()
                            (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang))

                          ;; (add-hook 'c-mode-common-hook 'my-ac-clang-mode-common-hook)
                          ))))))))))

(defun lotus-expand/init-pde-indent-dwim ()
  (use-package pde-indent-dwim
      :defer t
      :config
      (progn
        ;; (global-set-key-if-unbind (kbd "C-M-=") 'pde-indent-dwim)
        )))

(defun lotus-expand/post-init-comint ()
  (use-package comint
      :defer t
      :config
      (progn
        ;; Correct create comment!
        ;; (global-set-key-if-unbind (kbd "M-;") 'comment-dwim)
        ;; (global-set-key-if-unbind (kbd "C-c f") 'comint-dynamic-complete)
        )))

(defun lotus-expand/post-init-yasnippet ()
  (use-package yasnippet
      :defer t
      :config
      (progn
        (progn
          (defun yas--keybinding-beyond-yasnippet-advice (orig-fun &rest args)
            ;; (let ((binding (apply orig-fun args)))
            (let ((binding (apply orig-fun args)))
              (if (eq binding 'pabbrev-expand-maybe)
                  'indent-for-tab-command
                binding)))

          (when (fboundp 'advice-add)
            (advice-add 'yas--keybinding-beyond-yasnippet
                        :around
                        #'yas--keybinding-beyond-yasnippet-advice))

          (when nil
            (advice-remove 'yas--keybinding-beyond-yasnippet
                           #'yas--keybinding-beyond-yasnippet-advice))

          ;; (setq-default yas-fallback-behavior '(apply indent-for-tab-command . nil))

          (setq-default yas-fallback-behavior 'call-other-command)

          ;; (autoload 'comint-dynamic-complete "comint" "Complete for file name" t)

          (setq comint-completion-addsuffix '("/" . ""))

          ;; ;; pabbrev-expand-maybe
          ;; ;; (pabbrev-get-previous-binding)

          ;; do not want it.
          ;; (setq yas/trigger-key "")
          ;; inplace of tab I want it to use C->
          (setq yas/trigger-key "C->")))))

(defun lotus-expand/post-init-hippie-exp ()
  ;; TAB used for indent-for-tab-command
  ;; Left key C-TAB C-M-TAB
  ;; nearest key to dabbrev-expand
  (use-package hippie-exp
      :defer t
      :config
      (progn
        (setq
         hippie-expand-try-functions-list
         '(yas/hippie-try-expand
           try-expand-list
           try-expand-list-all-buffers
           try-expand-dabbrev-visible
           try-expand-dabbrev-all-buffers
           try-expand-dabbrev-from-kill
           try-complete-file-name
           try-complete-file-name-partially
           try-complete-lisp-symbol
           try-complete-lisp-symbol-partially
           try-expand-whole-kill
           try-expand-dabbrev
           try-expand-line
           try-expand-line-all-buffers))

        )))

(defun lotus-expand/init-yas-suggest ()
  (use-package yas-suggest
      :defer t
      :config
      (progn
        )))

(defun lotus-expand/post-init-expand-region ()
  (use-package expand-region
      ;; https://github.com/magnars/expand-region.el
      :defer t
      :config
      (progn
        (progn
          (global-set-key (kbd "C-=") 'er/expand-region)))))

;;; packages.el ends here
