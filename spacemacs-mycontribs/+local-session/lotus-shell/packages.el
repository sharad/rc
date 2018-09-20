;;; packages.el --- lotus-shell layer packages file for Spacemacs.
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
;; added to `lotus-shell-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-shell/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-shell/pre-init-PACKAGE' and/or
;;   `lotus-shell/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-shellS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-shell-packages
  '(
    (oneliner :location local)
    eshell
    )
  "The list of Lisp packages required by the lotus-shell layer.

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

(defun lotus-shell/init-oneliner ()
  (use-package oneliner
    :defer t
    :config
    (progn

      (defvar buf nil)

      ;; (load-library "oneliner")
      (message "correct it in lotus-shell")
      ;; (require 'utils-config)
      ;; (require 'tramp-config)

      (setq oneliner-temp-dir-name (if (getenv "TMPDIR")
                                       (expand-file-name (getenv "TMPDIR"))
                                     (expand-file-name "temp" (getenv "HOME"))
                                     ;; (error "test")
                                     )
            oneliner-debug t
            oneliner-shell-type 'zsh
            oneliner-sync-default-directory-after-prompt t)


      (defadvice oneliner (around shell-call-shell-file-name activate)
        ;; I can not make (env ESELL) to zsh, becasue of tramp
        ;; see in tramp-config.el
        ;; oneliner choose shell-type by shell-file-name
        "make (shell) to prefer shell-file-name"
        (let ((explicit-shell-file-name shell-file-name))
          ad-do-it))

      (defadvice oneliner-for-dir (around shell-call-shell-file-name activate)
        ;; I can not make (env ESELL) to zsh, becasue of tramp
        ;; see in tramp-config.el
        ;; oneliner choose shell-type by shell-file-name
        "make (shell) to prefer shell-file-name"
        (let ((explicit-shell-file-name shell-file-name))
          ad-do-it))


      ;; (defun cd-tramp-absolute (dir &optional base-directory)
      ;;   (let* ((tramp-prefix "\\`/[^/]+[@:][^:/]+:")
      ;;          (base-directory (or base-directory default-directory))
      ;;          (prefix (if (string-match tramp-prefix base-directory)
      ;;                      (match-string 0 base-directory)))
      ;;          (dir (concat  prefix dir)))
      ;;     (cd-absolute dir)))


      (defun cd-tramp-absolute (dir &optional base-directory)
        (let* ((base-directory (or base-directory default-directory))
               (prefix (tramp-file-prefix base-directory))
               (dir (concat  prefix dir)))
          (cd-absolute dir)))



      (defun get-tramp-env (variable)
        (with-temp-buffer
          (process-file "bash" nil t nil "-c" (concat "echo $" variable) )
          (trim-string (buffer-string))))


      (defun shell-process-cd (arg) ;; redefining shell.el.gz function
        (let ((new-dir (cond ((zerop (length arg)) (concat comint-file-name-prefix
                                                           (get-tramp-env "HOME")))
                             ((string-equal "-" arg) shell-last-dir)
                             (t (shell-prefixed-directory-name arg)))))
          (setq shell-last-dir default-directory)
          (shell-cd new-dir)
          (shell-dirstack-message)))


      (defun oneliner-tramp-send-cd (arg &optional dir)
        "Change directory of *Oneliner shell* to current buffer's `default-directory'."
        (interactive "p")
        (message "Hello")
        (let ((curdir (or dir default-directory)))
          (oneliner-invisible-command-exec
           (concat "cd "
                   (file-name-localname curdir)))
          (when (called-interactively-p 'any) ;;(interactive-p)
            (message "Send to %s buffer 'cd %s'" (buffer-name oneliner-shell-buffer)
                     curdir))))


      (defvar oneliners-list nil "Multiple oneliners")

      ;; (defadvice tramp-open-connection-setup-interactive-shell
      ;;     (after start-oneliner last (p vec) activate)
      ;;   (let ((prefix (tramp-connection-prefix vec)))
      ;;     (unless (member prefix oneliners-list)
      ;;       (push prefix oneliners-list)
      ;;       (let ((oneliner-suffix prefix))
      ;;         (oneliner)))))

      ;; (defadvice tramp-open-connection-setup-interactive-shell
      ;;     (after start-oneliner last (p vec) activate)
      ;;   (save-window-excursion
      ;;     ;; check if save-excrusion is required.
      ;;     (oneliner-for-dir
      ;;      (file-name-directory
      ;;       (tramp-connection-file vec)))))


      (when nil
        (defadvice tramp-open-connection-setup-interactive-shell
            (after start-oneliner last (p vec) disable)
          ;; see function tramp-sh-file-name-handler in tramp-sh.el
          (unless (and tramp-locked (not tramp-locker)) ;; (or tramp-locked tramp-locker)
            (let* ((prefix (tramp-connection-prefix vec))
                   (file (tramp-connection-file vec))
                   (dir (if (file-directory-p file)
                            file
                          (file-name-directory file)))
                   (onelinerbuf (make-oneliner-shell-buffer-name dir)))
              (save-window-excursion
                (unless (member prefix oneliners-list)
                  (push prefix oneliners-list)
                  (oneliner-for-dir dir))
                (if (bufferp onelinerbuf)
                    (with-current-buffer onelinerbuf
                      (oneliner-tramp-send-cd dir))))))
          (message "start-oneliner: tramp-locked %s tramp-locker %s" tramp-locked tramp-locker)
          ad-return-value)

        (add-to-enable-startup-interrupting-feature-hook
         #'(lambda ()
                     (ad-enable-advice 'tramp-open-connection-setup-interactive-shell 'after 'start-oneliner)
                     (ad-update 'tramp-open-connection-setup-interactive-shell)
                     (ad-activate 'tramp-open-connection-setup-interactive-shell))
                  t)

        (when nil
          (ad-remove-advice 'tramp-open-connection-setup-interactive-shell 'after 'start-oneliner)
          (ad-update 'tramp-open-connection-setup-interactive-shell)
          (ad-activate 'tramp-open-connection-setup-interactive-shell))))))

(defun lotus-shell/post-init-eshell ()
  (use-package eshell
    :defer t
    :config
    (progn
      ;; checkout: http://snarfed.org/why_i_run_shells_inside_emacs
      )))

;;; packages.el ends here
