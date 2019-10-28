;;; packages.el --- basic-startup layer packages file for Spacemacs.
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
;; added to `basic-startup-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `basic-startup/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `basic-startup/pre-init-PACKAGE' and/or
;;   `basic-startup/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-basic-startup-packages
  '(
    ;; (basic-utils :location local)
    ;; (init-setup :location local)
    ;; (utils-custom :location local)
    startup-hooks
    sessions-unified
    elscreen
    lotus-wrapper)

  "The list of Lisp packages required by the basic-startup layer.

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

;; (defun lotus-basic-startup/init-basic-utils ()
;;   (use-package basic-utils
;;       ;; :ensure t
;;       ;; :defer t
;;       :demand t
;;       :commands (add-to-hook)
;;       :config
;;     (progn
;;       )))

;; (defun lotus-basic-startup/init-init-setup ()
;;   (use-package init-setup
;;       ;; :ensure t
;;       :config
;;       (progn
;;         )))

;; (defun lotus-basic-startup/init-utils-custom ()
;;   (use-package utils-custom
;;     ;; :ensure t
;;     :config
;;     (progn)))


(defun lotus-basic-startup/init-startup-hooks ()
  (use-package startup-hooks
    ;; :ensure t
    ;; :demand t
    :init
    (add-hook 'after-init-hook #'startup-hooks-insinuate)
    :defer t
    :commands (startup-hooks-insuniate)
    :config
    (progn
      (setq
       lotus-disable-startup-begin-debug-on-error        nil
       lotus-disable-startup-finish-debug-on-error       nil
       lotus-enable-startup-begin-debug-on-error         nil
       lotus-enable-startup-finish-debug-on-error        nil

       lotus-disable-login-session-begin-debug-on-error  nil
       lotus-disable-login-session-finish-debug-on-error nil
       lotus-enable-login-session-begin-debug-on-error   nil
       lotus-enable-login-session-finish-debug-on-error  nil

       lotus-disable-startup-begin-debug-on-quit         nil
       lotus-disable-startup-finish-debug-on-quit        nil
       lotus-enable-startup-begin-debug-on-quit          nil
       lotus-enable-startup-finish-debug-on-quit         nil

       lotus-disable-login-session-begin-debug-on-quit   nil
       lotus-disable-login-session-finish-debug-on-quit  nil
       lotus-enable-login-session-begin-debug-on-quit    nil
       lotus-enable-login-session-finish-debug-on-quit   nil))))



(defun lotus-basic-startup/init-sessions-unified ()
  (use-package startup-hooks)
  (use-package sessions-unified
    ;; :ensure t
    :defer t
    :init
    (progn
      (progn
        (defun lotus-construct-desktop-filename-regex-function ()
          (concat "^" (expand-file-name
                       (concat ".cache/" (file-name-nondirectory (directory-file-name session-unified-dir)))
                       user-emacs-directory)
                  "/"  "desktop" "/emacs-desktop-" server-name))
        (setq session-unified-dir (expand-file-name ".cache/session-unified" user-emacs-directory))
        (setq desktop-path        (expand-file-name "desktop/" session-unified-dir))
        (setq desktop-dirname     (expand-file-name "desktop/" session-unified-dir))
        (setq session-save-file   (expand-file-name "session/session.el" session-unified-dir)))
      (when t
        (with-eval-after-load "startup-hooks"
          (add-to-enable-startup-interrupting-feature-hook
           #'frame-session-restore-hook-func
           t)
          (add-to-enable-startup-interrupting-feature-hook
           '(lambda ()
              (run-at-time-or-now 7 'lotus-desktop-session-restore)))
          (add-to-enable-startup-interrupting-feature-hook
           '(lambda ()
              (run-at-time-or-now 70
                                  '(lambda ()
                                     (call-interactively
                                      'lotus-check-session-saving))))))))
    :commands (lotus-desktop-session-restore)
    :config
    (progn

      (when t
        (progn

          (setq *session-unified-desktop-enabled* t)

          (setq *frame-session-restore-screen-display-function*
                #'(lambda ()
                    (with-temp-buffer
                      (spacemacs-buffer/goto-buffer))))))

      (when t
        (progn
          (add-to-disable-desktop-restore-interrupting-feature-hook
           '(lambda ()
              (setq
               tags-add-tables nil)
              (setq vc-handled-backends (remove 'P4 vc-handled-backends))
              (when (fboundp 'spacemacs/check-large-file)
                (remove-hook
                 'find-file-hook
                 'spacemacs/check-large-file))))
          (add-to-enable-desktop-restore-interrupting-feature-hook
           '(lambda ()
              (setq
               tags-add-tables (default-value 'tags-add-tables))
              (add-to-list 'vc-handled-backends 'P4)
              (when (fboundp 'spacemacs/check-large-file)
                (add-hook
                 'find-file-hook
                 'spacemacs/check-large-file))))))

      (when t
        (progn
          (with-eval-after-load "utils-custom"
            (setq sessions-unified-utils-notify 'message-notify))))
      (when t
        (progn
          (with-eval-after-load "startup-hooks"
            (add-to-enable-startup-interrupting-feature-hook
             'frame-session-restore-hook-func
             t)
            (add-to-enable-startup-interrupting-feature-hook
             '(lambda ()
                (run-at-time-or-now 7 'lotus-desktop-session-restore)))

            (add-to-enable-startup-interrupting-feature-hook
             '(lambda ()
                (run-at-time-or-now 70
                                    '(lambda ()
                                       (call-interactively
                                        'lotus-check-session-saving))))))))

      (when t
        (progn
          (add-hook 'session-unified-save-all-sessions-before-hook' clean-buffer-list))))

    (when t
      (use-package init-setup
        ;; :ensure t
        :defer t
        :config
        (progn
          (progn
            (use-package startup-hooks
              :defer t
              :config
              (progn
                (progn
                  (add-to-enable-login-session-interrupting-feature-hook
                   #'set-dbus-session)
                  (add-to-enable-startup-interrupting-feature-hook
                   #'set-dbus-session)
                  (with-eval-after-load "utils-custom"
                    (add-hook 'emacs-startup-hook
                              '(lambda ()
                                 (message-notify "Emacs" "Loaded Completely :)")
                                 (message "\n\n\n\n")))))))))))))

(defun lotus-basic-startup/init-elscreen ()
  (use-package elscreen
    :defer t
    :init
    (elscreen-start)
    :config
    (progn
      (defun elscreen-move-right ()
        (interactive)
        (elscreen-next)
        (elscreen-swap)
        (elscreen-notify-screen-modification))

      (defun elscreen-move-left ()
        (interactive)
        (elscreen-previous)
        (elscreen-swap)
        ;; (elscreen-next)
        (elscreen-notify-screen-modification)))))

(defun lotus-basic-startup/init-lotus-wrapper ()
  (progn
    (progn
      (use-package lotus-wrapper
          :defer t
          :config
          (progn
            (progn
              ))))
    (progn
      (progn
        (use-package startup-hooks
            :defer t
            :config
            (progn
              (add-to-enable-startup-interrupting-feature-hook #'lotus-wrapper-insinuate))))
      (progn ;; Need it.
        ;; (warn  "running lotus-wrapper-insinuate")
        ;; (add-hook 'after-init-hook #'lotus-wrapper-insinuate)
        (lotus-wrapper-insinuate)))))

;;; packages.el ends here
