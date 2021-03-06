;;; packages.el --- lotus-timeclock layer packages file for Spacemacs.
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
;; added to `lotus-timeclock-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-timeclock/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-timeclock/pre-init-PACKAGE' and/or
;;   `lotus-timeclock/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-timeclockS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-timeclock-packages
  '(
    timeclock
    (timeclock-x :location local)
    )
  "The list of Lisp packages required by the lotus-timeclock layer.

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

(defun lotus-timeclock/init-timeclock ()
  (use-package timeclock
    :defer t
    :config
    (progn
      ;; X-URL: http://obsidianrook.com/codelife/timetrack.html
      ;; X-URL: http://obsidianrook.com/codelife/code/timeclock-setup.el
      ;; (define-key ctl-x-map "ti" 'timeclock-in)
      ;; (define-key ctl-x-map "to" 'timeclock-out)
      ;; (define-key ctl-x-map "tc" 'timeclock-change)
      ;; (define-key ctl-x-map "tr" 'timeclock-reread-log)
      ;; (define-key ctl-x-map "tu" 'timeclock-update-modeline)
      ;; (define-key ctl-x-map "tw" 'timeclock-when-to-leave-string)

      ;; define this before we use it below...
      (defun timeclock-additional-setup ()
        "Some additional set-up (timeclock-x squared?).
Creates a ~/.timeclock/default.log if it doesn't exist already."
        (interactive)
        (let* ((location (substitute-in-file-name "$HOME/.timeclock"))
               (log-file-base "default.log")
               (log-file (concat location "/" log-file-base)))

          (unless (file-exists-p location)
            (make-directory location t))

          (unless (file-exists-p log-file)
            (save-excursion
              (switch-to-buffer log-file t)
              (write-file log-file t) ;; additional safety: ask for confirmation on over-write
              (kill-buffer log-file-base)
              ))))

      (defun timeclock-x-initialize ()
        (interactive)
        (timeclock-additional-setup)
        (timeclock-initialize)
        (timeclock-setup-keys)
        ;; (timeclock-modeline-display 1) ;; if you want modline display
        (timeclock-mode-line-display 1))




      (defun timeclock-insert-project-hours-report  ()
        "Insert a daily report of hours spent on each project.
Summarizes the data in the timeclock log."
        (interactive)
        (let* ((command "~/bin/timeclock_project_hours_report")
               (report (shell-command-to-string command) ))
          (insert report)))

      ;; TODO
      (unless (and (boundp 'sup-t-map)
                   (keymapp sup-t-map))
        ;; (setq sup-t-map (make-sparse-keymap))
        (define-prefix-command 'sup-t-map)
        ;; (local-set-key (kbd "s-t") 'sup-t-map)
        (global-set-key (kbd "s-t") 'sup-t-map))

      ;; (global-set-key "\C-xtR" 'timeclock-insert-project-hours-report)
      ;; (global-set-key "\C-ctR" 'timeclock-insert-project-hours-report)
      (define-key sup-t-map "tR" 'timeclock-insert-project-hours-report)

      (defun timeclock-display-project-names  ()
        "Displays project names in use in the timeclock log."
        (interactive)
        (let* ((command "~/bin/timeclock_project_names")
               (report (shell-command-to-string command)))
          (delete-other-windows)
          (split-window-vertically)
          (other-window 1)
          (switch-to-buffer "*timeclock projects*")
          (mark-whole-buffer)
          (delete-region (mark) (point))
          (insert report)
          (set-mark-command (point)) ; just want to deactivate region

          ;; TODO switch to an appropriate mode
          ;;  (mh-folder-list-mode)
          ))

      ;; (global-set-key "\C-xtN" 'timeclock-display-project-names)
      ;; (global-set-key "\C-ctN" 'timeclock-display-project-names)
      (define-key sup-t-map "tR" 'timeclock-display-project-names))))

(defun lotus-timeclock/init-timeclock-x ()
  (use-package timeclock-x
    :defer t
    :config
    (progn
      )))

;;; packages.el ends here
