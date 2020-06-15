;;; packages.el --- lotus-editing layer packages file for Spacemacs.
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
;; added to `lotus-editing-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-editing/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-editing/pre-init-PACKAGE' and/or
;;   `lotus-editing/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-editingS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org


;; http://www.emacswiki.org/emacs/CopyAndPaste#toc5
;; (global-set-key [mouse-2] 'mouse-yank-primary)

(defconst lotus-editing-packages '((common-win :location local)
                                   (light-symbol :location local)
                                   hilit-chg
                                   highlight-symbol
                                   (show-wspace :location local)
                                   paren
                                   corral
                                   (autorevert :location local)
                                   (simple :location local)
                                   parinfer)
  "The list of Lisp packages required by the lotus-editing layer.

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

(defun lotus-editing/init-common-win ()
  (use-package common-win
      :defer t
      :config
      (progn
        (setq
         x-select-enable-clipboard t
         x-select-enable-primary t
         ;; http://www.emacswiki.org/emacs/CopyAndPaste#toc5
         select-active-regions 'only))))

(defun lotus-editing/init-light-symbol ()
  (use-package light-symbol
      :defer t
      :config
      (progn
        ;; http://stackoverflow.com/a/385676/341107
        ;; (add-element-to-lists 'light-symbol-mode pgm-langs)
        ;; (light-symbol-mode 1) - not works
        )))

(defun lotus-editing/init-hilit-chg ()
  (use-package hilit-chg
    :defer t
    :config
    (progn
      ;; (add-element-to-lists '(lambda ()
      ;;                         (light-symbol-mode 1)
      ;;                         (highlight-changes-visible-mode t)
      ;;                         (highlight-changes-mode t)) pgm-langs)
      ;; (highlight-changes-mode t) - not works
      ;; (highlight-changes-visible-mode t)

      ;;{{
      ;; http://www.emacswiki.org/emacs/TrackChanges
      (make-empty-face 'highlight-changes-saved-face)
      (setq highlight-changes-face-list '(highlight-changes-saved-face))

      ; Example: activate highlight changes with rotating faces for C programming
      (add-hook 'c-mode-hook
                (function (lambda ()
                            (add-hook 'local-write-file-hooks 'highlight-changes-rotate-faces)
                            (highlight-changes-mode t)
                            ;; (... other stuff for setting up C mode ...)
                            )))
      ;;}}

      ;;{{
      (defun DE-highlight-changes-rotate-faces ()
        (let ((toggle (eq highlight-changes-mode 'passive)))
          (when toggle (highlight-changes-mode t))
          (highlight-changes-rotate-faces)
          (when toggle (highlight-changes-mode nil))))

      ; Example for c-mode-hook:
      (add-hook 'c-mode-hook
                (function (lambda ()
                            (add-hook 'local-write-file-hooks 'DE-highlight-changes-rotate-faces)
                            (highlight-changes-mode t)
                            (highlight-changes-mode nil)
                            ;; (... other stuff for setting up C mode ...)
                            )))
      ;;}}

      ;;{{
      ;; Following function can make the highlight vanish after save file --coldnew
      (defun highlight-changes-remove-after-save ()
        "Remove previous changes after save."
        (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook
                  (lambda ()
                    (highlight-changes-remove-highlight (point-min) (point-max)))))
      ;;}}
      )))

(defun lotus-editing/init-highlight-symbol ()
  (use-package highlight-symbol
    :defer t
    :config
    (progn
      (add-hook 'prog-mode-hook #'highlight-symbol-mode))))

(defun lotus-editing/init-show-wspace ()
  ;; http://emacswiki.org/emacs/ShowWhiteSpace
  (use-package show-wspace
    :defer t
    :config
    (progn
      )))

(defun lotus-editing/init-paren ()
  (use-package paren
    :defer t
    :config
    (progn
      (progn
        ;; http://www.emacswiki.org/emacs/ShowParenMode#toc1
        (setq
         blink-matching-paren t
         ;; blink-matching-paren is a variable defined in `simple.el'.
         ;; Its value is jump

         ;; Original value was t


         ;; Documentation:
         ;; Non-nil means show matching open-paren when close-paren is inserted.
         ;; If t, highlight the paren.  If `jump', briefly move cursor to its
         ;; position.  If `jump-offscreen', move cursor there even if the
         ;; position is off screen.  With any other non-nil value, the
         ;; off-screen position of the opening paren will be shown in the
         ;; echo area.


         )

        (progn

          (when nil
            (progn
              (progn                  ;NOT REQUIRED
                ;; Hilight matching parenthesis
                ;; (unless (featurep 'xemacs) (show-paren-mode 1))
                (show-paren-mode 1)
                ;; http://www.emacswiki.org/emacs/ShowParenMode#toc1
                (defadvice show-paren-function (after show-matching-paren-offscreen activate)
                  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
                  (interactive)
                  (let* ((cb (char-before (point)))
                         (matching-text (and cb
                                             (char-equal (char-syntax cb) ?\))
                                             (blink-matching-open))))
                    (when (and
                           matching-text
                           (stringp matching-text))
                      (message matching-text)))))
              (progn
                (when t
                  (ad-remove-advice 'show-paren-function 'after 'show-matching-paren-offscreen))))))))))

(defun lotus-editing/init-corral ()
  ;; https://github.com/nivekuil/corral
  (use-package corral
    :defer t
    :config
    (progn)))


(defun lotus-editing/init-autorevert ()
  (use-package autorevert
    :defer t
    :config
    (progn
      ;; TODO: write a new correct-auto-revert-notify-add-watch for emacs-snapshot
      (when nil                         ;not workign with emacs-snapshot
        (defun correct-auto-revert-notify-add-watch ()
          "Enable file notification for current buffer's associated file."
          ;; We can assume that `buffer-file-name' and
          ;; `auto-revert-use-notify' are non-nil.
          (if (or (string-match auto-revert-notify-exclude-dir-regexp
                                (expand-file-name default-directory))
                  (and
                   buffer-file-name
                   (file-symlink-p buffer-file-name)))
              ;; Fallback to file checks.
              (set (make-local-variable 'auto-revert-use-notify) nil)

            (when (not auto-revert-notify-watch-descriptor)
              (setq auto-revert-notify-watch-descriptor
                    (ignore-errors
                      (file-notify-add-watch
                       (expand-file-name buffer-file-name default-directory)
                       '(change attribute-change) 'auto-revert-notify-handler)))
              (if auto-revert-notify-watch-descriptor
                  (progn
                    (puthash
                     auto-revert-notify-watch-descriptor
                     (cons (current-buffer)
                           (gethash auto-revert-notify-watch-descriptor
                                    auto-revert-notify-watch-descriptor-hash-list))
                     auto-revert-notify-watch-descriptor-hash-list)
                    (add-hook (make-local-variable 'kill-buffer-hook)
                              'auto-revert-notify-rm-watch))
                ;; Fallback to file checks.
                (set (make-local-variable 'auto-revert-use-notify) nil)))))

        (defalias 'auto-revert-notify-add-watch #'correct-auto-revert-notify-add-watch)

        (defun auto-revert-notify-exclude-dir-regexp-reset-default ()
          (setq auto-revert-notify-exclude-dir-regexp ;;default value
                (concat
                 ;; No mounted file systems.
                 "^" (regexp-opt '("/afs/" "/media/" "/mnt" "/net/" "/tmp_mnt/"))
                 ;; No remote files.
                 (unless auto-revert-remote-files "\\|^/[^/|:][^/|]+:"))))

        (defun auto-revert-notify-exclude-dir-regexp-add-regex (re)
          (concat auto-revert-notify-exclude-dir-regexp re))

        (setq
         auto-revert-use-notify t       ;default
         auto-revert-notify-exclude-dir-regexp (auto-revert-notify-exclude-dir-regexp-add-regex
                                                (concat "\\|" "^" (expand-file-name "." "~") "/$")))))))

(defun lotus-editing/init-simple ()
  (use-package simple
    :defer t
    :config
    (progn
      (progn
        (setq kill-whole-line t)))))

(defun lotus-editing/post-init-parinfer ()
  (use-package parinfer
    :defer t
    :config
    (progn

      (progn
        (when nil
          ;; from delsel.el
          (put 'self-insert-command 'delete-selection 'delete-selection-uses-region-p)

          (put 'insert-char 'delete-selection t)
          (put 'quoted-insert 'delete-selection t)

          (put 'yank 'delete-selection 'yank)
          (put 'clipboard-yank 'delete-selection 'yank)
          (put 'insert-register 'delete-selection t)
          ;; delete-backward-char and delete-forward-char already delete the selection by
          ;; default, but not delete-char.
          (put 'delete-char 'delete-selection 'supersede)

          (put 'reindent-then-newline-and-indent 'delete-selection t)
          (put 'newline-and-indent 'delete-selection t)
          (put 'newline 'delete-selection t)
          (put 'electric-newline-and-maybe-indent 'delete-selection t)
          (put 'open-line 'delete-selection t)))

      (progn
        ;; (get 'parinfer-yank 'delete-selection)
        ;; (get 'parinfer-smart-yank:yank 'delete-selection)
        (put 'parinfer-yank 'delete-selection 'yank)
        (put 'parinfer-smart-yank:yank 'delete-selection 'yank)))))



(defun lotus-editing/init-PACKAGE ()
  (use-package PACKAGE
    :defer t
    :config
    (progn
      )))

;;; packages.el ends here
