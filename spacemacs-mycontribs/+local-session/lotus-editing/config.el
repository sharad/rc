;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defun lotus-editing/init-delsel-init ())

(defun lotus-editing/init-delsel-config ()
  (setq select-active-regions 'only)
  (delete-selection-mode 1))


(defun lotus-editing/init-common-win-init ())

(defun lotus-editing/init-common-win-config ())


(defun lotus-editing/init-light-symbol-init ())

(defun lotus-editing/init-light-symbol-config ())


(defun lotus-editing/init-hilit-chg-init ())

(defun lotus-editing/init-hilit-chg-config ()
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
                            (highlight-changes-mode t))))
                            ;; (... other stuff for setting up C mode ...)
      ;;}}

      ;;{{
      (defun DE-highlight-changes-rotate-faces ()
        (let ((toggle (eq highlight-changes-mode 'passive)))
          (when toggle (highlight-changes-mode t))
          (highlight-changes-rotate-faces)
          (when toggle (highlight-changes-mode nil))))

      ; Example for c-mode-hook:
      (add-hook 'c-mode-hook
                #'(lambda ()
                    (add-hook 'local-write-file-hooks 'DE-highlight-changes-rotate-faces)
                    (highlight-changes-mode t)
                    (highlight-changes-mode nil)))
                            ;; (... other stuff for setting up C mode ...)
      ;;}}

      ;;{{
      ;; Following function can make the highlight vanish after save file --coldnew
      (defun highlight-changes-remove-after-save ()
        "Remove previous changes after save."
        (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook
                  #'(lambda ()
                      (highlight-changes-remove-highlight (point-min) (point-max))))))
      ;;}}


(defun lotus-editing/init-highlight-symbol-init ())

(defun lotus-editing/init-highlight-symbol-config ()
  (when nil
    (add-hook 'prog-mode-hook
              #'(lambda () (highlight-symbol-mode t)))))


(defun lotus-editing/init-symbol-overlay-init ())

(defun lotus-editing/init-symbol-overlay-config ()
  (progn
    (defun enable-symbol-overlay-mode ()
      (unless (or (minibufferp)
                  (derived-mode-p 'magit-mode)
                  (derived-mode-p 'xref--xref-buffer-mode))
        (symbol-overlay-mode t)))
    (define-global-minor-mode global-symbol-overlay-mode ;; name of the new global mode
      symbol-overlay-mode                                ;; name of the minor mode
      enable-symbol-overlay-mode))
  (progn
    (global-symbol-overlay-mode)))


(defun lotus-editing/init-common-win-init ())

(defun lotus-editing/init-common-win-config ()
  (setq   x-select-enable-clipboard t
          x-select-enable-primary t
          ;; http://www.emacswiki.org/emacs/CopyAndPaste#toc5
          select-active-regions 'only))


(defun lotus-editing/init-light-symbol-init ())

(defun lotus-editing/init-light-symbol-config ())
  ;; http://stackoverflow.com/a/385676/341107
  ;; (add-element-to-lists 'light-symbol-mode pgm-langs)
  ;; (light-symbol-mode 1) - not works


(defun lotus-editing/init-show-wspace-init ())

(defun lotus-editing/init-show-wspace-config ())


(defun lotus-editing/init-paren-init ())

(defun lotus-editing/init-paren-config ()
  (progn
    ;; http://www.emacswiki.org/emacs/ShowParenMode#toc1
    (setq
     blink-matching-paren t)
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
                                         (char-equal (char-syntax cb) ?\)
                                             (blink-matching-open)))))
                (when (and
                       matching-text
                       (stringp matching-text))
                  (message matching-text)))))
          (progn
            (when t
              (ad-remove-advice 'show-paren-function 'after 'show-matching-paren-offscreen))))))))


(defun lotus-editing/init-autorevert-init ())

(defun lotus-editing/init-autorevert-config ()
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

    (setq auto-revert-use-notify                t       ;default
          auto-revert-notify-exclude-dir-regexp (auto-revert-notify-exclude-dir-regexp-add-regex (concat "\\|" "^" (expand-file-name "." "~") "/$")))))


(defun lotus-editing/post-init-parinfer-init ())

(defun lotus-editing/post-init-parinfer-config ()
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
      (put 'open-line 'delete-selection t)

      (progn
        ;; (get 'parinfer-yank 'delete-selection)
        ;; (get 'parinfer-smart-yank:yank 'delete-selection)
        (put 'parinfer-yank 'delete-selection 'yank)
        (put 'parinfer-smart-yank:yank 'delete-selection 'yank)))))


