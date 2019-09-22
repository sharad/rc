;;; misc-unified.el --- Misc unified                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <spratap@merunetworks.com>
;; Keywords: convenience, internal, tools

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

(provide 'misc-unified)



(require 'sessions-unified)


(require 'savehist-20+)


(when (featurep 'savehist-20+)
  ;; savehist: save some history
  (setq savehist-additional-variables    ;; also save...
        '(search ring regexp-search-ring)    ;; ... my search entries
        savehist-autosave-interval 60        ;; save every minute (default: 5 min)
        savehist-file (auto-config-file "savehist/savehist.el"))   ;; keep my home clean
  ;; do customization before activation
  (savehist-mode t))

;; TODO: find it
;; (require 'workspaces)

;; (deh-require-maybe desktop-recover
;;   ;; ssee:http://www.emacswiki.org/emacs/DesktopRecover
;;   ;; from: https://github.com/doomvox/desktop-recover/blob/master/desktop-recover-setup.el
;;   ;; And this brings up the interactive buffer restore menu
;;   (desktop-recover-interactive))

;; (deh-require-maybe desktopaid
(testing
  ;; see: http://desktopaid.sourceforge.net/
  (dta-hook-up))

;; (deh-require-maybe frame-restore
;;   ;; check this library will know what to do.
;;   ;; http://www.emacswiki.org/emacs/frame-restore.el
;;   )

(require 'revive)
(require 'tapestry)

;; first test it with startup
;; (deh-require-maybe winner
;;   ;; see: http://emacs.wordpress.com/2007/01/28/simple-window-configuration-management/
;;   (winner-mode 1))

(with-eval-after-load "tapestry"
  ;; http://superuser.com/questions/383560/how-to-restore-emacs-windows-and-buffers-from-the-last-session
  (defvar my-tapestry-file "~/.tapestry")

  (defun load-my-tapestry ()
    (interactive)
    (let ((b (find-file-noselect my-tapestry-file)))
      (sit-for 0)
      (set-tapestry (read b))
      (kill-buffer b)))

  (defun save-my-tapestry ()
    (interactive)
    (let ((tap (tapestry)))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (setcar tap (make-list (length (car tap)) nil))
          (print tap)
          (write-region (point-min) (point-max) my-tapestry-file)))))

  (add-hook 'kill-emacs-hook
            #'save-my-tapestry))


(progn ;; "emacs session management"
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Session-Management.html
  ;; {{
  ;; 39.17 Session Management

  ;; Emacs supports the X Session Management Protocol, which is used
  ;; to suspend and restart applications. In the X Window System, a
  ;; program called the session manager is responsible for keeping
  ;; track of the applications that are running. When the X server
  ;; shuts down, the session manager asks applications to save their
  ;; state, and delays the actual shutdown until they respond. An
  ;; application can also cancel the shutdown.

  ;; When the session manager restarts a suspended session, it directs
  ;; these applications to individually reload their saved state. It
  ;; does this by specifying a special command-line argument that says
  ;; what saved session to restore. For Emacs, this argument is
  ;; ‘--smid session’.  — Variable: emacs-save-session-functions

  ;; Emacs supports saving state via a hook called
  ;; emacs-save-session-functions. Emacs runs this hook when the
  ;; session manager tells it that the window system is shutting
  ;; down. The functions are called with no arguments, and with the
  ;; current buffer set to a temporary buffer. Each function can use
  ;; insert to add Lisp code to this buffer. At the end, Emacs saves
  ;; the buffer in a file, called the session file.

  ;; Subsequently, when the session manager restarts Emacs, it loads
  ;; the session file automatically (see Loading). This is performed
  ;; by a function named emacs-session-restore, which is called during
  ;; startup. See Startup Summary.

  ;; If a function in emacs-save-session-functions returns non-nil,
  ;; Emacs tells the session manager to cancel the shutdown.

  ;; Here is an example that just inserts some text into *scratch*
  ;; when Emacs is restarted by the session manager.

  ;; (add-hook 'emacs-save-session-functions 'save-yourself-test)

  (defun save-yourself-test ()
    (insert "(save-current-buffer
       (switch-to-buffer \"*scratch*\")
       (insert \"I am restored\"))")
    nil))

;; }}


(progn ;; "undo-history"
  ;; http://stackoverflow.com/questions/2985050/is-there-any-way-to-have-emacs-save-your-undo-history-between-sessions

  (progn ;; "undo funs"
    (defun save-undo-filename (orig-name)
      "given a filename return the file name in which to save the undo list"
      (concat (file-name-directory orig-name)
              "."
              (file-name-nondirectory orig-name)
              ".undo"))

    (defun save-undo-list ()
      "Save the undo list to a file"
      (save-excursion
        (ignore-errors
          (let ((undo-to-save `(setq buffer-undo-list ',buffer-undo-list))
                (undo-file-name (save-undo-filename (buffer-file-name))))
            (find-file undo-file-name)
            (erase-buffer)
            (let (print-level
                  print-length)
              (print undo-to-save (current-buffer)))
            ;; (let ((write-file-hooks (remove 'save-undo-list write-file-hooks)))
            (let ((write-file-functions (remove 'save-undo-list write-file-functions)))
              (save-buffer))
            (kill-buffer))))
      nil)

    (defvar handling-undo-saving nil)

    (defun load-undo-list ()
      "load the undo list if appropriate"
      (ignore-errors
        (when (and
               (not handling-undo-saving)
               (null buffer-undo-list)
               (file-exists-p (save-undo-filename (buffer-file-name))))
          (let* ((handling-undo-saving t)
                 (undo-buffer-to-eval (find-file-noselect (save-undo-filename (buffer-file-name)))))
            (eval (read undo-buffer-to-eval)))))))

    ;; (add-hook 'write-file-hooks 'save-undo-list)

    ;; (remove-hook 'write-file-functions 'save-undo-list)
    ;; (remove-hook 'find-file-hook 'load-undo-list)



  (add-to-list 'desktop-locals-to-save 'buffer-undo-list)
  (add-to-list 'session-locals-include 'buffer-undo-list)
  (setq undo-tree-auto-save-history t))



;;; misc-unified.el ends here
