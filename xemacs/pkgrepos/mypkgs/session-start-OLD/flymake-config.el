;;; flymake-config.el --- Flymake Config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords: lisp

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

(deh-require-maybe flymake
  (setq
   ;; http://stackoverflow.com/questions/2571436/emacs-annoying-flymake-dialog-box
   ;; flymake-gui-warnings-enabled nil       ;need to know.
   flymake-gui-warnings-enabled nil       ;need to know.
   flymake-run-in-place
   ;; https://github.com/illusori/emacs-flymake/issues/1
   t)

  (deh-require-maybe warnings
    ;; Overwrite flymake-display-warning so that no annoying dialog box is
    ;; used.

    ;; This version uses lwarn instead of message-box in the original version.
    ;; lwarn will open another window, and display the warning in there.
    (defun flymake-display-warning (warning)
      "Display a warning to the user, using lwarn"
      (lwarn 'flymake :warning warning))

    ;; Using lwarn might be kind of annoying on its own, popping up windows and
    ;; what not. If you prefer to recieve the warnings in the mini-buffer, use:
    (defun flymake-display-warning (warning)
      "Display a warning to the user, using lwarn"
      (message warning)))

  (deh-require-maybe flymake-cursor
    ;; http://www.emacswiki.org/emacs/flymake-cursor.el
    )


  ;;;; general init-cleanup and helper routines
  ;; TODO: rename these to something sane and deprecate the current names.
  (defun flymake-create-temp-copy (file-name prefix) ;source of sorrow with desktop-vc-read
    "Make filename for a temporary copy of FILE-NAME.

If `flymake-run-in-place' is true it will use `flymake-create-temp-inplace',
otherwise it will use `flymake-create-temp-intemp'.

Note that this function, despite its name, does not actually create a
copy of the file: it only choses and returns a filename for the temp
copy."
    (if (and flymake-run-in-place
             (file-writable-p (dirname-of-file file-name)))
        (flymake-create-temp-inplace file-name prefix)
        (flymake-create-temp-intemp file-name prefix)))

  (eval-when-compile
    '(progn
      (require 'session-config)
      (deh-require-maybe session-config
        (add-to-list 'desktop-minor-mode-handlers (cons 'flymake-mode
                                                        (desktop-get-readonly-proof-mode flymake-mode))))))
  (require 'session-config)

  ;; (deh-require-maybe session-config
  ;;   (add-to-list 'desktop-minor-mode-handlers (cons 'flymake-mode
  ;;                                                   (desktop-get-readonly-proof-mode flymake-mode))))




  )

;; http://stackoverflow.com/questions/20377288/setup-makefile-to-check-both-c-and-c-source-files-with-emacs-flymake
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

(provide 'flymake-config)
;;; flymake.el ends here
