;;; wrappers-config.el --- wrapper

;; Copyright (C) 2013  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords:lisp

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

;; emacs24 server-execute
(if (>= emacs-major-version 24)
    (defun server-execute (proc files nowait commands dontkill frame tty-name)
      ;; This is run from timers and process-filters, i.e. "asynchronously".
      ;; But w.r.t the user, this is not really asynchronous since the timer
      ;; is run after 0s and the process-filter is run in response to the
      ;; user running `emacsclient'.  So it is OK to override the
      ;; inhibit-quit flag, which is good since `commands' (as well as
      ;; find-file-noselect via the major-mode) can run arbitrary code,
      ;; including code that needs to wait.
      (with-local-quit
        (condition-case err
            (let ((buffers (when files
                             (run-hooks 'pre-command-hook)
                             (prog1 (server-visit-files files proc nowait)
                               (run-hooks 'post-command-hook)))))
              (mapc 'funcall (nreverse commands))

              ;; If we were told only to open a new client, obey
              ;; `initial-buffer-choice' if it specifies a file.
              (unless (or files commands)
                (if (stringp initial-buffer-choice)
                    (find-file initial-buffer-choice)
                    (switch-to-buffer (get-buffer-create "*scratch*")
                                      'norecord)))

              ;; Delete the client if necessary.
              (cond
                (nowait
                 ;; Client requested nowait; return immediately.
                 (server-log "Close nowait client" proc)
                 (server-delete-client proc))
                ((and (not dontkill) (null buffers))
                 ;; This client is empty; get rid of it immediately.
                 (server-log "Close empty client" proc)
                 (server-delete-client proc)))
              (cond
                ((or isearch-mode (minibufferp))
                 nil)
                ((and frame (null buffers))
                 (message "%s" (substitute-command-keys
                                "When done with this frame, type \\[delete-frame]")))
                ((not (null buffers))
                 (server-switch-buffer (car buffers) nil (cdr (car files)))
                 (run-hooks 'server-switch-hook)
                 (unless nowait
                   (message "%s" (substitute-command-keys
                                  "When done with a buffer, type \\[server-edit]")))))
              (when (and frame (null tty-name))
                (server-unselect-display frame)))
          ((quit error)
           (when (eq (car err) 'quit)
             (message "Quit emacsclient request"))
           (server-return-error proc err))))))


(if (< emacs-major-version 24)
    (defun custom-display-graphic-p ()
      (eq (frame-parameter (selected-frame) 'window-system) 'x))
    (defun custom-display-graphic-p ()
      (display-graphic-p)))

(provide 'wrappers-config)
;;; wrappers-config.el ends here
