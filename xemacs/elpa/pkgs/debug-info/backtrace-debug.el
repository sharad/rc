;;; backtrace-debug.el --- Backtrace debug           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <spratap@merunetworks.com>
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

(provide 'backtrace-debug)


;; import utils-custom.el also may things are there
(defvar emacs-hang-load-file
  (auto-config-file "hang/hang.el")
  "emacs hang load file")


(defun backtrace-to-buffer (&optional buffer)
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Internals-of-Debugger.html
  (let ((buffer (or buffer (get-buffer-create "Test"))))
    (with-output-to-temp-buffer buffer ; "backtrace-output"
      (let ((var 1))
        (save-excursion
          (setq var (eval '(progn
                             (if (boundp 'var) (1+ var))
                             (list 'testing (backtrace))))))))))


;;;###autoload
(defun emacs-collect-states-and-log ()
  (interactive)
  (let ((backtrace-buffer (get-buffer-create "*CurrentBacktrace*"))
        (backtrace-file   (auto-config-file "backtrace/backtrace.log"))
        (message-file     (auto-config-file "message/message.log")))
    (message "(recursion-depth) = %d" (recursion-depth))
    (message "emacs-collect-states-and-log: taking backtrace in %s" backtrace-file)
    (message "emacs-collect-states-and-log: taking messages in %s"  message-file)
    (progn
      (with-current-buffer "*Messages*"
        (write-region nil t message-file t))
      (backtrace-to-buffer backtrace-buffer)
      (with-current-buffer backtrace-buffer
        (write-region nil t backtrace-file t))))
  (setq debug-on-next-call t))

(defun emacs-clean-hangup ()
  (emacs-collect-states-and-log)
  (tramp-cleanup-all-connections)
  (let ((ispell (get-process "ispell")))
    (if ispell
        (kill-process ispell)))
  (if (file-exists-p emacs-hang-load-file)
      (load-file emacs-hang-load-file)))


;;;###autoload
(defun sigusr1-handler ()
  (interactive)
  (let ((li last-input-event))
    (message "Caught signal %S" li)
    (message "(recursion-depth) = %d" (recursion-depth))
    (emacs-clean-hangup)
    (keyboard-quit)
    (message "Caught signal %S" li)))

;;;###autoload
(defun sigusr2-handler ()
  (interactive)
  (message "(recursion-depth) = %d" (recursion-depth))
  (emacs-collect-states-and-log)
  (message "Caught signal %S" last-input-event))


(defmacro with-error-trace-buffer (buf &rest body)
  `(condition-case e
       ,@body
     (error
      (print (format "Error: %s" e) (get-buffer ,buf)))))
      ;; (backtrace-to-buffer ,buf)



(defun backtrace-before-error (orig-fun &rest args)
  (apply #'message (concat "backtrace-before-error: " (car args)) (cdr args))
  (backtrace-to-buffer "*errbuf*"))


;; https://emacs.stackexchange.com/questions/28202/get-backtrace-from-error-programmatically
(defun my-debugger (&rest debugger-args)
  (message "(recursion-depth) = %d" (recursion-depth))
  (message "BACKTRACE: %s"
           (with-temp-buffer
             (let ((standard-output (current-buffer)))
               (backtrace)
               (buffer-string)))))

(defun debugger-output-to-file (&rest debugger-args)
  (let ((backtrace-buffer (get-buffer-create "*CurrentBacktrace*"))
        (backtrace-file   (auto-config-file "backtrace/backtrace.log")))
    (with-current-buffer backtrace-buffer
      (with-output-to-temp-buffer (current-buffer)
        (let ((buffer-read-only nil))
          (princ "\n\n")
          (princ (format "error: %s" debugger-args))
          (princ "\n\n")
          (backtrace)
          (write-region nil t backtrace-file t))))))

(defmacro with-debugger-output-to-file (&rest body)
  `(let ((debugger #'debugger-output-to-file))
     ,@body))

(when nil
  (let ((debugger #'my-debugger))
    (foobar))
  (with-debugger-output-to-file (foobar))
  (let ((debugger #'debugger-output-to-file))
    (foobar))) ; Runs a function with no definition!)


;;;###autoload
(defun debug-info-error-backtrace-insinuate ()
  (interactive)
  (advice-add 'error :before #'backtrace-before-error))
;;;###autoload
(defun debug-info-error-backtrace-uninsinuate ()
  (interactive)
  (advice-remove 'error #'backtrace-before-error))
;;;###autoload
(defun debug-info-signal-backtrace-insinuate ()
  (interactive)
  ;; (define-key special-event-map [sigint] 'sigusr-handler)
  ;; http://www.gnu.org/s/emacs/manual/html_node/elisp/Misc-Events.html
  (add-hook 'kill-emacs-hook 'emacs-collect-states-and-log)
  (define-key special-event-map [sigusr1] 'sigusr1-handler)
  (define-key special-event-map [sigusr2] 'sigusr2-handler))
;;;###autoload
(defun debug-info-signal-backtrace-uninsinuate ()
  (interactive)
  ;; (define-key special-event-map [sigint] 'sigusr-handler)
  ;; http://www.gnu.org/s/emacs/manual/html_node/elisp/Misc-Events.html
  (remove-hook 'kill-emacs-hook 'emacs-collect-states-and-log)
  (define-key special-event-map [sigusr1] 'ignore)
  (define-key special-event-map [sigusr2] 'ignore))


;;;###autoload
(defun lotus-debug-hang ()
  (interactive)
  (message "helm-alive-p %s"    helm-alive-p)
  (message "(recursion-depth) = %d" (recursion-depth))
  (setq helm-alive-p nil)
  (with-debugger-output-to-file (foobar))
  (safe-exit-recursive-edit-if-active)
  (safe-exit-recursive-edit))


;; To test the signal handler, you can make Emacs send a signal to
;; itself:
(when nil                               ;to test
  (signal-process (emacs-pid) 'sigusr1))
;; }}}



;;; backtrace-debug.el ends here
