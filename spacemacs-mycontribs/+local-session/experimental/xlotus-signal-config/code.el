;; signal-config.el -- Signal Configurations
;; Login : <s@taj>
;; Started on  Sat Jan 15 17:44:40 2011 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;

(require 'tramp-cmds)

;; {{{ Signal to Emacs



(defvar emacs-hang-load-file
  (lotus-cache-file "hang/hang.el")
  "emacs hang load file")

(defun emacs-collect-states-and-log ()
  (interactive)
  (with-current-buffer "*Messages*"
    (write-region nil t (lotus-cache-file "message/message.log")))
  (backtrace-to-buffer "*CurrentBacktrace*")
  (with-current-buffer "*CurrentBacktrace*"
    (write-region nil t (lotus-cache-file "backtrace/backtrace.log"))))

(add-hook 'kill-emacs-hook 'emacs-collect-states-and-log)

(defun emacs-clean-hangup ()
  (emacs-collect-states-and-log)
  (tramp-cleanup-all-connections)
  (let ((ispell (get-process "ispell")))
    (if ispell
        (kill-process ispell)))
  (if (file-exists-p emacs-hang-load-file)
      (load-file emacs-hang-load-file)))

(defun sigusr1-handler ()
  (interactive)
  (let ((li last-input-event))
    (message "Caught signal %S" li)
    (emacs-clean-hangup)
    (keyboard-quit)
    (message "Caught signal %S" li)))

(defun sigusr2-handler ()
  (interactive)
  (emacs-collect-states-and-log)
  (message "Caught signal %S" last-input-event))


(defmacro with-error-trace-buffer (buf &rest body)
  `(condition-case e
       ,@body
     (error
      (print (format "Error: %s" e) (get-buffer ,buf))
      ;; (backtrace-to-buffer ,buf)
      )))


(defadvice error (before dumptrace activate)
  (backtrace-to-buffer "*errbuf*")
  t)

(ad-disable-advice 'error 'before 'dumptrace)
(ad-update 'error)


;; (with-error-trace-buffer "*Messages*"
;;   (message "sdafds"))


;; http://www.gnu.org/s/emacs/manual/html_node/elisp/Misc-Events.html
(define-key special-event-map [sigusr1] 'sigusr1-handler)
(define-key special-event-map [sigusr2] 'sigusr2-handler)

(define-key special-event-map [sigint] 'sigusr-handler)



;; To test the signal handler, you can make Emacs send a signal to
;; itself:
(when nil                               ;to test
  (signal-process (emacs-pid) 'sigusr1))
;; }}}

(provide 'signal-config)

;;; signal-config.el ends here
