;;; debug-hang.el --- debug hang                     -*- lexical-binding: t; -*-

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

(provide 'debug-hang)


;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Event-Input-Misc.html#Event-Input-Misc

(defvar fast-helm-test-timer nil)

(defmacro fast-helm-timed (timeout win-buff &rest body)
  (let ((temp-win-config (make-symbol "test-helm-timed")))
    `(let* ((,temp-win-config (current-window-configuration))
            (current-command (or
                              (helm-this-command)
                              this-command))
            (str-command     (helm-symbol-name current-command))
            (buf-name        (or ,win-buff (format "*helm-mode-%s*" str-command)))
            (timer (run-with-idle-plus-timer ,timeout nil
                                        #'(lambda (buffname)
                                            (let* ((buff (or
                                                          (get-buffer buffname)
                                                          (get-buffer "*helm*")))
                                                   (w (if buff (get-buffer-window buff))))
                                              (message "helm-timed: triggered timer for new-win %s" w)
                                              ;; TODO: open emacs why SIGABRT triggered on pressin C-g three time when struck.
                                              ;;       with below line.
                                              (discard-input)
                                              (when (and (windowp w)
                                                         (window-valid-p w))
                                                (safe-delete-window w)
                                                (safe-exit-recursive-edit-if-active)
                                                (select-frame-set-input-enable-raise)
                                                (when ,temp-win-config
                                                  (set-window-configuration ,temp-win-config)
                                                  (setq ,temp-win-config nil)))))
                                        buf-name)))
       (unwind-protect
            (progn
              (select-frame-set-input-disable-raise)
              (progn
                ,@body))
         (select-frame-set-input-enable-raise)
         (cancel-timer timer)))))
(put 'fast-helm-timed 'lisp-indent-function 2)

(defun fast-helm-test ()
  (fast-helm-timed 2 nil
    (let* ((prompt (format "fast edit - (recursion-depth): %d" (recursion-depth)))
           (source (helm-build-sync-source prompt :candidates '(a b c))))
      (helm
       :sources (list source))
      (message "(recursion-depth): %d" (recursion-depth)))))

(defun fast-helm-test-stop-timer ()
  (interactive)
  (when fast-helm-test-timer
    (cancel-timer fast-helm-test-timer)
    (setq fast-helm-test-timer nil)))

(defun fast-helm-test-start-timer ()
  (interactive)
  (fast-helm-test-stop-timer)
  (setq fast-helm-test-timer (run-with-timer 2 2 'fast-helm-test)))


(when nil

  (fast-helm-test-stop-timer)


  (fast-helm-test-start-timer)


  ())

;;; debug-hang.el ends here
