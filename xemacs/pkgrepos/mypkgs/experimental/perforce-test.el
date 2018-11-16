;;; perforce-test.el --- Perforce test               -*- lexical-binding: t; -*-

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


(deh-require-maybe pabbrev
  (setq pabbrev-read-only-error nil)
  (defun pabbrev-activate ()
    (unless buffer-read-only
      (pabbrev-mode 1)))

  (add-element-to-lists 'pabbrev-activate pgm-langs)
  ;; Error during redisplay: (wrong-type-argument arrayp nil)
  ;; is courtesy of pabbrev
  ;; run
  ;; (remove-element-to-lists 'pabbrev-activate pgm-langs)
  ;; to correct it.
  )

(deh-require-maybe vc-p4
  (when (executable-find "p4")
    (defvar enable-p4-login nil "test")

    (if (and (setq vc-p4-require-p4config t)
             (not (getenv "P4CONFIG")))
        (setenv "P4CONFIG" ".p4conf"))

    (require 'host-info)

    (defvar run-office-activate-failed-max 4 "run-office-activate")
    (defvar run-office-activate-failed     0 "run-office-activate")
    (defvar run-office-activate            t "run-office-activate")

    (defadvice vc-p4-registered (around checkp4accessible first (file) activate)
      (when run-office-activate
        (if (< run-office-activate-failed run-office-activate-failed-max)
            (when (and file
                       (with-timeout (4 (progn (incf run-office-activate-failed) nil)) (login-to-perforce)))
              ad-do-it)
          (progn
            (message-notify
             "office-activate"
             "perforce is not reachable, so disabling office-activate and removing P4 from vc-handled-backends.")
            (setq vc-handled-backends (remove 'P4 vc-handled-backends))
            (add-hook 'lotus-enable-desktop-restore-interrupting-feature
                      '(lambda ()
                         (add-to-list 'vc-handled-backends 'P4)))
            (setq run-office-activate nil)))))

    (defun is-perforce-is-alive ()
      (if (shell-command-no-output "timeout -k 3 2 p4 depots")
          t
        (progn
          (incf run-office-activate-failed)
          nil)))

    (defun login-to-perforce ()
      (if (and
           enable-p4-login
           sharad-in-office-with-perforce
           (is-perforce-is-alive))
          ;; fix p4 timeout problem, detect it than disable it for next run.
          (if (shell-command-no-output "timeout -k 3 2 p4 user -o")
              t
            (shell-command-no-output "zenity --password | timeout -k 7 6 p4 login"))))

    (autoload 'magit-git-files "magit-git")

    (defvar office-git-remote-regex "")

    (defun office-file-p (file)
      (progn

        (when nil
          (with-timeout (4 (progn (incf run-office-activate-failed) nil))
            (login-to-perforce))
          (with-timeout (4 (progn (incf run-office-activate-failed) nil))
            (vc-p4-registered file)))

        (or
         (let ((remote-repo
                (car
                 (remove-if-not
                  #'(lambda (s) (if s (string-match-p "^origin" s)))
                  (magit-git-lines "remote" "-v")))))
           (if (and
                (functionp 'magit-git-lines)
                remote-repo)
               (string-match-p
                office-git-remote-regex
                remote-repo))))))

    (defun office-activate ()
      (interactive)
      (when run-office-activate
        (if (< run-office-activate-failed run-office-activate-failed-max)
            (let ((file (buffer-file-name)))
              (when (and file (office-file-p file))
                ;; if file is handled by perforce than assume it is
                ;; related to office perforce repository.
                (office-mode 1)))
          (progn
            (message-notify
             "office-activate"
             "perforce is not reachable, so disabling office-activate and removing P4 from vc-handled-backends.")
            (setq vc-handled-backends (remove 'P4 vc-handled-backends))
            (add-hook 'lotus-enable-desktop-restore-interrupting-feature
                      '(lambda ()
                         (add-to-list 'vc-handled-backends 'P4)))
            (setq run-office-activate nil)))))

    (if sharad-in-office-with-perforce
        (add-element-to-lists 'office-activate pgm-langs))))

(unless (fboundp 'login-to-perforce)
  (defun login-to-perforce ()
    (message "login-to-perforce: p4 not installed.")
    t))

(unless (fboundp 'is-perforce-is-alive)
  (defun is-perforce-is-alive ()
    (message "is-perforce-is-alive: p4 not installed.")
    t))

(defvar enable-p4-login nil "test")

(progn ;; "Forgive"
  (defun forgive/them ()
    (interactive)
    (if (and
         (featurep 'develock)
         (assq major-mode develock-keywords-alist))
        (develock-mode -1))
    (highlight-changes-visible-mode -1)))




(provide 'perforce-test)
;;; perforce-test.el ends here
