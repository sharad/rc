;;; org-clock-utils-lotus.el --- copy config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@merunetworks.com>
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
;; (use-package startup-hooks
;;     :defer t
;;     :config
;;     (progn
;;       (progn
;;         (add-to-enable-startup-interrupting-feature-hook
;;          '(lambda ()
;;            (when nil
;;              (add-hook 'after-make-frame-functions
;;                        '(lambda (nframe)
;;                          (run-at-time-or-now 100
;;                           '(lambda ()
;;                             (if (any-frame-opened-p)
;;                                 (org-clock-in-if-not)))))
;;                        t))
;;            (add-hook 'delete-frame-functions
;;             '(lambda (nframe)
;;               (if (and
;;                    (org-clock-is-active)
;;                    (y-or-n-p-with-timeout (format "Do you want to clock out current task %s: " org-clock-heading) 7 nil))
;;                   (org-with-clock-writeable-buffer
;;                    (let (org-log-note-clock-out)
;;                      (if (org-clock-is-active)
;;                          (org-clock-out))))))))
;;          t))

;;       (progn
;;         (add-to-enable-desktop-restore-interrupting-feature-hook
;;          '(lambda ()
;;            (if (fboundp 'org-clock-persistence-insinuate)
;;                (org-clock-persistence-insinuate)
;;                (message "Error: Org Clock function org-clock-persistence-insinuate not available."))
;;            (if (fboundp 'org-clock-start-check-timer)
;;                (org-clock-start-check-timer)))
;;          t))))


;; (add-hook
;;  'kill-emacs-hook
;;  (lambda ()
;;    (if (and
;;         (org-clock-is-active)
;;         ;; (y-or-n-p-with-timeout (format "Do you want to clock out current task %s: " org-clock-heading) 7 nil)
;;         )
;;        (org-with-clock-writeable-buffer
;;         (let (org-log-note-clock-out)
;;           (if (org-clock-is-active)
;;               (org-clock-out)))))))

;;; Code:

(require 'org)
(require 'org-timer)
(require 'org-clock)
(require 'timer-utils-lotus)
(require 'startup-hooks)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'org-misc-utils-lotus)


(defun org-file-clockable-refile ())




(provide 'org-clock-utils-lotus)
;;; org-clock-utils-lotus.el ends here
