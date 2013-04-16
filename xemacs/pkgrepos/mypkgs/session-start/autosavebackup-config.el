;;; autosavebackup-config.el --- session setting

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad>
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


(deh-require-maybe real-auto-save
  ;;link: http://www.litchie.net/programs/real-auto-save.html
  (add-hook 'text-mode-hook 'turn-on-real-auto-save)
  (add-hook 'muse-mode-hook 'turn-on-real-auto-save)
  (setq real-auto-save-interval 5))

;; Explanation: when emacs does a backup, by default it renames the
;; original file into the backup file name, then create a new file and
;; insert the current data into it. This effectively destroys the
;; creation date of your file.

(setq backup-by-copying t   ; don't clobber symlinks
      version-control nil     ; use versioned backups
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2)

(eval-when-compile
  '(require 'vc))

(require 'vc)

(deh-require-maybe (and vc vc-rcs)
;; (when nil

  (defvar rcs-backup-vc-file nil "")

  (defun vc-find-backend (file &optional backends)
    (catch 'found
      (mapc
       (lambda (b)
         (if (vc-call-backend b 'registered file)
              (throw 'found b)))
       (or backends vc-handled-backends))))

  (defun put-file-in-rcs (from-file)
    ;; http://www.emacswiki.org/emacs/VersionControlAlways
    (interactive
     (list (buffer-file-name (current-buffer))))
    (if (file-exists-p from-file)
        (let ((org-from-file (file-truename from-file)))
          (if (file-exists-p org-from-file)
              (if (member 'RCS vc-handled-backends)
                  (let* ((default-directory (file-truename default-directory)) ;to fix planner muse file issue.
                         (fmode (file-modes org-from-file))
                         (file-nonrcs-backend
                          (unless rcs-backup-vc-file
                            (vc-find-backend org-from-file (remove 'RCS vc-handled-backends)))))
                    (message "put-file-in-rcs: adding to rcs")
                    (message "from-file %s" from-file)
                    (message "org-from-file %s" org-from-file)

                    (if (not file-nonrcs-backend)
                        (if (not (string-match ".+,v" org-from-file))
                            (let ((tempdir (getenv "TMPDIR"))
                                  (vc-rcs-checkin-switches "-l")
                                  (vc-rcs-register-switches "-l")
                                  (file-is-in-rcs (vc-call-backend 'RCS 'registered org-from-file))
                                  (rcsdir (expand-file-name "RCS" (file-name-directory org-from-file))))

                              (if tempdir
                                  (when (not (file-exists-p tempdir))
                                        ;no question.
                                    (make-directory tempdir t)))

                              (message "Now it is sure file %s will be VCed." from-file)
                              (add-hook 'vc-mode-line-hook #'vc-mode-line nil t)

                              (if (not (or file-is-in-rcs
                                           (file-exists-p (expand-file-name
                                                           (concat
                                                            (file-name-nondirectory org-from-file) ",v")
                                                           rcsdir))))
                                  (let ()
                                    (when (not (file-exists-p rcsdir))
                                        ;no question.
                                      (make-directory rcsdir t))
                                    (if (file-exists-p rcsdir)
                                        (progn
                                          (vc-rcs-register (list org-from-file))
                                          (vc-switch-backend from-file 'RCS))
                                        (message "Not able to create %s for %s" rcsdir org-from-file)))

                                  (if file-is-in-rcs
                                      (progn
                                        (message "going to checkin")
                                        ;; (vc-checkin file 'RCS nil "checkin" nil)
                                        (with-temp-buffer
                                          (with-vc-properties
                                              (list org-from-file)
                                            (progn
                                              (vc-call-backend 'RCS 'checkin (list org-from-file) nil "autobackup")
                                              (mapc 'vc-delete-automatic-version-backups (list org-from-file))
                                              (message "Checked in %s" org-from-file))
                                            `((vc-state . up-to-date)
                                              (vc-checkout-time . ,(nth 5 (file-attributes org-from-file)))
                                              (vc-working-revision . nil))))
                                        ;; (with-temp-buffer
                                        ;;     (sharad/vc-checkout org-from-file t))
                                        ;; (vc-checkout org-from-file t)
                                        ;; (vc-toggle-read-only)
                                        ;; (run-hook-with-args 'vc-mode-line-hook org-from-file)
                                        )))


                              (run-hook-with-args 'vc-mode-line-hook org-from-file)
                              (set-file-modes org-from-file fmode)
                              (message nil))
                            (message "file %s is a backup file." org-from-file))
                        (message "file %s is %s file" org-from-file file-nonrcs-backend)))
                  (message "RCS is not available."))
              (message "file %s do not exists." org-from-file)))
        (message "file %s do not exists." from-file)))

;; (message "%s" vc-mode)

;; (vc-call-backend 'RCS 'mode-line-string buffer-file-name)
;; (vc-default-mode-line-string buffer-file-name)

;; (vc-mode-line buffer-file-name 'RCS)
;; (sharad/vc-mode-line buffer-file-name 'RCS)

  (eval
   `(defadvice backup-buffer-copy (after
                                   backup-buffer-copy-in-rcs
                                   ,(help-function-arglist 'backup-buffer-copy)
                                   disable)
      ;; (message "defadvise filename %s %s" from-name to-name)
      (condition-case e
          (put-file-in-rcs from-name)
          ('error nil))))


  (defadvice vc-rcs-find-file-hook (after backup-buffer-copy-in-rcs-ff () disable)
    ;; (message "yes in backup-buffer-copy-in-rcs-ff")
    (set (make-local-variable 'backup-inhibited) nil))


  (define-minor-mode rcs-backup-mode
    "backup-rcs-mode"
    ;; :initial-value nil
    :init-value 1
    :lighter 'rcb
    :global t
    (if rcs-backup-mode
        (progn
          (ad-enable-advice 'backup-buffer-copy 'after 'backup-buffer-copy-in-rcs)
          (ad-enable-advice 'vc-rcs-find-file-hook 'after 'backup-buffer-copy-in-rcs-ff))
        (progn
          (ad-disable-advice 'backup-buffer-copy 'after 'backup-buffer-copy-in-rcs)
          (ad-disable-advice 'vc-rcs-find-file-hook 'after 'backup-buffer-copy-in-rcs-ff)))
    (ad-activate #'backup-buffer-copy)
    (ad-update #'backup-buffer-copy)
    (ad-activate #'vc-rcs-find-file-hook)
    (ad-update #'vc-rcs-find-file-hook)))

(rcs-backup-mode t)


;; (remove-hook 'after-save-hook 'put-file-in-rcs)

(provide 'autosavebackup-config)

;;; autosavebackup-config.el ends here
