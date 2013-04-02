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
      version-control t     ; use versioned backups
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 10)


(deh-require-maybe (and vc vc-rcs)
;; (when nil

  (defun put-file-in-rcs (nfile)
    ;; http://www.emacswiki.org/emacs/VersionControlAlways
    (interactive
     (list (buffer-file-name (current-buffer))))
    (message "put-file-in-rcs: adding to rcs")
    (if (not (string-match ".+,v" nfile))
        (let ((vc-rcs-checkin-switches "-l"))
          (add-hook 'vc-mode-line-hook #'vc-mode-line nil t)
          (if (not (vc-backend nfile))
              (let ((subdir (expand-file-name "RCS" (file-name-directory nfile))))
                (when (not (file-exists-p subdir))
                                        ;no question.
                  (make-directory subdir t))
                (if (file-exists-p subdir)
                    (progn
                      (vc-rcs-register (list nfile))
                      (with-temp-buffer
                        (vc-checkout nfile t))
                      ;; (vc-toggle-read-only)
                      )
                    (message "Not able to create %s for %s" subdir nfile)))
              (if (eq (vc-backend nfile) 'RCS)
                  (progn
                    (message "going to checkin")
                    ;; (vc-checkin file 'RCS nil "checkin" nil)
                    (with-temp-buffer
                      (with-vc-properties
                          (list nfile)
                        (progn
                          (vc-call-backend 'RCS 'checkin (list nfile) nil "testcomment")
                          (mapc 'vc-delete-automatic-version-backups (list nfile))
                          (message "Checked in %s" nfile))
                        `((vc-state . up-to-date)
                          (vc-checkout-time . ,(nth 5 (file-attributes file)))
                          (vc-working-revision . nil))))
                    ;; (with-temp-buffer
                    ;;     (sharad/vc-checkout nfile t))
                    ;; (vc-checkout nfile t)
                    ;; (vc-toggle-read-only)
                    (run-hook-with-args 'vc-mode-line-hook nfile)))))
        (message "file %s is a backup file." nfile))
    (message nil))

;; (message "%s" vc-mode)

;; (vc-call-backend 'RCS 'mode-line-string buffer-file-name)
;; (vc-default-mode-line-string buffer-file-name)

;; (sharad/vc-mode-line buffer-file-name 'RCS)
;; (sharad/vc-mode-line buffer-file-name 'RCS)

  (eval
   `(defadvice backup-buffer-copy (after
                                   backup-buffer-copy-in-rcs
                                   ,(help-function-arglist 'backup-buffer-copy)
                                   disable)
      (message "defadvise filename %s %s" from-name to-name)
      (put-file-in-rcs from-name)))


  (defadvice  vc-rcs-find-file-hook (after backup-buffer-copy-in-rcs-ff () disable)
    (message "yes in backup-buffer-copy-in-rcs-ff")
    (set (make-local-variable 'backup-inhibited) nil))


  (define-minor-mode rcs-backup-mode
    "backup-rcs-mode"
    ;; :initial-value nil
    :init-value nil
    :init-value nil
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



;; (remove-hook 'after-save-hook 'put-file-in-rcs)

(provide 'autosavebackup-config)

;;; autosavebackup-config.el ends here
