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
  ;; (defun put-file-in-rcs (&optional file)
  ;;   (let ((file (or file
  ;;                   (buffer-file-name (current-buffer)))))
  ;;     (if (not (vc-backend file))
  ;;         (vc-rcs-register (list file)))))

  ;; (defun backup-buffer-copy (from-name to-name modes)
  ;;   (let ((umask (default-file-modes)))))


  (defun put-file-in-rcs (file)
    (message "put-file-in-rcs: adding to rcs")
    (if (not (string-match ".+,v" file))
        (if (not (vc-backend file))
            (if (and (not (file-exists-p
                           (setq subdir (expand-file-name "RCS" (file-name-directory file)))))
                     (make-directory subdir)) ;no question.
                (vc-rcs-register (list file))
                (message "Not able to create %s for %s" subdir file))
            (message "file %s already in vcs not doing anything."))
        (message "file %s is a backup file." file)))


  (defadvice backup-buffer-copy (after backup-buffer-copy-in-rcs (from-name to-name modes) disable)
    (message "defadvise filename %s %s" from-name)
    (put-file-in-rcs from-name))



  (define-minor-mode rcs-backup-mode
    "backup-rcs-mode"
    ;; :initial-value nil
    :init-value nil
    :init-value nil
    :lighter 'rcb
    :global t
    (if rcs-backup-mode
        (ad-enable-advice 'backup-buffer-copy 'after 'backup-buffer-copy-in-rcs)
        (ad-disable-advice 'backup-buffer-copy 'after 'backup-buffer-copy-in-rcs))
    (ad-activate #'backup-buffer-copy)
    (ad-update #'backup-buffer-copy)))

;; (remove-hook 'after-save-hook 'put-file-in-rcs)





(provide 'autosavebackup-config)

;;; autosavebackup-config.el ends here
