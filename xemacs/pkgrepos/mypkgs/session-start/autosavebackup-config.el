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

  ;; (setq vc-rcs-checkin-switches
  ;;       ;; http://www.emacswiki.org/emacs/RevisionControlSystem#toc1
  ;;       "-l")

  (defun put-file-in-rcs (file)
    ;; http://www.emacswiki.org/emacs/VersionControlAlways
    (message "put-file-in-rcs: adding to rcs")
    (if (not (string-match ".+,v" file))
        (if (not (vc-backend file))
            (let ((subdir (expand-file-name "RCS" (file-name-directory file))))
              (when (not (file-exists-p subdir))
                ;no question.
                    (make-directory subdir t))
              (if (file-exists-p subdir)
                  (progn
                    (vc-rcs-register (list file))
                    (vc-checkout file t)
                    ;; (vc-toggle-read-only)
                    )
                  (message "Not able to create %s for %s" subdir file)))
            (if (eq (vc-backend file) 'RCS)
                (progn
                  (message "going to checkin")
                  (vc-checkin file 'RCS nil "checkin" nil)
                  (vc-checkout file t)
                  ;; (vc-toggle-read-only)
                  (message "Checked in"))
                (message "file %s already in %s vcs not doing anything."
                         file (vc-backend file))))
        (message "file %s is a backup file." file)))


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



;; (vc-rcs-find-file-hook)


;; (defun sharad/vc-find-file-hook ()
;;   "Function for `find-file-hook' activating VC mode if appropriate."
;;   ;; Recompute whether file is version controlled,
;;   ;; if user has killed the buffer and revisited.
;;   (when buffer-file-name
;;     (let (backend)
;;       (cond
;;         ((setq backend (with-demoted-errors (vc-backend buffer-file-name)))
;;          (unless vc-make-backup-files
;;            ;; Use this variable, not make-backup-files,
;;            ;; because this is for things that depend on the file name.
;;            (set (make-local-variable 'backup-inhibited) t))
;;          ;; Let the backend setup any buffer-local things he needs.
;;          (vc-call-backend backend 'find-file-hook))
;;        ((let ((link-type (and (not (equal buffer-file-name buffer-file-truename))
;; 			      (vc-backend buffer-file-truename))))
;; 	  (cond ((not link-type) nil)	;Nothing to do.
;; 		((eq vc-follow-symlinks nil)
;; 		 (message
;; 		  "Warning: symbolic link to %s-controlled source file" link-type))
;; 		((or (not (eq vc-follow-symlinks 'ask))
;; 		     ;; If we already visited this file by following
;; 		     ;; the link, don't ask again if we try to visit
;; 		     ;; it again.  GUD does that, and repeated questions
;; 		     ;; are painful.
;; 		     (get-file-buffer
;; 		      (abbreviate-file-name
;; 		       (file-chase-links buffer-file-name))))

;; 		 (vc-follow-link)
;; 		 (message "Followed link to %s" buffer-file-name)
;; 		 (vc-find-file-hook))
;; 		(t
;; 		 (if (yes-or-no-p (format
;; 				   "Symbolic link to %s-controlled source file; follow link? " link-type))
;; 		     (progn (vc-follow-link)
;; 			    (message "Followed link to %s" buffer-file-name)
;; 			    (vc-find-file-hook))
;; 		   (message
;; 		    "Warning: editing through the link bypasses version control")
;; 		   )))))))))

;; (add-hook 'find-file-hook 'vc-find-file-hook)


(provide 'autosavebackup-config)

;;; autosavebackup-config.el ends here
