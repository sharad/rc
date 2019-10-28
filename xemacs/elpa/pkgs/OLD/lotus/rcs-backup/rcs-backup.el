;;; rcs-backup.el --- rcs autosave backup

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





(eval-when-compile
  '(require 'vc))

(require 'vc)
(require 'vc-rcs)
;; (require 'tramp-util) ;; for `tramp-handle-executable-find'

(defun dirname-of-file (file &optional final-slash)
  ;; (ido-no-final-slash
  (if final-slash
      (expand-file-name
       (file-name-directory file))
      (directory-file-name
       (expand-file-name
        (file-name-directory file)))))


(defun dir-final-slash (dir &optional noerror)
  (if dir
      (expand-file-name (concat dir "/"))
      (unless noerror
          (error "dir is nil"))))


(setq vc-handled-backends
      ;; want RCSshould be last.
      (append (remove 'RCS vc-handled-backends) '(RCS)))

(defun failed (fmt &rest args)
  (message fmt args)
  nil)

(defvar rcs-backup-vc-file nil "")

;; (defun rcs-ci-executable-find (file)
;;   ;; (require 'tramp-util)
;;   (if (file-remote-p file)
;;       (tramp-handle-executable-find "ci")
;;     (executable-find "ci")))

(defun rcs-ci-executable-find (file)
  ;; (require 'tramp-util)
  (executable-find "ci"))


(defun vc-find-backend (file &optional backends)
  (catch 'found
    (mapc
     (lambda (b)
       (if (vc-call-backend b 'registered file)
           (throw 'found b)))
     (or backends vc-handled-backends))
    nil))



;; (when nil
;;   (vc-backend (file-truename "~/.Organize/emacs/plan/Plans/2013.06.26.muse"))
;;   (expand-file-name (file-name-directory (file-truename "~/.Organize/emacs/plan/Plans/2013.06.26.muse") ))
;;   (vc-file-getprop (file-truename "~/.Organize/emacs/plan/Plans/2013.06.26.muse") 'vc-backend)
;;   (vc-registered (file-truename "~/.Organize/emacs/plan/Plans/2013.06.26.muse"))
;;   (file-exists-p (vc-call-backend 'RCS 'registered (file-truename "~/.Organize/emacs/plan/Plans/2013.06.26.muse")))
;;   )


;;;###autoload
(defun put-file-in-rcs-for-backup (from-file)
  (let ()
    (add-hook 'vc-mode-line-hook #'vc-mode-line nil t)
    (when (if (file-exists-p
               (expand-file-name (file-name-nondirectory from-file)
                                 default-directory))
              (put-file-in-rcs from-file default-directory)
              (put-file-in-rcs from-file))

      (run-hook-with-args 'vc-mode-line-hook (file-truename from-file)))))

(defun put-file-in-rcs (nfile &optional ndirectory)
  ;; http://www.emacswiki.org/emacs/VersionControlAlways
  (interactive
   (list (buffer-file-name (current-buffer))))
  (if (file-exists-p nfile)
      (let ((org-nfile (file-truename nfile)))
        (if (file-exists-p org-nfile)
            (if (rcs-ci-executable-find org-nfile)
                (if (member 'RCS vc-handled-backends)
                    (let* ((default-directory (file-truename (or (dir-final-slash ndirectory t) (dirname-of-file org-nfile t)))) ;to fix planner muse file issue.
                           (fmode (file-modes org-nfile))
                           (file-nonrcs-backend
                            (unless rcs-backup-vc-file
                              (vc-find-backend org-nfile (remove 'RCS vc-handled-backends)))))
                      ;; (message "default-directory %s" default-directory)
                      ;; (message "put-file-in-rcs: adding to rcs")
                      ;; (message "nfile %s" nfile)
                      ;; (message "org-nfile %s" org-nfile)

                      (if (not file-nonrcs-backend)
                          (if (not (string-match ".+,v" org-nfile))
                              (let ((tempdir (getenv "TMPDIR"))
                                    (vc-rcs-checkin-switches "-l")
                                    (vc-rcs-register-switches "-l")
                                    (file-is-in-rcs (vc-call-backend 'RCS 'registered org-nfile))
                                    (rcsdir (expand-file-name "RCS" (file-name-directory org-nfile))))
                                ;; (message "Now it is sure file %s will be VCed." nfile)


                                (if tempdir
                                    (when (not (file-exists-p tempdir))
                                        ;no question.
                                      (make-directory tempdir t)))

                                ;; (add-hook 'vc-mode-line-hook #'vc-mode-line nil t)

                                (if (not (or file-is-in-rcs
                                             (file-exists-p (expand-file-name
                                                             (concat
                                                              (file-name-nondirectory org-nfile) ",v")
                                                             rcsdir))))
                                    (let ()
                                      (when (not (file-exists-p rcsdir))
                                        ;no question.
                                        (make-directory rcsdir t))
                                      (if (file-exists-p rcsdir)
                                          (progn
                                            (vc-rcs-register (list org-nfile))
                                            (vc-switch-backend nfile 'RCS))
                                          (failed "Not able to create %s for %s" rcsdir org-nfile)))

                                    (if file-is-in-rcs
                                        (progn
                                          ;; (message "going to checkin")
                                          ;; (vc-checkin file 'RCS nil "checkin" nil)
                                          (with-temp-buffer
                                            (with-vc-properties
                                                (list org-nfile)
                                              (progn
                                                ;; (vc-call-backend 'RCS 'checkin (list org-nfile) nil "autobackup")
                                                (vc-call-backend 'RCS 'checkin (list org-nfile) "autobackup")
                                                (mapc 'vc-delete-automatic-version-backups (list org-nfile))
                                                ;; (message "Checked in %s" org-nfile)
                                                )
                                              `((vc-state . up-to-date)
                                                (vc-checkout-time . ,(nth 5 (file-attributes org-nfile)))
                                                (vc-working-revision . nil)))))))
                                ;; (run-hook-with-args 'vc-mode-line-hook org-nfile)
                                (set-file-modes org-nfile fmode)
                                (message nil)
                                t)
                              (failed "file %s is a backup file." org-nfile))
                          (failed "file %s is %s file" org-nfile file-nonrcs-backend)))
                    (failed "RCS is not available."))
                (failed "rcs ci executable not available."))
            (failed "file %s do not exists." org-nfile)))
      (failed "file %s do not exists." nfile)))



;; (message "%s" vc-mode)

;; (vc-call-backend 'RCS 'mode-line-string buffer-file-name)
;; (vc-default-mode-line-string buffer-file-name)

;; (vc-mode-line buffer-file-name 'RCS)
;; (lotus-vc-mode-line buffer-file-name 'RCS)

(eval
 `(defadvice backup-buffer-copy (after
                                 backup-buffer-copy-After-in-rcs-depricated
                                 ;; ,(help-function-arglist 'backup-buffer-copy)
                                 ()
                                 disable)
    ;; (message "defadvise filename %s %s" from-name to-name)
    (condition-case e
        (put-file-in-rcs-for-backup from-name)
      ('error (message "Error: %s" e)))))

(defun backup-buffer-copy-After-in-rcs (from-name to-name modes extended-attributes)
  (condition-case e
      (put-file-in-rcs-for-backup from-name)
    ('error (message "Error: %s" e))))

(defadvice vc-rcs-find-file-hook (after vc-rcs-find-file-hook-After-in-rcs-depricated () disable)
  ;; (message "yes in backup-buffer-copy-After-in-rcs-ff")
  (set (make-local-variable 'backup-inhibited) nil))

(defun vc-rcs-find-file-hook-After-in-rcs ()
  (set (make-local-variable 'backup-inhibited) nil))


(defun rcs-backup-mode-enable ()
  (if (version<= "25" emacs-version)
      (progn
        (add-function
         :after (symbol-function 'backup-buffer-copy)
         #'backup-buffer-copy-After-in-rcs)
        (add-function
         :after (symbol-function 'vc-rcs-find-file-hook)
         #'vc-rcs-find-file-hook-After-in-rcs))
    (progn
      (ad-enable-advice 'backup-buffer-copy 'after 'backup-buffer-copy-After-in-rcs-depricated)
      (ad-enable-advice 'vc-rcs-find-file-hook 'after 'vc-rcs-find-file-hook-After-in-rcs-depricated))))

(defun rcs-backup-mode-disable ()
  (if (version<= "25" emacs-version)
      (progn
        (remove-function
         (symbol-function 'backup-buffer-copy)
         #'backup-buffer-copy-After-in-rcs)
        (remove-function
         (symbol-function 'vc-rcs-find-file-hook)
         #'vc-rcs-find-file-hook-After-in-rcs))
    (progn
      (ad-disable-advice 'backup-buffer-copy 'after 'backup-buffer-copy-After-in-rcs-depricated)
      (ad-disable-advice 'vc-rcs-find-file-hook 'after 'vc-rcs-find-file-hook-After-in-rcs-depricated))))

;;;###autoload
(define-minor-mode rcs-backup-mode
    "backup-rcs-mode"
  ;; :initial-value nil
  :init-value 1
  :lighter 'rcb
  :global t
  (if rcs-backup-mode
      (rcs-backup-mode-enable)
    (rcs-backup-mode-disable))
  (unless (version<= "25" emacs-version)
    ;; user ad-remove-advice to remove advice/advise.
    (ad-activate #'backup-buffer-copy)
    (ad-update #'backup-buffer-copy)
    (ad-activate #'vc-rcs-find-file-hook)
    (ad-update #'vc-rcs-find-file-hook)))


(provide 'rcs-backup)
;;; rcs-backup.el ends here
