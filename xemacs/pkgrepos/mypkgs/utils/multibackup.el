;;; multibackup.el --- multiple backup of file

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


(require 'rcs-backup)


(defvar multibackup-locs nil "List of locations to backup file.")

(defun multibackup-file (file locations)
  )



(deh-section "Buffer with Multiple files"

  (defvar buffer-linked-files nil "list of buffer-linked-files")
  (make-local-variable 'buffer-linked-files)

  (defun multibackup-copy-all-file ()
    (if buffer-linked-files
        (dolist (f buffer-linked-files)
          ;; (write-region nil nil f nil)
          (unless (file-exists-p (file-name-directory f))
            (make-directory (file-name-directory f) t))

          (let (ofilemode)
            (when (and (file-exists-p f)
                       (not (file-writable-p f)))
              (setq ofilemode (file-modes f))
              (set-file-modes f 666))

            (copy-file (buffer-file-name (current-buffer)) f t)

            (if ofilemode
                (set-file-modes f ofilemode)))

          (put-file-in-rcs f)
          (message "copied %s to %s" (file-name-nondirectory buffer-file-name) f))
        (message "No target for multibackup, could be added using M-x multibackup-add-linked-file.")))


  (defun multibackup-add-linked-file (tfile)
    (interactive "Flink file: ")
    (if buffer-file-name
        (let* ((srcfilename (file-name-nondirectory buffer-file-name))
               (file (file-truename tfile))
               (file   (if (file-exists-p file)
                           (if (file-directory-p file)
                               (concat (dir-final-slash file) srcfilename)
                               file)
                           (if (= (aref file (1- (length file))) ?\/)
                               (concat (dir-final-slash file) srcfilename)
                               file))))
          (if (string-equal (file-truename buffer-file-name) (file-truename file))
              (error "backup can not be same."))
          (make-local-variable 'buffer-linked-files)
          (pushnew file buffer-linked-files))
        (message "buffer is not associated with any file.")))

  (require 'desktop)
  (add-to-list 'desktop-locals-to-save 'buffer-linked-files)
  (require 'session)
  (add-to-list 'session-locals-include 'buffer-linked-files)



  (define-minor-mode multibackup-mode
      "multibackup-mode"
    ;; :initial-value nil
    :init-value 1
    ;; :lighter 'rcb
    :global nil
    (if multibackup-mode
        (add-hook 'after-save-hook 'multibackup-copy-all-file nil t)
        (remove-hook 'after-save-hook 'multibackup-copy-all-file t)))

  ;; (add-hook 'after-save-hook 'copy-all-file)
  )

(provide 'multibackup)
;;; session-config.el ends here
