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


(deh-require-maybe rcs-backup
  (rcs-backup-mode t))

;; (remove-hook 'after-save-hook 'put-file-in-rcs)

(provide 'autosavebackup-config)

;;; autosavebackup-config.el ends here
