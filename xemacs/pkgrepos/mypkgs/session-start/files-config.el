;;; files-config.el --- files

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@merunetworks.com>
;; Keywords:

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


(deh-require-maybe find-file-in-project
  ;; If non-nil, this function is called to determine the project root.
  (setq
   ffip-project-root-function nil
   ;; define suitable functon for it.
   ffip-project-root "~/"
   ffip-patterns (append '("*.cpp" "*.h") ffip-patterns)
   )

  (defun ffip-set-project-root ()
    (interactive)
    (setq
     ffip-project-root (ido-read-directory-name "FFip Root Dir: " ffip-project-root))))

(deh-require-maybe lusty-explorer
  )

(deh-require-maybe ff-paths
  (ff-paths-install))

(deh-section "File no writable problem"
  (make-variable-buffer-local
   (defvar my-override-mode-on-save nil
     "Can be set to automatically ignore read-only mode of a file when saving."))

  (defadvice file-writable-p (around my-overide-file-writeable-p act)
    "override file-writable-p if `my-override-mode-on-save' is set."
    (setq ad-return-value (or
                           my-override-mode-on-save
                           ad-do-it)))

  (defun sharad/override-toggle-read-only ()
    "Toggle buffer's read-only status, keeping `my-override-mode-on-save' in sync."
    (interactive)
    (setq my-override-mode-on-save (not my-override-mode-on-save))
    (toggle-read-only)))

(provide 'files-config)
;;; files-config.el ends here
