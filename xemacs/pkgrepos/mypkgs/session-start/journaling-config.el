;;; journaling-config.el --- journaling blog etc

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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


;; journal's
(deh-require-maybe records
  (setq
   records-init-file (expand-file-name "~/.emacs.d/records")
   records-directory (expand-file-name "~/.Organize/emacs/records")))



(deh-require-mustbe publishing-config

  (eval-after-load "org-journal"
    '(progn
      (defvar org-journal-dir (org-publish-get-attribute "journal" "org" :base-directory))
      (setq
       org-journal-file-format "%Y-%m-%d.org"
       org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format)
       org-journal-dir (org-publish-get-attribute "journal" "org" :base-directory))
      (org-journal-update-auto-mode-alist))))



(provide 'journaling-config)
;;; journaling-config.el ends here
