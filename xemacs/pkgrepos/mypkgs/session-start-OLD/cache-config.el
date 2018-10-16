;;; cache-config.el ---

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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

;; Use filecache:

;; filecache remembers visited places. Add the directory into the cache:

(message "tramp-mode %s" tramp-mode)

(eval-after-load "filecache"

  '(progn
    (defvar file-cache-directories nil "file-cache-directories")

    '(add-hook
      'lotus-enable-startup-interrupting-feature-hook
      '(lambda ()
        (condition-case e
            (dolist (dir file-cache-directories)
              (file-cache-add-directory dir))
          ('error (message "problem happened in %s fun call."
                           'file-cache-add-directory)))))))

;; Whenever you want to load a file, you can enter C-x C-f C-<TAB> in
;; the minibuffer. The completion is done for the given directory.

(provide 'cache-config)
;;; cache-config.el ends here
