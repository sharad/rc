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



(defvar configuration|common|cache-config|package-list nil)


;;;###autoload
(defun configuration|common|cache-config|filecache|config ()
  ;; Use filecache:
  ;; filecache remembers visited places. Add the directory into the cache:
  ;; Whenever you want to load a file, you can enter C-x C-f C-<TAB> in
  ;; the minibuffer. The completion is done for the given directory.
  (defvar file-cache-directories nil "file-cache-directories")
  (with-eval-after-load "startup-hooks"
    (message "tramp-mode %s" tramp-mode)
    (add-hook
     'lotus-enable-startup-interrupting-feature-hook
     '(lambda ()
       (condition-case e
           (dolist (dir file-cache-directories)
             (file-cache-add-directory dir))
         ('error (message "problem happened in %s fun call."
                          'file-cache-add-directory)))))))

;;;###autoload
(defun configuration|common|cache-config|filecache|init ()
    (use-package filecache
      :defer t
      :config
      (configuration|common|cache-config|filecache|config)))
(push 'filecache configuration|common|cache-config|package-list)



;;;###autoload
(defun configuration|common|cache-config|config ()
  configuration|common|cache-config|package-list)
;;;###autoload
(defun configuration|common|cache-config|init ()
  (configuration|common|cache-config|config))

;;;###autoload
(defun configuration|common|cache-config|packages ()
  configuration|common|cache-config|package-list)


(provide 'cache-config)
;;; cache-config.el ends here
