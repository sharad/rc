;;; etask-config.el --- Emacs Tasks

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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

(deh-require-maybe etask
  (setq etask-working-dir (auto-config-dir "etask/" t))

  (defun etask-disabled-key()
    "Tell user that key is disabled."
    (interactive)
    (message "%s" (etask-lang-msg 1004 etask-language))
    (ding)))



(provide 'etask-config)
;;; etask-config.el ends here
