;;; vc-config.el --- Vsrsion Control

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad>
;; Keywords: files

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


(require 'general-testing)

(setq
 vc-follow-symlinks
 ;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
 t)

(testing
 ;; TODO: check
 ;; http://www.emacswiki.org/emacs/log-edit-fill
 ;; http://www.emacswiki.org/emacs/VcAddLogEntries

)


(provide 'vc-config)
;;; vc-config.el ends here
