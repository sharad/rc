;;; session-config.el --- session setting

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d@gmail.com>
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

(require 'startup-hooks)
(require 'sessions-mgr)
(require 'basic-utils)


(add-hook
 'sharad/enable-startup-interrupting-feature-hook
 'frame-session-restore-hook-func
 t)

(add-hook ;; 'after-init-hook
 'sharad/enable-startup-interrupting-feature-hook
 '(lambda ()
   (run-at-time-or-now 7 'sharad/desktop-session-restore)))


(provide 'session-config)
;;; session-config.el ends here
