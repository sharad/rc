;;; startup-config.el --- Startup related functions advices

;; Copyright (C) 2013  Sharad Pratap

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


(require 'init-setup)

(add-hook 'lotus-enable-login-session-interrupting-feature-hook 'set-dbus-session)
(add-hook 'lotus-enable-startup-interrupting-feature-hook 'set-dbus-session)

(provide 'startup-config)
;;; startup-config.el ends here
