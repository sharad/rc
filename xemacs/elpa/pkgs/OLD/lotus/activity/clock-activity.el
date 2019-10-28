;;; clock-activity.el --- Emacs Clock-Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: data

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

;; This package meant to log, analyze and report all emacs clock-activity of
;; user which could further utilized to visualize clock-activity of user
;; during period of time or editing session.

;; Enable Clock-Activity for the current buffer by invoking
;; `clock-activity-mode'. If you wish to activate it globally, use
;; `global-clock-activity-mode'.

;; Set variable `clock-activity-api-key' to your API key. Point
;; `clock-activity-cli-path' to the absolute path of the CLI script
;; (clock-activity-cli.py).

;;; Code:

(provide 'clock-activity)


(require 'activity-base)

;; (defclass clocking-activity (@buffer-activity)
;;   (())
;;   "A buffer activity.")

;; (defvar @clocking-activity
;;   (@extend @buffer-activity
;;            :name "Class for Clocking Activity"
;;            :marker nil))


;; (defvar @clocking-in-activity
;;   (@extend @clocking-activity
;;            :name "Class for Clocking In Activity"
;;            :marker nil))

;; (defvar @clocking-out-activity
;;   (@extend @clocking-activity
;;            :name "Class for Clocking In Activity"
;;            :marker nil))



;;; clock-activity.el ends here
