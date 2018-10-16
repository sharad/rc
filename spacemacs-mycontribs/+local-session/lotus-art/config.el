;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Author: sharad <sh4r4d _at_ _G-mail_>
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



;; (provide 'config)
;;; config.el ends here



  (defun boxes-create ()
    (interactive)
    (shell-command-on-region
     (region-beginning) (region-end) "boxes -d c-cmt2" nil 1 nil))

  (defun boxes-remove ()
    (interactive)
    (shell-command-on-region
     (region-beginning) (region-end) "boxes -r -d c-cmt2" nil 1 nil))

  ;;Jason also wrote a boxes mode for Emacs. Remember to update the
  ;;design list when you add new designs to your config file.

  ;; try table-insert  !! excellent.
  ;; +-------------+-----------+--------+
  ;; |sssd         |ddddd      |sdfsd   |
  ;; +-------------+-----------+--------+
  ;; |saddsa       |sadfsfsafds|dsasfds |
  ;; +-------------+-----------+--------+
  ;; |asdsadf      |asfdsfdsf  |asdsfdsf|
  ;; +-------------+-----------+--------+
