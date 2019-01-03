;;; elscreen-modification.el --- elscreen modifications  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <spratap@merunetworks.com>
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


;;; Mode Line & Menu & Tab

;; GNU Emacs
(defvar elscreen-mode-line-string "[0]")
(defun elscreen-mode-line-update ()
  (when (elscreen-screen-modified-p 'elscreen-mode-line-update)
    (let ((screen-num (elscreen-get-current-screen)))
      (when (numberp screen-num)
        (setq elscreen-mode-line-string
              (format "[%d]" (elscreen-get-current-screen)))
        (force-mode-line-update)))))

(when (consp mode-line-format)
  (let ((point (memq 'mode-line-position mode-line-format))
        (elscreen-mode-line-elm '(elscreen-display-screen-number
                                  (" " elscreen-mode-line-string))))
    (when (and (null (member elscreen-mode-line-elm mode-line-format)) point)
      (setcdr point (cons elscreen-mode-line-elm (cdr point))))))

(add-hook 'elscreen-screen-update-hook 'elscreen-mode-line-update)




;;; elscreen-modification.el ends here
