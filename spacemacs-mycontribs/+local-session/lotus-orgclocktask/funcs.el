;;; funcs.el --- funcs                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

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



(defadvice kill-emacs (around kill-emacs-org-clock-out activate)
  "emacs-org-clock-out"
  (progn
    (kill-emacs-org-clock-out)
    ad-do-it))

(defadvice save-buffers-kill-emacs (around kill-emacs-org-clock-out activate)
  "emacs-org-clock-out"
  (progn
    (kill-emacs-org-clock-out)
    ad-do-it))

;;; funcs.el ends here
