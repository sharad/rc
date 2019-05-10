;;; occ-mode.el --- occ mode                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
;; Keywords: convenience, tools

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

(provide 'occ-mode)


(define-minor-mode occ-mode
  "Toggle Occ mode.
      ...rest of documentation as before..."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Occ"
  ;; The minor mode bindings.
  :keymap
  '(([C-backspace] . occ-electric-delete)
    ([C-M-backspace]
     . (lambda ()
         (interactive)
         (occ-electric-delete t))))
  :group 'hunger
  (if occ-mode
      (occ-insinuate)
    (occ-uninsinuate)))


;;; occ-mode.el ends here
