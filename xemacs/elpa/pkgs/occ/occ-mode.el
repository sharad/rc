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


(require 'occ-config)


(defvar occ-mode-main-keymap (make-keymap) "occ-mode keymap.")

;; https://www.emacswiki.org/emacs/PrefixKey
;; https://stackoverflow.com/questions/25524710/define-key-in-prefix-keymap-for-a-particular-mode

(define-prefix-command 'occ-mode-keymap)

(defun occ-enable-mode-map ()
  (define-key occ-mode-main-keymap (kbd "M-n") 'occ-mode-keymap))

(defun occ-disable-mode-map ()
  (define-key occ-mode-main-keymap (kbd "M-n") nil))


(define-key occ-mode-keymap (kbd "v") 'occ-version)
(define-key occ-mode-keymap (kbd "q") 'occ-keep-quiet-for)

;;;###autoload
(define-minor-mode occ-mode
  "Toggle Occ mode.
      ...rest of documentation as before..."
  ;; The initial value.
  :init-value nil
  :global     t
  ;; The indicator for the mode line.
  :lighter " Occ"
  ;; The minor mode bindings.
  :keymap occ-mode-main-keymap
  :group 'occ
  (if occ-mode
      (occ-insinuate)
    (occ-uninsinuate)))

;;; occ-mode.el ends here
