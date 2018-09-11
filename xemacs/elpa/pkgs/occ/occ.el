;;; occ.el --- occ               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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

(require 'occ-main)

;;;###autoload
(defun occ-set-global-tsk-collection-spec (spec)
  (setq
   occ-global-tsk-collection      nil
   occ-global-tsk-collection-spec spec))

(defun occ-reset-global-tsk-collection ()
  (setq
   occ-global-tsk-collection      nil))

;;;###autoload
(defun occ-insinuate ()
  (interactive)
  (progn
    (setq occ-global-tsk-collection        nil)
    (add-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer t)
    (add-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer t)
    (add-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer t)
    (add-hook 'after-save-hook             'occ-after-save-hook-fun nil t))

  (dolist (prop (cl-method-matched-arg 'occ-readprop nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (push propstr org-use-property-inheritance)))))

;;;###autoload
(defun occ-uninsinuate ()
  (interactive)
  (progn
    (setq occ-global-tsk-collection         nil)
    (remove-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer)
    ;; (setq buffer-list-update-hook nil)
    (remove-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer)
    (remove-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer)
    (remove-hook 'after-save-hook             'occ-after-save-hook-fun t))

  (dolist (prop (cl-method-matched-arg 'occ-readprop nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (delete propstr org-use-property-inheritance)))))

(provide 'occ)
;;; occ.el ends here
