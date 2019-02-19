;;; occ.el --- occ               -*- lexical-binding: t; -*-
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

(require 'switch-buffer-functions)


(require 'occ-main)


(provide 'occ)

;;;###autoload
(defun occ-switch-buffer-run-curr-ctx-timer-function (prev next)
  (occ-run-curr-ctx-timer))

;;;###autoload
(defun occ-add-after-save-hook-fun-in-org-mode ()
  (add-hook 'after-save-hook 'occ-after-save-hook-fun t t))

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
  (lwarn 'occ :debug "occ-insinuate: begin")
  (message "occ-insinuate: begin")
  (progn
    (setq occ-global-tsk-collection        nil)
    ;; (add-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer t)
    ;; (add-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer t)
    ;; (add-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer t)
    (add-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
    (add-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode))
  (dolist (prop (cl-method-matched-arg 'occ-readprop nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (push propstr org-use-property-inheritance))))
  (org-clock-load) ;; newly added
 (lwarn 'occ :debug "occ-insinuate: finish")
 (message "occ-insinuate: finish"))


;;;###autoload
(defun occ-uninsinuate ()
  (interactive)
  (lwarn 'occ :debug "occ-uninsinuate: begin")
  (message "occ-uninsinuate: begin")
  (progn
    (setq occ-global-tsk-collection            nil)
    ;; (setq buffer-list-update-hook nil)

    ;; (remove-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer)
    ;; (remove-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer)
    ;; (remove-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer)
    ;; (remove-hook 'after-save-hook             'occ-after-save-hook-fun t)
    (remove-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
    (remove-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode))
  (dolist (prop (cl-method-matched-arg 'occ-readprop nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (delete propstr org-use-property-inheritance))))
 (lwarn 'occ :debug "occ-insinuate: finish")
 (message "occ-insinuate: finish"))

;;; occ.el ends here
