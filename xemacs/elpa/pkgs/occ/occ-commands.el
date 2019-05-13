;;; occ-commands.el --- occ commands                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <spratap@merunetworks.com>
;; Keywords:

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

(provide 'occ-commands)


(require 'occ-main)


;;;###autoload
(defun occ-helm-match-select ()
  (interactive)
  (occ-helm-select (occ-make-ctx-at-point)
                   :collector #'occ-matches
                   :action (occ-helm-actions-get :clock-in :child :child-clock-in :goto)
                   :action-transformer #'(lambda (action candidate) (occ-helm-actions-get :clock-in :child :child-clock-in :goto))
                   :timeout 7))

(defun occ-helm-list-select ()
  (interactive)
  ;; TODO: FIX it :action-transformer (lambda (actions candidate) (occ-helm-action-transformer candidate actions))
  (occ-helm-select (occ-make-ctx-at-point)
                   :collector #'occ-list
                   :action (occ-helm-actions-get :clock-in :child :child-clock-in :goto)
                   :action-transformer #'(lambda (action candidate) (occ-helm-actions-get :clock-in :child :child-clock-in :goto))
                   :timeout 7))


;;;###autoload
(defun occ-merge-unamed-task ()
  (interactive)
  (error "Implement it."))


;;;###autoload
(defun occ-proprty-edit ()
  (interactive)
  (occ-obj-prop-edit (point-marker)
                     (occ-make-ctx (get-buffer (read-buffer-to-switch "buffer: ")))
                     7))


;;;###autoload
(defun occ-run-timer ()
  (interactive)
  (occ-run-curr-ctx-timer))


;;;###autoload
(defun occ-clock-in-curr-ctx (&optional force)
  (interactive "P")
  (occ-clock-in-if-not (occ-make-ctx-at-point)))

;;;###autoload
(defun occ-clock-in-curr-ctx-if-not (&optional force)
  (interactive "P")
  ;; TODO: Add code to which check if only focus present than only trigger
  ;; else postpone it by calling run-with-idle-plus-timer
  (occ-debug :debug "begin occ-clock-in-curr-ctx-if-not")
  (lotus-with-other-frame-event-debug "occ-clock-in-curr-ctx-if-not" :cancel
    (occ-debug :debug "%s: occ-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug" (time-stamp-string))
    (if force
        (occ-clock-in-curr-ctx (occ-make-ctx-at-point))
      (occ-clock-in-if-chg (occ-make-ctx-at-point))))
  (occ-debug :nodisplay "%s: end occ-clock-in-curr-ctx-if-not" (time-stamp-string)))


;;;###autoload
(defun occ-reset-collection-object ()
  (interactive)
  (setq occ-global-tsk-collection nil)
  occ-global-tsk-collection)


(defun occ-reload (&optional uncompiled)
  (interactive "P")
  (occ-reload-lib uncompiled))

;;;###autoload
(defun occ-insinuate ()
  (interactive)
  (occ-debug :debug "occ-insinuate: begin")
  (occ-message "occ-insinuate: begin")
  (progn
    (setq occ-global-tsk-collection        nil)
    ;; (add-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer t)
    ;; (add-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer t)
    ;; (add-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer t)
    (add-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
    (add-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode))
  (dolist (prop (cl-method-sig-matched-arg '(occ-readprop (`((head ,val) occ-ctx) val)) nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (push propstr org-use-property-inheritance))))
  (org-clock-load) ;; newly added
 (occ-debug :debug "occ-insinuate: finish")
 (occ-message "occ-insinuate: finish"))


;;;###autoload
(defun occ-uninsinuate ()
  (interactive)
  (occ-debug :debug "occ-uninsinuate: begin")
  (occ-message "occ-uninsinuate: begin")
  (progn
    (setq occ-global-tsk-collection            nil)
    ;; (setq buffer-list-update-hook nil)

    ;; (remove-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer)
    ;; (remove-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer)
    ;; (remove-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer)
    ;; (remove-hook 'after-save-hook             'occ-after-save-hook-fun t)
    (remove-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
    (remove-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode))
  (dolist (prop (cl-method-sig-matched-arg '(occ-readprop (`((head ,val) occ-ctx) val)) nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (delete propstr org-use-property-inheritance))))
 (occ-debug :debug "occ-insinuate: finish")
 (occ-message "occ-insinuate: finish"))


(defun occ-version (&optional here full message)
  "Show the Occ version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive (list current-prefix-arg t (not current-prefix-arg)))
  (message (occ-get-version here full message)))


;;; occ-commands.el ends here
