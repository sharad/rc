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


;;; Selectors

;;;###autoload
(defun occ-set-to-ctxual-tsk ()
  (occ-helm-select-ctxual-tsk
   #'occ-ctxual-tsk-marker
   #'occ-set-to))

(defun occ-goto-tsk ()
  (occ-helm-select-tsk
   #'occ-tsk-marker
   #'occ-goto))

;;;###autoload
(defun occ-create-child-tsk ()
  (interactive)
  (occ-helm-select-ctxual-tsk
   #'identity
   #'occ-capture))

(defun occ-create-child-tsk ()
  (interactive)
  (occ-helm-select-tsk
   #'identity
   #'occ-capture))

;; (push "Nothing to complete" debug-ignored-errors)

(defun occ-goto-test ()
  (interactive)
  (occ-goto-tsk))


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


;;; occ-commands.el ends here
