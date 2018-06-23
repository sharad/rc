;;; occ-main.el --- occ-api               -*- lexical-binding: t; -*-
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

(require 'occ-object-methods)
(require 'occ-unnamed)
(require 'occ-interactive)

(defcustom *occ-last-buffer-select-time*        (current-time) "*occ-last-buffer-select-time*")
(defvar    *occ-tsk-current-ctx-time-interval* 7)
(defvar    *occ-tsk-previous-ctx*              nil)
(defvar    *occ-tsk-current-ctx*               nil)
(defvar    occ-tree-tsk-root-org-file org-context-clock-tsk-tree-tsk-root-org-file)

(defun occ-set-global-tsk-collection-spec (spec)
  (setq
   occ-global-tsk-collection      nil
   occ-global-tsk-collection-spec spec))

(occ-set-global-tsk-collection-spec
 (list :tree occ-tree-tsk-root-org-file))

(cl-defmethod occ-clockin-assoctsk-if-not ((ctx occ-ctx))
  (if (or
       (occ-clock-marker-is-unnamed-clock-p)
       (>= 0 (occ-associated-p (occ-current-tsk) ctx)))

      (progn                ;current clock is not matching
        (occ-debug :debug "occ-update-current-ctx: Now really going to clock.")
        (unless (occ-clockin-assoctsk ctx)
          ;; not able to find associated, or intentionally not selecting a clock
          (occ-debug :debug "trying to create unnamed tsk.")
          (occ-maybe-create-clockedin-unnamed-ctxual-tsk ctx))
        (occ-debug :debug "occ-update-current-ctx: Now really clock done.")
        t)

      (progn
        (occ-debug :debug "occ-update-current-ctx: Current tsk already associate to %s" ctx)
        nil)))

(cl-defmethod occ-clockin-assoctsk-if-chg ((ctx occ-ctx))
  (if (>
       (float-time (time-since *occ-last-buffer-select-time*))
       *occ-tsk-current-ctx-time-interval*)
      (let* ((buff    (occ-ctx-buffer ctx)))
        (setq *occ-tsk-current-ctx* ctx)
        (if (and
             (occ-chgable-p)
             buff (buffer-live-p buff)
             (not (minibufferp buff))
             (not              ;BUG: Reconsider whether it is catching case after some delay.
              (equal *occ-tsk-previous-ctx* *occ-tsk-current-ctx*)))
            (progn
              (when (occ-clockin-assoctsk-if-not ctx)
                (setq *occ-tsk-previous-ctx* *occ-tsk-current-ctx*)))
            (occ-debug :debug "occ-update-current-ctx: ctx %s not suitable to associate" ctx)))
    (occ-debug :debug "occ-update-current-ctx: not enough time passed.")))

(defun occ-clockin-assoctsk-to-curr-ctx-if-not (&optional force)
  (interactive "P")
  (occ-clockin-assoctsk-if-chg (occ-make-context)))


(defun occ-run-task-current-context-timer ()
  (interactive)
  (progn
    (setq *occ-last-buffer-select-time* (current-time))
    (when *occ-buffer-select-timer*
      (cancel-timer *occ-buffer-select-timer*)
      (setq *occ-buffer-select-timer* nil))
    (setq *occ-buffer-select-timer*
          ;; distrubing while editing.
          ;; run-with-timer
          (run-with-idle-timer
           (1+ *occ-task-current-context-time-interval*)
           nil
           'occ-update-current-context))))

(defun occ-insinuate ()
  (interactive)
  (progn
    (add-hook 'buffer-list-update-hook     'occ-run-task-current-context-timer)
    (add-hook 'elscreen-screen-update-hook 'occ-run-task-current-context-timer)
    (add-hook 'elscreen-goto-hook          'occ-run-task-current-context-timer)
    (add-hook 'after-save-hook             'occ-after-save-hook nil t))

  (dolist (prop (occ-keys-with-operation :getter nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (push propstr org-use-property-inheritance)))))

;;;###autoload
(defun occ-uninsinuate ()
  (interactive)
  (progn
    (remove-hook 'buffer-list-update-hook 'occ-run-task-current-context-timer)
    ;; (setq buffer-list-update-hook nil)
    (remove-hook 'elscreen-screen-update-hook 'occ-run-task-current-context-timer)
    (remove-hook 'elscreen-goto-hook 'occ-run-task-current-context-timer)
    (remove-hook 'after-save-hook             'occ-after-save-hook t))

  (dolist (prop (occ-keys-with-operation :getter nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (delete propstr org-use-property-inheritance)))))

(provide 'occ-main)
;;; occ-main.el ends here
