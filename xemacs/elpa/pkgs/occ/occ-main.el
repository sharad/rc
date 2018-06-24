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

(defcustom *occ-last-buff-sel-time*       (current-time) "*occ-last-buff-sel-time*")
(defvar    *occ-buff-sel-timer*           nil)
(defvar    *occ-tsk-current-ctx-time-interval* 7)
(defvar    *occ-tsk-previous-ctx*              nil)
(defvar    *occ-tsk-current-ctx*               nil)
(defvar    occ-tree-tsk-root-org-file          org-context-clock-task-tree-task-root-org-file)

;;;###autoload
(defun occ-set-global-tsk-collection-spec (spec)
  (setq
   occ-global-tsk-collection      nil
   occ-global-tsk-collection-spec spec))

(occ-set-global-tsk-collection-spec
 (list :tree occ-tree-tsk-root-org-file))

;;;###autoload
(cl-defmethod occ-clockin-if-not ((ctx occ-ctx))
  (if (or
       (occ-clock-marker-is-unnamed-clock-p)
       (>= 0 (occ-associated-p (occ-current-tsk) ctx)))

      (progn                ;current clock is not matching
        (occ-debug :debug "occ-update-current-ctx: Now really going to clock.")
        (unless (occ-clockin ctx)
          ;; not able to find associated, or intentionally not selecting a clock
          (occ-debug :debug "trying to create unnamed tsk.")
          (occ-maybe-create-clockedin-unnamed-ctxual-tsk ctx))
        (occ-debug :debug "occ-update-current-ctx: Now really clock done.")
        t)

      (progn
        (occ-debug :debug "occ-update-current-ctx: Current tsk already associate to %s" ctx)
        nil)))

(cl-defmethod occ-clockin-if-chg ((ctx occ-ctx))
  (if (>
       (float-time (time-since *occ-last-buff-sel-time*))
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
              (when (occ-clockin-if-not ctx)
                (setq *occ-tsk-previous-ctx* *occ-tsk-current-ctx*)))
            (occ-debug :debug "occ-update-current-ctx: ctx %s not suitable to associate" ctx)))
    (occ-debug :debug "occ-update-current-ctx: not enough time passed.")))

(defun occ-clockin-to-curr-ctx-if-not (&optional force)
  (interactive "P")
  (occ-clockin-if-chg (occ-make-ctx)))

;;;###autoload
(defun occ-run-curr-ctx-timer ()
  (interactive)
  (progn
    (setq *occ-last-buff-sel-time* (current-time))
    (when *occ-buff-sel-timer*
      (cancel-timer *occ-buff-sel-timer*)
      (setq *occ-buff-sel-timer* nil))
    (setq *occ-buff-sel-timer*
          ;; distrubing while editing.
          ;; run-with-timer
          (run-with-idle-timer
           (1+ *occ-tsk-current-ctx-time-interval*)
           nil
           'occ-clockin-to-curr-ctx-if-not))))

;;;###autoload
(defun occ-after-save-hook ()
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name))
    (org-context-clock-build-tasks (buffer-file-name))))

;;;###autoload
(defun occ-insinuate ()
  (interactive)
  (progn
    (add-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer)
    (add-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer)
    (add-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer)
    (add-hook 'after-save-hook             'occ-after-save-hook nil t))

  (dolist (prop (cl-method-matched-arg 'occ-readprop nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (push propstr org-use-property-inheritance)))))

;;;###autoload
(defun occ-uninsinuate ()
  (interactive)
  (progn
    (remove-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer)
    ;; (setq buffer-list-update-hook nil)
    (remove-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer)
    (remove-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer)
    (remove-hook 'after-save-hook             'occ-after-save-hook t))

  (dolist (prop (cl-method-matched-arg 'occ-readprop nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (delete propstr org-use-property-inheritance)))))

(provide 'occ-main)
;;; occ-main.el ends here
