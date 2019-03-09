;;; occ-deprecated.el --- occ deprecated             -*- lexical-binding: t; -*-

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

(progn                                  ;method

  ;; (cl-defmethod occ-readprop ((tsk-pair (head root))
  ;;                             (ctx occ-ctx))
  ;;   (let* ((file (if ctx (occ-ctx-file ctx)))
  ;;          (dir (if (stringp file) (file-name-directory file) (dirname-of-file file)))
  ;;          (prompt (concat (symbol-name (car tsk-pair)) ": ")))
  ;;     (ido-read-directory-name prompt dir dir)))
  ;; (cl-defmethod occ-readprop ((tsk-pair (head subtree))
  ;;                             (ctx occ-ctx))
  ;;   (let ((prompt (concat (symbol-name (car tsk-pair)) ": ")))
  ;;     (file-relative-name
  ;;      (ido-read-file-name ;; org-iread-file-name
  ;;       prompt
  ;;       default-directory default-directory))))
  ;; (cl-defmethod occ-writeprop ((tsk-pair (head subtree))))

  ;; deprecated
  ;; (cl-defmethod occ-sacha-selection-line ((ctxask occ-ctxual-tsk))
  ;;   "Insert a line for the clock selection menu.
  ;; And return a cons cell with the selection character integer and the marker
  ;; pointing to it."
  ;;   (let ((marker (occ-ctxual-tsk-marker ctxask))
  ;;         (rank   (occ-ctxual-tsk-rank   ctxask)))
  ;;     (when (marker-buffer marker)
  ;;       (with-current-buffer (org-base-buffer (marker-buffer marker))
  ;;         (org-with-wide-buffer
  ;;          (progn ;; ignore-errors
  ;;            (goto-char marker)
  ;;            (let* ((cat (org-get-category))
  ;;                   (heading (org-get-heading 'notags))
  ;;                   (prefix (save-excursion
  ;;                             (org-back-to-heading t)
  ;;                             (looking-at org-outline-regexp)
  ;;                             (match-string 0)))
  ;;                   (tsk (substring
  ;;                         (org-fontify-like-in-org-mode
  ;;                          (concat prefix heading)
  ;;                          org-odd-levels-only)
  ;;                         (length prefix))))
  ;;              (when tsk ;; (and cat tsk)
  ;;                ;; (insert (format "[%c] %-12s  %s\n" i cat tsk))
  ;;                ;; marker
  ;;                (cons (occ-print ctxask) ctxask))))))))) ;TODO

  (cl-defmethod occ-clock-in ((ctx occ-ctx))
    "marker and ranked version"
    (interactive
     (list (occ-make-ctx)))
    (progn
      (message "in occ-clock-in occ-ctx 1")
      (let* ((ctx (or ctx (occ-make-ctx)))
             (matched-ctxual-tsks
              (run-unobtrusively           ;heavy task

                ;; BUG Urgent TODO: SOLVE ASAP ???? at (occ-clock-in-if-not ctx) and (occ-clock-in ctx)

                ;; begin occ-clock-in-curr-ctx-if-not
                ;; 2019-03-06 22:55:31 s: occ-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug
                ;; occ-clock-in-if-not: Now really going to clock.
                ;; in occ-clock-in occ-ctx 1
                ;; user input 111 retval t
                ;; trying to create unnamed tsk.
                ;; occ-maybe-create-unnamed-tsk: Already clockin unnamed tsk
                ;; occ-clock-in-if-not: Now really clock done.

                (remove-if-not
                 #'(lambda (ctxual-tsk)
                     (let* ((marker (occ-ctxual-tsk-marker ctxual-tsk)))
                       (and
                        marker
                        (marker-buffer marker))))
                 (occ-matching-ctxual-tsks (occ-collection-object) ctx)))))
        (unless (eq matched-ctxual-tsks t)
          (if matched-ctxual-tsks
              (let* ((sel-ctxual-tsk
                      (if (> (length matched-ctxual-tsks) 1)
                          (occ-sacha-helm-select-timed matched-ctxual-tsks)
                        (car matched-ctxual-tsks))))
                ;; (sel-tsk   (if sel-ctxual-tsk (plist-get sel-ctxual-tsk :tsk)))
                ;; (sel-marker (if sel-tsk      (plist-get sel-tsk      :tsk-clock-marker)))

                ;; (occ-debug 6 "sel-ctxual-tsk %s sel-tsk %s sel-marker %s" sel-ctxual-tsk sel-tsk sel-marker)
                (if sel-ctxual-tsk (occ-clock-in sel-ctxual-tsk)))
            (progn
              ;; here create unnamed tsk, no need
              (setq *occ-update-current-ctx-msg* "null clock")
              (occ-debug nil
                         "No clock found please set a match for this ctx %s, add it using M-x occ-add-to-org-heading."
                         ctx)
              (occ-add-to-org-heading-when-idle ctx 7)
              nil)))))))


(provide 'occ-deprecated)
;;; occ-deprecated.el ends here
