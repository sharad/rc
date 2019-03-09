;;; occ-obj-simple.el --- occ simple methods         -*- lexical-binding: t; -*-

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

(provide 'occ-obj-simple)


(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-prop)


(cl-defmethod occ-fontify-like-in-org-mode ((tsk occ-tsk))
  (let* ((level   (or (occ-get-property tsk 'level) 0))
         (heading (occ-get-property tsk 'heading-prop))
         (prefix  (concat (make-string level ?\*) " ")))
    (if nil ;; if test without else with prefix
        (substring
         (org-fontify-like-in-org-mode
          (concat prefix heading)
          org-odd-levels-only)
         (1+ level))

      (org-fontify-like-in-org-mode
       (concat prefix heading)
       org-odd-levels-only))))

(cl-defmethod occ-ctxual-tsk-marker ((ctxask occ-ctxual-tsk))
  (let* ((tsk    (occ-ctxual-tsk-tsk ctxask))
         (marker (occ-tsk-marker tsk)))
    marker))

(cl-defmethod occ-print ((tsk occ-tsk))
  ;; (format "[%4d] %s"
  ;;         0
  ;;         (occ-fontify-like-in-org-mode tsk))
  (format "%s"
          (occ-fontify-like-in-org-mode tsk)))

(cl-defmethod occ-print ((ctxask occ-ctxual-tsk))
  (let ((tsk (occ-ctxual-tsk-tsk ctxask)))
    (format "[%4d] %s"
            (occ-ctxual-tsk-rank ctxask)
            (occ-fontify-like-in-org-mode tsk))))


;; could be handled with
;;
;; (cl-defmethod occ-rank ((tsk-pair (head current-clock))
;;                            (ctx occ-ctx))
(defun occ-current-tsk ()
  (when (and
         org-clock-marker
         (markerp org-clock-marker)
         (> (marker-position-nonil org-clock-marker) 0))
    (org-with-cloned-marker org-clock-marker "<tree>"
      (let ((view-read-only nil)
            (buffer-read-only t))
        (read-only-mode)
        (org-previous-visible-heading 1)
        (let ((tsk (occ-make-tsk
                    (or org-clock-hd-marker org-clock-marker)
                    (occ-tsk-builder))))
          tsk)))))
;; Create tsk info out of current clock:1 ends here

;; Test if TSK is associate to CTX

(cl-defmethod occ-associated-p ((tsk symbol)
                                (ctx occ-ctx))
  0)

(cl-defmethod occ-associated-p ((tsk occ-tsk)
                                (ctx occ-ctx))
  (if tsk
      (occ-rank tsk ctx)
    0))
;; Test if TSK is associate to CTX:1 ends here

;; Collect and return tsk matching to CTX
;;;###autoload
(cl-defmethod occ-current-associated-p ((ctx occ-ctx))
  (let ((tsk (occ-tsk-current-tsk)))
    (when tsk (occ-rank tsk ctx))))


(cl-defgeneric occ-sacha-selection-line (obj))


(cl-defmethod occ-sacha-selection-line ((mrk marker))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the mrk
pointing to it."
  (when (mrk-buffer mrk)
    (with-current-buffer (org-base-buffer (mrk-buffer mrk))
      (org-with-wide-buffer
       (progn ;; ignore-errors
         (goto-char mrk)
         (let* ((cat (org-get-category))
                (heading (org-get-heading 'notags))
                (prefix (save-excursion
                          (org-back-to-heading t)
                          (looking-at org-outline-regexp)
                          (match-string 0)))
                (tsk (substring
                      (org-fontify-like-in-org-mode
                       (concat prefix heading)
                       org-odd-levels-only)
                      (length prefix))))
           (when tsk ;; (and cat tsk)
             ;; (insert (format "[%c] %-12s  %s\n" i cat tsk))
             ;; mrk
             (cons tsk mrk))))))))

(cl-defmethod occ-sacha-selection-line ((tsk occ-tsk))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-print tsk) tsk))

(cl-defmethod occ-sacha-selection-line ((ctxask occ-ctxual-tsk))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-print ctxask) ctxask))
;; function to setup ctx clock timer:2 ends here


(cl-defgeneric occ-goto (obj))


(cl-defmethod occ-goto ((mrk marker))
  (progn
    (switch-to-buffer (marker-buffer mrk))
    ;; TODO find about "org-overview"
    ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
    ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
    ;; https://github.com/facetframer/orgnav
    ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
    ;; (outline-show-all)
    (org-content 10)
    (goto-char mrk)))

(cl-defmethod occ-goto ((tsk occ-tsk))
  (let ((mrk (occ-tsk-marker tsk)))
    (if (and
         (markerp mrk)
         (marker-buffer mrk))
        (occ-goto mrk)
      (error "marker %s invalid." mrk))))

(cl-defmethod occ-goto (ctxask occ-ctxual-tsk)
  (occ-goto (occ-ctxual-tsk-marker)))

(cl-defmethod occ-set-to ((mrk marker))
  (progn
    (set-buffer (marker-buffer mrk))
    ;; TODO find about "org-overview"
    ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
    ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
    ;; https://github.com/facetframer/orgnav
    ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
    ;; (outline-show-all)
    ;; (org-content 10)
    (goto-char mrk)))

(cl-defmethod occ-set-to ((tsk occ-tsk))
  (let ((mrk (occ-tsk-marker tsk)))
    (if (and
         (markerp mrk)
         (marker-buffer mrk))
        (occ-set-to mrk)
      (error "marker %s invalid." mrk))))

(cl-defmethod occ-set-to (ctxask occ-ctxual-tsk)
  (occ-set-to (occ-ctxual-tsk-marker)))


(defvar occ-capture+-helm-templates-alist org-capture+-helm-templates-alist)

(defun occ-capture+-helm-select-template ()
  (org-capture+-helm-select-template nil occ-capture+-helm-templates-alist))

(cl-defgeneric occ-capture (obj)
  "occ-capture")

(cl-defmethod occ-capture ((mrk marker))
  (org-capture+                ;TODO
   'entry
   `(marker ,mrk)
   'occ-capture+-helm-select-template
   :empty-lines 1))

(cl-defmethod occ-capture ((tsk occ-tsk))
  (let ((mrk (occ-tsk-marker tsk)))
    (occ-capture mrk)))

(cl-defmethod occ-capture ((ctxual-tsk occ-ctxual-tsk))
  (let ((mrk (occ-ctxual-tsk-marker ctxual-tsk)))
    (occ-capture mrk)))


;;; occ-obj-simple.el ends here
