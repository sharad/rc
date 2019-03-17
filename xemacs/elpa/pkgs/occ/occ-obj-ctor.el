;;; occ-obj-ctor.el --- occ-api               -*- lexical-binding: t; -*-
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

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html

;;; Code:



(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj)


(provide 'occ-obj-ctor)


(defvar occ-global-tsk-collection-spec        nil)
(defvar occ-global-tsk-collection             nil)
(defvar occ-global-tsk-collection-change-hook nil
  "run when occ-global-tsk-collection-change-hook get changed.")

(defun occ-tsk-builder ()
  (unless occ-global-tsk-collection (occ-collection-object))
  (if occ-global-tsk-collection
      (let ((classname (cl-classname (occ-collection-object))))
        ;;let ((classname (cl-classname occ-global-tsk-collection)))
        (cond
         ((eq 'occ-list-collection classname)
          #'make-occ-list-tsk)
         ((eq 'occ-tree-collection classname)
          #'make-occ-tree-tsk)
         (t (error "occ-global-tsk-collection is not from occ-list-collection or occ-tree-collection class"))))
    (error "occ-global-tsk-collection is NIL not from occ-list-collection or occ-tree-collection class")))

(defun occ-heading-content-only ()
  (if (org-at-heading-p)
      (save-excursion
        (save-restriction
          (let ((start (progn
                         (goto-char (org-element-property :contents-begin (org-element-at-point)))
                         (while (org-at-drawer-p)
                           (goto-char (org-element-property :end (org-element-at-point))))
                         ;; (if (org-at-heading-p) (backward-char))
                         (point))))
            (unless (org-at-heading-p)
              (progn
                (outline-next-heading)
                ;; (outline-next-visible-heading 1)
                (backward-char)
                (buffer-substring start (point)))))))))

(defun occ-make-tsk-at-point (&optional builder)
    ;; (org-element-at-point)
    (let ((builder (or builder
                       (occ-tsk-builder))))
        (let (tsk
              (heading-with-string-prop
               (if (org-before-first-heading-p)
                   "empty heading"
                 (org-get-heading 'notags))))
          (let ((heading (when heading-with-string-prop
                           (substring-no-properties heading-with-string-prop)))
                (heading-prop (if heading-with-string-prop
                                  heading-with-string-prop))
                (marker  (move-marker
                          (make-marker)
                          (point)
                          (org-base-buffer (current-buffer))))
                (file    (buffer-file-name))
                (point   (point))
                (clock-sum (if (org-before-first-heading-p)
                               0
                             (org-clock-sum-current-item)))
                (tsk-plist (cadr (org-element-at-point))))
            (when heading
              (setf tsk
                    (funcall builder
                             :name    heading
                             :heading heading
                             :heading-prop heading-prop
                             :marker  marker
                             :file file
                             :point point
                             :clock-sum clock-sum
                             :plist tsk-plist))

              (let ((inherited-props
                     ;; is it correct ?
                     (cl-method-param-case
                      '(occ-readprop (`((head ,val) occ-ctx) val)))))
                (dolist (prop inherited-props)
                  (let* ((propstr (if (keywordp prop)
                                      (substring (symbol-name prop) 1)
                                    (symbol-name prop)))
                         (val (org-entry-get nil propstr t)))
                    (unless (occ-get-property tsk prop)
                      (occ-set-property tsk prop val))))))
            tsk))))

(cl-defmethod occ-make-tsk ((n number)
                            &optional builder)
  (occ-debug :debug "point %s" n)
  (if (<= n (point-max))
      (save-restriction
        (save-excursion
          (goto-char n)
          (occ-make-tsk-at-point builder)))))

(cl-defmethod occ-make-tsk ((m marker)
                            &optional builder)
  (occ-debug :debug "point %s" m)
  (if (and
       (marker-buffer m)
       (numberp (marker-position m)))
      (with-current-buffer (marker-buffer m)
        (if (<= (marker-position m) (point-max))
            (occ-make-tsk (marker-position m) builder)))))

(defun occ-make-ctx (&optional buff)
  (let* ((buff (if buff
                   (if (bufferp buff)
                       buff
                     (if (stringp buff)
                         (or
                          (get-buffer buff)
                          (if (file-exists-p buff)
                              (get-file-buffer buff)))))
                 (window-buffer)))
         (buf (org-base-buffer buf))
         (file (buffer-file-name buff))
         (ctx (make-occ-ctx
               :name (buffer-name buff)
               :file file
               :buffer buff)))
    ctx))

(cl-defmethod occ-make-tsk ((m null)
                            &optional builder)
  (occ-debug :debug "current pos %s" (point-marker))
  (occ-make-tsk (point-marker) builder))

(cl-defgeneric occ-make-ctxual-tsk (tsk ctx rank)
  "occ-make-ctxual-tsk")

(cl-defmethod occ-make-ctxual-tsk ((tsk occ-tsk)
                                   (ctx occ-ctx)
                                   rank)
  ;; use occ-build-ctxual-tsk
  (make-occ-ctxual-tsk
   :name    nil
   :tsk     tsk
   :ctx     ctx
   :rank    rank))

(cl-defmethod occ-build-ctxual-tsk ((tsk occ-tsk) ;ctor
                                    (ctx occ-ctx))
  (occ-make-ctxual-tsk tsk
                       ctx
                       (occ-rank tsk ctx)))


(cl-defmethod occ-build-obj ((tsk occ-tsk) (obj occ-ctx))
   (occ-build-ctxual-tsk tsk ctx))

(cl-defmethod occ-build-obj ((tsk occ-tsk) (obj null))
  tsk)


(cl-defmethod occ-make-tsk-collection ((file-spec (head :tree)))
  (unless occ-global-tsk-collection
    (let ((collection (make-occ-tree-collection
                       :name "tsk collection tree"
                       :roots (cdr file-spec))))
      (setf occ-global-tsk-collection collection))))

(cl-defmethod occ-make-tsk-collection ((file-spec (head :list)))
  (unless occ-global-tsk-collection
    (let ((collection (make-occ-list-collection
                       :name "tsk collection list"
                       :roots (cdr dir-spec))))
      (setf occ-global-tsk-collection collection))))

;;;###autoload
(defun occ-run-with-global-tsk-collection (fn)
  (if occ-global-tsk-collection
      (funcall fn)
    (add-hook
     'occ-global-tsk-collection-change-hook
     fn)))

;;;###autoload
(defun occ-reset-collection-object ()
  (interactive)
  (setq occ-global-tsk-collection nil)
  occ-global-tsk-collection)


;;; occ-obj-ctor.el ends here
