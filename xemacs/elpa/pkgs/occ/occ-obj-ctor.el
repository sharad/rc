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

(cl-defmethod occ-collect-tsks (collection
                                &optional
                                force)
  (error "first argument should be of type (or occ-tree-collection occ-list-collection)"))

(cl-defmethod occ-collect-tsks ((collection occ-tree-collection)
                                &optional
                                force)
  (unless (occ-tree-collection-tree collection)
    (setf
     (occ-tree-collection-tree collection)
     (occ-tree-tsk-build
      #'(lambda ()
          (or
           (occ-make-tsk-at-point #'make-occ-tree-tsk)
           (make-occ-tree-tsk :name "empty tree tsk" :subtree nil))) ;; note: only using first file of roots
      (car (occ-tree-collection-roots collection))))))

(cl-defmethod occ-collect-files ((collection occ-tree-collection)
                                 &optional
                                 force)
  (unless (occ-tree-collection-files collection)
    (occ-collect-tsks collection nil)
    (setf
     (occ-tree-collection-files collection)
     (remove nil
             (delete-dups
              (let ((tsks (occ-collection collection))
                    (files '()))
                (occ-mapc-tree-tsks
                 #'(lambda (tsk args)
                     (push (occ-tsk-file tsk) files))
                 tsks
                 nil)
                files)))))
  (occ-tree-collection-files collection))

(cl-defmethod occ-collect-list ((collection occ-tree-collection))
  (let ((tsks (occ-collection collection))
        (tsk-list '()))
    (occ-mapc-tree-tsks
     #'(lambda (tsk args)
         (push tsk tsk-list))
     tsks
     nil)
    tsk-list))

(cl-defmethod occ-collect-list ((collection occ-list-collection))
  (let ((tsks (occ-collection collection)))
    tsks))

(cl-defmethod occ-collect-tsks ((collection occ-list-collection)
                                force)
  (unless (occ-list-collection-list collection)
    (setf
     (occ-list-collection-list collection)
     (remove nil
             (org-map-entries
              #'(lambda ()
                  (or
                   (occ-make-tsk-at-point #'make-occ-list-tsk)
                   (make-occ-list-tsk :name "empty list tsk")))
              t
              (occ-list-collection-roots collection))))))

(cl-defmethod occ-collect-files ((collection occ-list-collection)
                                 &optional
                                 force)
  (unless (occ-list-collection-files collection)
    (setf
     (occ-list-collection-files collection)
     (occ-list-collection-roots collection)))
  (occ-list-collection-files collection))

(cl-defmethod occ-collection ((collection occ-tree-collection))
  (unless (occ-tree-collection-tree occ-global-tsk-collection)
    (occ-collect-tsks occ-global-tsk-collection nil)
    (run-hooks 'occ-global-tsk-collection-change-hook))
  (occ-tree-collection-tree occ-global-tsk-collection))

(cl-defmethod occ-collection ((collection occ-list-collection))
  (unless (occ-list-collection-list occ-global-tsk-collection)
    (occ-collect-tsks occ-global-tsk-collection nil)
    (run-hooks 'occ-global-tsk-collection-change-hook))
  (occ-list-collection-list occ-global-tsk-collection))

;; (cl-defmethod occ-collection-files ((collection occ-tree-collection))
;;   (unless (occ-tree-collection-files occ-global-tsk-collection)
;;     (occ-collect-files occ-global-tsk-collection nil))
;;   (occ-tree-collection-files occ-global-tsk-collection))

(defun occ-collection-object ()
  (unless occ-global-tsk-collection
    (if occ-global-tsk-collection-spec
        (progn
          (occ-make-tsk-collection occ-global-tsk-collection-spec)
          (occ-collect-tsks occ-global-tsk-collection t))
      (progn
        (message "occ-global-tsk-collection-spec is nil, set using occ-set-global-tsk-collection-spec")
        (error "occ-global-tsk-collection-spec is nil"))))
  occ-global-tsk-collection)

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


(when nil
  (progn
    (setq occ-global-tsk-collection nil)
    (occ-make-tsk-collection occ-global-tsk-collection-spec)
    (occ-tree-collection-tree occ-global-tsk-collection)
    (occ-collect-tsks occ-global-tsk-collection t)
    (occ-tree-collection-roots occ-global-tsk-collection)
    (setf occ-gtree
          (occ-tree-collection-tree occ-global-tsk-collection)))


  (cl-get-field occ-gtree 'subtree)

  (cl-get-field occ-gtree 'plist)

  (cl-get-field (make-occ-tree-tsk :name "empty tree tsk" :subtree nil) 'subtree)

  (cl-set-field occ-gtree 'subtree 1)

  (cl-class-slots (cl-classname occ-gtree))
  ;; (type-of occ-gtree)

  (setf
   occ-test-gtree
   (occ-tsk-tree-build
    #'(lambda ()
        (or
         (occ-make-tsk-at-point #'make-occ-tree-tsk)
         (make-occ-tree-tsk :name "empty tree tsk" :subtree nil))) ;; note: only using first file of roots
    "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tsks/xx.org"))

  (setq occ-test-gtree
        (occ-tsk-tree-build
         #'(lambda ()
             (or
              (occ-make-tsk-at-point #'make-occ-tree-tsk)
              (make-occ-tree-tsk :name "empty tree tsk" :subtree nil))) ;; note: only using first file of roots
         ;; todo: occ-global-tsk-collection-spec
         org-ctx-clock-tsk-tree-tsk-root-org-file))

  (with-current-buffer (find-file-noselect "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tsks/xx.org")
    (goto-char (point-min))
    (setf occ-file-subtree
          (occ-org-map-subheading
           #'(lambda ()
               (occ-tsk-tree-collect-tsk
                #'(lambda ()
                    (or
                     (occ-make-tsk-at-point #'make-occ-tree-tsk)
                     (make-occ-tree-tsk :name "empty tree tsk" :subtree nil)))))))))

;;; occ-obj-ctor.el ends here
