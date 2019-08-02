;;; occ-helm-method.el --- occ helm method           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <spratap@merunetworks.com>
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

(provide 'occ-helm-method)


(defvar occ-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; (define-key map (kbd "RET")           'helm-ff-RET)
    (define-key map (kbd "C-]")           'helm-ff-run-toggle-basename)
    (define-key map (kbd "S-RET")         'occ-helm-run-child-clock-in)
    (helm-define-key-with-subkeys map (kbd "DEL") ?\d 'helm-ff-delete-char-backward
                                  '((C-backspace . helm-ff-run-toggle-auto-update)
                                    ([C-c DEL] . helm-ff-run-toggle-auto-update))
                                  nil 'helm-ff-delete-char-backward--exit-fn)
    (when helm-ff-lynx-style-map
      (define-key map (kbd "<left>")      'helm-find-files-up-one-level)
      (define-key map (kbd "<right>")     'helm-execute-persistent-action))
    (delq nil map))
  "Keymap for `helm-find-files'.")

(defvar occ-helm-doc-header " (\\<helm-find-files-map>\\[helm-find-files-up-one-level]: Go up one level)"
  "*The doc that is inserted in the Name header of a find-files or dired source.")

(defun occ-helm-run-child-clock-in ()
  "Run mail attach files command action from `helm-source-find-files'."
  ;; (interactive)                         ;TODO: to move to occ-commands.el
  (with-helm-alive-p
    (helm-exit-and-execute-action 'occ-child-clock-in)))
(put 'occ-helm-run-child-clock-in 'helm-only t)
;; add occ-child-clock-in in action


;;
;; https://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

;; function 'sacha/helm-org-create-tsk

;; (helm-build-dummy-source "Create tsk"
;;   :action (helm-make-actions
;;            "Create tsk"
;;            'sacha/helm-org-create-tsk))

(defun occ-helm-dummy-source (prompt action)
  (helm-build-dummy-source prompt
    :action (helm-make-actions
             prompt
             action)))

(cl-defmethod occ-helm-build-obj-source ((obj occ-obj-ctx) &optional actions)
  (occ-helm-build-candidates
   :source
   (occ-list obj)
   actions))


(cl-defmethod occ-helm-build-candidates-source ((obj        occ-ctx)
                                                (candidates list)
                                                &key
                                                unfiltered-count
                                                filters
                                                builder
                                                action
                                                action-transformer
                                                auto-select-if-only
                                                timeout)
  (let ((filtered-count (length candidates))
        (called-never   t))
     (let ((gen-candidates   #'(lambda ()
                                 (mapcar #'occ-candidate
                                         (if called-never
                                             (progn
                                               (setq called-never nil)
                                               candidates)
                                           (let* ((candidates-unfiltered (occ-list obj
                                                                                   :builder builder))
                                                  (candidates-filtered   (occ-filter obj
                                                                                     filters
                                                                                     candidates-unfiltered)))
                                             (setq filtered-count
                                                   (length candidates-filtered))
                                             candidates-filtered))))))
       (when (> unfiltered-count 0)
         (helm-build-sync-source
             (format "Select matching %s(%d/%d)"
                     (symbol-name (cl-inst-classname (car candidates)))
                     unfiltered-count
                     filtered-count)
           ;; :header-name
           :candidates                     #'(lambda ()
                                               (funcall gen-candidates))
           :action                         action
           :filtered-candidate-transformer nil
           :action-transformer             action-transformer
           :history                        'org-refile-history)))))

(cl-defmethod occ-helm-build-candidates-sources ((obj        occ-ctx)
                                                 (candidates list)
                                                 &key
                                                 unfiltered-count
                                                 filters
                                                 builder
                                                 action
                                                 action-transformer
                                                 auto-select-if-only
                                                 timeout)
  (list
   (occ-helm-build-candidates-source obj
                                     candidates
                                     :unfiltered-count   unfiltered-count
                                     :filters            filters
                                     :builder            builder
                                     :action             action
                                     :action-transformer action-transformer)
   (occ-helm-dummy-source "Create fast tsk"     #'occ-fast-procreate-child-clock-in)
   (occ-helm-dummy-source "Create template tsk" #'occ-procreate-child-clock-in)))


(cl-defgeneric occ-helm-select (obj
                                &key
                                filters
                                builder
                                return-transform
                                action
                                action-transformer
                                timeout)
  "occ-helm-select")

(cl-defmethod occ-helm-select ((obj occ-ctx)
                               &key
                               filters
                               builder
                               return-transform
                               action
                               action-transformer
                               timeout)
  (let ((ctx-tsk (occ-select obj
                             :filters            filters
                             :builder            builder
                             :return-transform   return-transform
                             :action             action
                             :action-transformer action-transformer
                             :timeout            timeout)))
    (occ-debug :debug "Selected ctxual-tsk %s" (occ-format ctx-tsk 'capitalize))
    ctx-tsk))







(defun occ-helm-select-XYZ (obj
                            selector
                            action)
  ;; here
  ;; (occ-debug :debug "sacha marker %s" (car ctxasks))
  (let (helm-sources)
    (push
     (occ-helm-build-obj-source obj (list (cons "Clock in and track" selector)))
     helm-sources)

    (when (and
           (org-clocking-p)
           (marker-buffer org-clock-marker))
      (push
       (helm-build-sync-source "Current Clocking Tsk"
         :candidates (list (occ-candidate
                            (occ-build-obj-with (occ-current-tsk) obj)))
         :action (list
                  (cons "Clock in and track" selector)))
       helm-sources))
    (funcall action (helm helm-sources))))




;; (cl-defgeneric occ-sacha-helm-action (ctxask clockin-fn)
;;   "occ-sacha-helm-action")

;; (cl-defmethod occ-sacha-helm-action ((ctxask occ-ctxual-tsk) clockin-fn)
;;   ;; (message "sacha marker %s" (car dyntskpls))
;;   ;; (setq sacha/helm-org-refile-locations tbl)
;;   (progn
;;     (helm
;;      (list
;;       (helm-build-sync-source "Select matching tsks"
;;         :candidates (mapcar 'occ-candidate ctxask)
;;         :action (list ;; (cons "Select" 'identity)
;;                  (cons "Clock in and track" #'(lambda (c) (funcall clockin-fn c))))
;;         :history 'org-refile-history)))))
;; ;; (helm-build-dummy-source "Create tsk"
;; ;;   :action (helm-make-actions
;; ;;            "Create tsk"
;; ;;            'sacha/helm-org-create-tsk))

;;; occ-helm-method.el ends here
