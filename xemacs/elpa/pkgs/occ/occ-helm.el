;;; occ-helm.el --- occ helm                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d _at_ GMail>
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

(provide 'occ-helm)


(require 'org-capture+-helm)
(require 'org-capture+-helm-dynamic)


(require 'occ-debug-method)
(require 'occ-obj-method)


;;;###autoload
(org-capture+-add-heading-template '(occ tsk todo) "TODO"
                                   "* TODO %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(occ tsk todo) "TODO"
                                   "* MILESTONE %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(occ tsk meeting) "MEETING"
                                   "* MEETING %? %^g\n %i\n [%a]\n")

;; (setq org-capture+-helm-templates-tree   (list t))
;; (org-capture+-add-heading-template '(xx) "TODO"    "* TODO %? %^g\n %i\n [%a]\n")



(defvar occ-capture+-helm-templates-alist org-capture+-helm-templates-alist)

(defun occ-capture+-helm-select-template ()
  (let ((selector (helm-template-gen-selector #'org-capture+-tree-predicate
                                              '(t occ tsk todo)
                                              0)))
    (funcall selector)))


(defvar occ-helm-actions-plist nil)

(defun occ-helm-action-add (key name action)
  (setq occ-helm-actions-plist
        (plist-put
         occ-helm-actions-plist
         key (cons name action))))

(defun occ-helm-action-get (key)
  (plist-get occ-helm-actions-plist key))

(defun occ-helm-actions-get (&rest keys)
  (let ((actions nil))
    (dolist (key keys)
      (let ((name-action (occ-helm-action-get key)))
        (when name-action
          (setf actions (nconc actions (list name-action))))))
    actions))

(progn

  (setq occ-helm-actions-plist nil)

  (occ-helm-action-add :ignore                   "Ignore"                   #'ignore)
  (occ-helm-action-add :identity                 "Select"                   #'identity)
  (occ-helm-action-add :clock-in                 "Clock-in"                 #'occ-clock-in)
  (occ-helm-action-add :try-fast-clock-in        "Try Fast Clock-in"        #'occ-try-fast-clock-in)
  (occ-helm-action-add :try-clock-in             "Try Clock-in"             #'occ-try-clock-in)
  (occ-helm-action-add :procreate-child          "Procreate Child"          #'occ-procreate-child)
  (occ-helm-action-add :procreate-child-clock-in "Procreate Child Clock-in" #'occ-procreate-child-clock-in)
  (occ-helm-action-add :goto                     "Goto"                     #'occ-goto)
  (occ-helm-action-add :set-to                   "Set To"                   #'occ-set-to)
  (occ-helm-action-add :proprty-window-edit      "Proprtes Window Edit"     #'occ-props-window-edit) ;TODO: implement it.
  (occ-helm-action-add :rank                     "Get Rank"                 #'occ-print-rank)
  (occ-helm-action-add :tsk                      "Get Task"                 #'occ-print-tsk))


(defun occ-helm-general-actions ()
  (occ-helm-actions-get :procreate-child
                        :procreate-child-clock-in
                        :proprty-window-edit
                        :try-clock-in
                        :goto
                        :rank
                        :tsk))

(defun occ-helm-intractive-command-actions ()
  (occ-helm-actions-get :try-clock-in
                        :procreate-child
                        :procreate-child-clock-in
                        :proprty-window-edit
                        :try-clock-in
                        :goto
                        :rank
                        :tsk))

(defun occ-helm-intractive-launch-actions ()
  (occ-helm-actions-get :ignore
                        :identity
                        :try-clock-in
                        :procreate-child
                        :procreate-child-clock-in
                        :proprty-window-edit
                        :try-clock-in
                        :goto
                        :rank
                        :tsk))

(cl-defmethod occ-helm-actions ((obj null))
  (occ-helm-general-actions))

(cl-defmethod occ-helm-actions ((obj occ-obj))
  (occ-helm-general-actions))

;; (cl-defmethod occ-helm-actions ((obj occ-ctx))
;;   (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))


(cl-defmethod occ-helm-action-transformer ((obj null) actions)
  (occ-helm-actions obj))

(cl-defmethod occ-helm-action-transformer ((obj occ-obj-tsk) actions)
  (occ-helm-actions obj))

(cl-defmethod occ-helm-action-transformer ((obj occ-obj-ctx-tsk) actions)
  ;; (message "occ-helm-action-transformer: %s"
  ;;          (occ-gen-edits-for-add-remove obj))
  ;; (occ-helm-actions obj)
  (append
   (occ-helm-actions obj)
   ;; (occ-gen-edits-for-add-remove obj)
   (occ-gen-edits-if-required obj nil nil)))


(cl-defun occ-helm-action-transformer-fun (action candidate)
  (occ-helm-action-transformer candidate action))

;; (cl-defmethod occ-helm-action-transformer ((obj occ-ctx) actions)
;;   (list
;;    (cons "Clock-in" #'occ-clock-in)
;;    (cons "Child"    #'occ-procreate-child)))

;; (cl-defmethod occ-helm-action-transformer ((obj occ-ctxual-tsk) actions)
;;   (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))


(cl-defmethod occ-props-edit-helm-actions ((obj null))
  (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))

(cl-defmethod occ-props-edit-helm-actions ((obj occ-obj))
  (occ-helm-actions-get :procreate-child :procreate-child-clock-in :proprty-window-edit))

(cl-defmethod occ-props-edit-helm-action-transformer ((obj null) actions)
  (occ-props-edit-helm-actions obj))

(cl-defmethod occ-props-edit-helm-action-transformer ((obj occ-obj-tsk) actions)
  (occ-props-edit-helm-actions obj))

(cl-defmethod occ-props-edit-helm-action-transformer ((obj occ-obj-ctx-tsk) actions)
  (occ-props-edit-helm-actions obj))

(cl-defun occ-props-edit-helm-action-transformer-fun (action candidate)
  (occ-props-edit-helm-action-transformer candidate action))


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

;; (helm-build-dummy-source "Create tsk"
;;   :action (helm-make-actions
;;            "Create tsk"
;;            'sacha/helm-org-create-tsk))

(defun occ-helm-dummy-source ()
  (helm-build-dummy-source "Create tsk"
    :action (helm-make-actions
             "Create tsk"
             'sacha/helm-org-create-tsk)))

(cl-defmethod occ-helm-build-obj-source ((obj occ-obj-ctx) &optional actions)
  (occ-helm-build-candidates
   -source
   (occ-list obj)
   actions))

(cl-defun occ-helm-build-candidates-source (candidates
                                            &key
                                            unfiltered-count
                                            action
                                            action-transformer)
  (list
   (let ((unfiltered-count (or unfiltered-count 0)))
     (when candidates
       (helm-build-sync-source (format "Select matching %s(%d/%d)"
                                       (symbol-name (cl-inst-classname (car candidates)))
                                       (length candidates)
                                       unfiltered-count)
         :candidates (mapcar #'occ-candidate candidates)
         ;; :action actions
         :filtered-candidate-transformer nil
         :action-transformer action-transformer
         :history 'org-refile-history)))
   (occ-helm-dummy-source)
   (occ-helm-dummy-source)))


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

;;; occ-helm.el ends here
