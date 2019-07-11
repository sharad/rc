;;; occ-commands.el --- occ commands                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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


(require 'occ)
(require 'occ-main)
(require 'occ-cl-utils)
(require 'occ-obj-utils)


;; example of clos in cl-struct-js2-export-binding-node is a variable defined in ‘js2-mode.el’.

;;;###autoload
(defun occ-helm-match-select (ctx)
  (interactive
   (list (occ-make-ctx-at-point)))
  (let ((filters            (occ-match-filters))
        (builder            #'occ-build-ctxual-tsk-with)
        (action             (occ-helm-intractive-command-actions))
        (action-transformer #'(lambda (action candidate)
                                (occ-helm-intractive-command-actions)))
        (timeout            occ-idle-timeout))
   (occ-helm-select ctx
                    :filters            filters
                    :builder            builder
                    :action             action
                    :action-transformer action-transformer
                    :timeout            timeout)))

(defun occ-helm-list-select (ctx)
  (interactive
   (list (occ-make-ctx-at-point)))
  (let ((filters            (occ-list-filters))
        (builder            #'occ-build-ctsk-with)
        (action             (occ-helm-intractive-command-actions))
        (action-transformer #'(lambda (action candidate)
                                (occ-helm-intractive-command-actions)))
        (timeout            occ-idle-timeout))
   (occ-helm-select ctx
                    :filters            filters
                    :builder            builder
                    :action             action
                    :action-transformer action-transformer
                    :timeout            timeout)))

(defun occ-helm-list-debug-select (ctx)
  (interactive
   (list (occ-make-ctx-at-point)))
  (let ((filters            (occ-list-filters))
        (builder            #'occ-build-ctsk-with)
        (action             (occ-helm-intractive-command-actions))
        (return-transform   t)
        (action-transformer #'(lambda (action candidate)
                                (occ-helm-intractive-command-actions)))
        (timeout            occ-idle-timeout))
    (let ((retval-ctx-tsk (occ-helm-select ctx
                                           :filters            filters
                                           :builder            builder
                                           :return-transform   return-transform
                                           :action             action
                                           :action-transformer action-transformer
                                           :timeout            timeout)))
      (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                        retval-ctx-tsk
                        (occ-format (occ-return-get-value retval-ctx-tsk) 'capitalize)
                        (occ-return-get-label retval-ctx-tsk))
      (if (and
           (occ-return-in-labels-p retval-ctx-tsk occ-return-select-label)
           (occ-return-get-value retval-ctx-tsk))
          (let ((ctsk     (occ-return-get-value retval-ctx-tsk))
                (launcher (cdr (assoc (completing-read "Action: " action) action))))
            (funcall launcher ctsk))
        (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): No selection")))))

(defun occ-helm-list-launch (ctx)
  (interactive
   (list (occ-make-ctx-at-point)))
  (let ((filters            (occ-list-filters))
        (builder            #'occ-build-ctsk-with)
        (return-transform   t)
        (action             (occ-helm-intractive-command-actions))
        (action-transformer #'(lambda (action candidate)
                                (occ-helm-intractive-command-actions)))
        (timeout            occ-idle-timeout))
    (let ((retval-ctx-tsk (occ-helm-select ctx
                                           :filters            filters
                                           :builder            builder
                                           :return-transform   return-transform
                                           :action             action
                                           :action-transformer action-transformer
                                           :timeout            timeout)))
       (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                         retval-ctx-tsk
                         (occ-format (occ-return-get-value retval-ctx-tsk) 'capitalize)
                         (occ-return-get-label retval-ctx-tsk))
       (if (and
            (occ-return-in-labels-p retval-ctx-tsk occ-return-select-label)
            (occ-return-get-value retval-ctx-tsk))
           (let* ((action      (occ-helm-intractive-launch-actions))
                  (ctx-tsk     (occ-return-get-value retval-ctx-tsk))
                  (launcher    (cdr (assoc (completing-read "Action: " action) action))))
             (funcall launcher ctx-tsk))
         (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): No selection")))))


;;;###autoload
(defun occ-curr-procreate-child ()
  (interactive)
  (let ((ctxual-tsk (occ-current-ctxual-tsk)))
    (if ctxual-tsk
        (occ-procreate-child ctxual-tsk)
      (occ-message "No current task clocking-in"))))

;;;###autoload
(defun occ-curr-procreate-child-clock-in ()
  (interactive)
  (let ((ctxual-tsk (occ-current-ctxual-tsk)))
    (if ctxual-tsk
        (occ-procreate-child-clock-in ctxual-tsk)
      (occ-message "No current task clocking-in"))))


;;;###autoload
(defun occ-proprty-edit ()
  (interactive)
  (let ((ctx (occ-make-ctx-at-point)))
    (occ-props-window-edit ctx
                           :action             (occ-props-edit-helm-actions ctx)
                           :action-transformer #'occ-props-edit-helm-action-transformer-fun)))


;;;###autoload
(defun occ-run-timer ()
  (interactive)
  (occ-run-curr-ctx-timer))


;;;###autoload
(defun occ-reset-collection-object ()
  (interactive)
  (setq occ-global-tsk-collection nil)
  occ-global-tsk-collection)


;;;###autoload
(defun occ-merge-unamed-task ()
  (interactive)
  (error "Implement it."))


;;;###autoload
(defun occ-start-day ()
  (interactive)
  ;; also detect if day is started.
  (error "Implement it."))

(defun occ-show-up (mins)
  (interactive)
  ;; https://www.merriam-webster.com/thesaurus/pack%20(up%20or%20off)
  (error "Implement it."))

(defun occ-stop-day ()
  (interactive)
  (error "Implement it."))

(defun occ-pack-up (mins)
  (interactive)
  ;; https://www.merriam-webster.com/thesaurus/pack%20(up%20or%20off)
  (error "Implement it."))

;; action
(cl-defmethod occ-log-not ()
  (error "Implement it."))

(cl-defmethod occ-curr-tsk-log-not ()
  (error "Implement it."))


(defun occ-curr-tsk-continyue-for (mins)
  (error "Implement it."))


(defun occ-clock-in-force ()
  (error "Implement it, open context ctx if not present, then occ-clock-in-if-associable else show error."))

(defun occ-interrupt-clock-in (mins)
  (error "Implement it."))

(defun occ-continue-prev ()
  (error "Implement it."))

;; TODO: direct prop edit/add/replace/remove etc from helm menu


;; implement console.

;; TODO: direct prop edit/add/replace/remove etc from helm menu


;;;###autoload
(defun occ-reset-spec ()
  (interactive)
  (setq occ-global-tsk-collection-spec nil))

;;;###autoload
(defun occ-make-spec ()
  (interactive)
  (if occ-global-tsk-collection-spec
      (occ-message "spec: %s already present, first reset it with occ-reset-spec"
                   occ-global-tsk-collection-spec)
    (let ((spec (completing-read "Spec: "(occ-specs))))
      (when spec
        (push (intern spec)
              occ-global-tsk-collection-spec)
        (occ-reset-collection-object)))))

;;;###autoload
(defun occ-add-to-spec (file)
  (interactive "FSpec file: ")
  ;; TODO: Improve to create direct tree from here rather than resetting whole occ-global-collection-object
  (unless (memq file (cdr occ-global-tsk-collection-spec))
    (let ((spec       (car occ-global-tsk-collection-spec))
          (spec-files (cdr occ-global-tsk-collection-spec)))
     (setq spec-files
           (if current-prefix-arg
               (nconc (list file) spec-files)
             (nconc spec-files (list file))))
     (setq occ-global-tsk-collection-spec
           (nconc (list spec) spec-files)))
    (prog1
        occ-global-tsk-collection-spec
      (occ-reset-collection-object))))

;;;###autoload
(defun occ-build-spec ()
  (interactive)
  (occ-make-spec)
  (wehn (car occ-global-tsk-collection-spec)
        (occ-add-to-spec (read-file-name "Spec file: ")))
  (prog1
      occ-global-tsk-collection-spec
    (occ-reset-collection-object)))


;;;###autoload
(defun occ-insinuate (&optional spec)
  (interactive)
  (occ-debug :debug "occ-insinuate: begin")
  (occ-message "occ-insinuate: begin")
  (occ-initialize)
  (progn
    (occ-reset-collection-object)
    ;; (add-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer t)
    ;; (add-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer t)
    ;; (add-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer t)
    (add-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
    (add-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode))
  (dolist (prop (occ-properties-to-inherit nil))
    (let ((propstr
           (upcase (if (keywordp prop)
                       (substring (symbol-name prop) 1)
                     (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (push propstr org-use-property-inheritance))))
  (progn
    (unless occ-global-tsk-collection-spec
      (if (occ-valid-spec-p spec)
          (setq occ-global-tsk-collection-spec spec)
        (when (called-interactively-p 'interactive)
          (occ-build-spec)))))
  (org-clock-load) ;; newly added
 (occ-debug :debug "occ-insinuate: finish")
 (occ-message "occ-insinuate: finish"))

;;;###autoload
(defun occ-uninsinuate ()
  (interactive)
  (occ-debug :debug "occ-uninsinuate: begin")
  (occ-message "occ-uninsinuate: begin")
  (occ-uninitialize)
  (progn
    (occ-reset-collection-object)
    ;; (setq buffer-list-update-hook nil)

    ;; (remove-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer)
    ;; (remove-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer)
    ;; (remove-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer)
    ;; (remove-hook 'after-save-hook             'occ-after-save-hook-fun t)
    (remove-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
    (remove-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode))
  (dolist (prop (occ-properties-to-inherit nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (delete propstr org-use-property-inheritance))))
 (occ-debug :debug "occ-uninsinuate: finish")
 (occ-message "occ-uninsinuate: finish"))


;; testing verification
(defun occ-files-with-null-regex ()
  (interactive)
  (let ((files
         (remove-if
          #'(lambda (f)
              (with-current-buffer (find-file-noselect f)
                org-complex-heading-regexp))
          (occ-files))))
    (occ-message "files with null regex %s" files)))

;; testing verification
(defun occ-files-not-in-org-mode ()
  (interactive)
  (let ((files
         (remove-if
          #'(lambda (f)
              (with-current-buffer (find-file-noselect f)
                (eq major-mode 'org-mode)))
          (occ-files))))
    (occ-message "files not in org-mode %s" files)))


(defun occ-reload (&optional uncompiled)
  (interactive "P")
  (occ-reload-lib uncompiled))


(defun occ-version (&optional here full message)
  "Show the Occ version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive
   (list
    current-prefix-arg
    t
    (not current-prefix-arg)))
  (occ-message (occ-get-version here full message)))

;;; occ-commands.el ends here
