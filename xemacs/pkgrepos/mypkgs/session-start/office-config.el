;;
;; office.el
;; Login : <spratap@spratap>
;; Started on  Wed Dec  1 17:11:05 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;



;; (define-minor-mode office-mode
;;     "Prepare for working with collarative office project."
;;   :initial-value nil
;;   :lighter " Office"
;;   :global nil
;;   (cond
;;     (office-mode
;;      (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
;;      )
;;     (t
;;      (add-hook 'before-save-hook 'delete-trailing-whitespace t t))))


;; This is the mode to be enabled when I am working in some files on
;; which other peoples are also working.





(define-minor-mode office-mode
    "Prepare for working with collarative office project."
  :initial-value nil
  :lighter " Office"
  :global nil
  (condition-case e
      (when office-mode
        (message "calling office mode")
        (set (make-local-variable 'before-save-hook) before-save-hook)
        (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
        (message "called office mode"))
    (error (message "Error: %s" e))))

(defvar taskdir "/home/s/paradise/tasks" "Task Directory")
(defvar current-task nil "Current task")

(defvar task-alist '(("bugs" (files "todo.org" "notes.org" "an0.org"))
                     ("features"
                      (files "reqirement.org" "feasibility.org"
                             "design.org" "todo.org" "notes.org" "an0.org"))))
(defvar task-file-properties '((buffer-read-only . t)
                               (fill-column . 172))
  "Task file properties.")

(defun find-task-dir (&optional force)
  (interactive "P")
  (if (or force (null current-task))
      (setq current-task (ido-read-directory-name "dir: " taskdir nil t))
      current-task))



(defun create-task (task name)
  (interactive
   (let* ((task (completing-read "what: " (mapcar 'car task-alist) nil t))
          (name (completing-read "name: " (directory-files (concat taskdir "/" task "/")) nil)))
     (list task name)))
  (let ((dir (concat taskdir "/" task "/")))
    (if (file-directory-p (concat dir name))
        (find-task (concat dir name))
        (progn

          (make-directory (concat dir name "/logs") t)

          (dolist (f (cdr (assoc 'files (cdr (assoc task task-alist)))))
            (let ((nfile (expand-file-name f (concat dir name "/")))
                  find-file-not-found-functions) ;find alternate of find-file-noselect to get non-existing file.
              (with-current-buffer (or (find-buffer-visiting nfile)
                                       (find-file-noselect nfile))
                (dolist (pv task-file-properties)
                  (add-file-local-variable-prop-line (car pv) (cdr pv)))
                (goto-char (point-max))
                (insert (format "\n\n* %s %s\n\n\n\n" (capitalize task) name ))
                (set-buffer-file-coding-system
                 (if (coding-system-p 'utf-8-emacs)
                     'utf-8-emacs
                     'emacs-mule))
                (write-file nfile))))

          (find-file (expand-file-name
                      (cadr (assoc 'files (cdr (assoc task task-alist))))
                      (concat dir name "/")))))

    (if (y-or-n-p (format "Should set %s current task" dir))
        (setq current-task dir))))

(defun find-task (task)
  (interactive
   (let ((task (ido-read-directory-name "dir: " taskdir nil t)))
     (list task)))
  (with-current-buffer (get-buffer-create (concat "*task*"))
    (dolist (f (directory-files task t "/*.org$"))
      (insert-file-contents f))
    (not-modified)
    (View-exit-and-edit)
    (make-local-variable 'view-read-only)
    (make-local-variable 'buffer-read-only)
    (setq view-read-only t
          buffer-read-only t))
  (switch-to-buffer "*task*")
  (org-mode))


(deh-section "Forgive"

  (defun forgive/them ()
    (interactive)
    (develock-mode -1)
    (highlight-changes-visible-mode -1)))



(provide 'office-config)

