;;; org-context-clocking-api-interaction.el --- org-context-clocking-api               -*- lexical-binding: t; -*-

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

;; "Interactive utitlity API's for adding root subtree etc"



(defvar org-context-clock-propery-set-functions-alist nil
  "Propery setting function.")

(defun org-context-clock-property-set-function (property fun)
  "fun is like org-icompleting-read
 (completing-read PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)"
  (push
   (cons property fun)
   org-context-clock-propery-set-functions-alist))

(defun org-context-clock-property-get-function (property)
  (cdr (assoc property org-context-clock-propery-set-functions-alist)))

(defun org-context-clock-get-property (prop)
  (org-get-property prop))

(defun org-context-clock-set-property (prop value context-plist &rest args)
  (org-set-property prop
                    (if value
                        value
                        (funcall
                         (org-context-clock-property-get-function prop)
                         context-plist args))))






(progn
  ;; (setq org-property-set-functions-alist nil)
  (org-context-clock-property-set-function "Root"
                                           '(lambda (context-plist &rest args)
                                             (let ((file (if context-plist (plist-get context-plist :file)))
                                                   (dir (if (stringp file) (file-name-directory file) default-directory)))
                                               (ido-read-directory-name
                                                (car args)
                                                dir dir))))
  (org-context-clock-property-set-function "SubtreeFile"
                                           '(lambda (context-plist&rest args)
                                             (file-relative-name
                                              (ido-read-file-name ;; org-iread-file-name
                                               (car args)
                                               default-directory default-directory)))))

;; (defun task-info-add-root ()
;;   (interactive)
;;   (if (org-set-property "Root" nil)
;;       (org-clocking-entry-update-task-infos t)))
;; (defun task-info-add-subtree-file ()
;;   (interactive)
;;   (if (org-set-property "SubtreeFile" nil)
;;       (org-clocking-entry-update-task-infos t)))

(defun org-context-clocking-select-propetry (&optional prompt)
  (ido-completing-read
   (or prompt "proptery: ")
   (list "Root" "SubtreeFile" "Done") nil t))

(defun org-context-clocking-test (context-plist timeout)
  (interactive '(nil nil))
  (lexical-let* ((timeout (or timeout 17))
                 (context-plist (or context-plist (org-context-clocking-build-context-plist)))
                 (buff (plist-get context-plist :buffer)))
    (message "test %s" timeout)))

(defun org-context-clocking-add-context-to-org-heading (context-plist timeout)
  (interactive '(nil nil))
  (lexical-let* ((timeout (or timeout 17))
                 (context-plist (or context-plist (org-context-clocking-build-context-plist)))
                 (buff (plist-get context-plist :buffer)))
    (if (and
         (eq (current-buffer) buff)
         (buffer-live-p buff)
         (not
          (eq buff
              (get-buffer "*helm-mode-org-context-clocking-add-context-to-org-heading*"))))
        (org-timed-miniwin-with-refile timeout nil
          (message "called add-context-to-org-heading %s" (current-buffer))
          (let ((timer (run-with-idle-timer timeout nil
                                            #'(lambda (w)
                                                (message "triggered timer for win %s" w)
                                                (when (active-minibuffer-window)
                                                  (abort-recursive-edit))
                                                (when (and w (windowp w) (window-valid-p w))
                                                  (delete-window w)))
                                            win)))
            (condition-case err
                (let ((buffer-read-only t))
                  (message "timer started for win %s" win)

                  ;; show proptery drawer
                  (let ((range (org-get-property-block (point) 'force)))
                    (when (eq org-cycle-subtree-status 'folded)
                      (org-show-entry)
                      (org-unlogged-message "CHILDREN")
                      (setq org-cycle-subtree-status 'children))
                    (when range
                      (goto-char (car range))
                      (when (org-at-drawer-p)
                        ;; show drawer
                        (let ((drawer (org-element-at-point)))
                          (when (memq (org-element-type drawer) '(drawer property-drawer))
                            (org-flag-drawer nil drawer)
                            ;; Make sure to skip drawer entirely or we might flag
                            ;; it another time when matching its ending line with
                            ;; `org-drawer-regexp'.
                            (goto-char (org-element-property :end drawer)))))))

                  (let ((prop nil))
                    (while (not
                            (string-equal "Done"
                                          (setq prop (org-context-clocking-select-propetry))))
                      (when (org-context-clock-set-property prop nil context-plist)
                        (org-clocking-entry-update-task-infos t)))
                    (if win (delete-window win))
                    (if timer (cancel-timer timer))))
              ((quit error)
               (progn
                 (if win (delete-window win))
                 (if timer (cancel-timer timer))
                 (signal (car err) (cdr err)))))))
        (progn
          (message "not running add-context-to-org-heading 1 %s, 2 %s 3 %s"
                   (eq (current-buffer) buff)
                   (buffer-live-p buff)
                   (eq buff
                       (get-buffer "*helm-mode-org-context-clocking-add-context-to-org-heading*")))))))

(defun org-context-clocking-add-context-to-org-heading-when-idle (context-plist timeout)
  (message "called add-context-to-org-heading-when-idle")
  (run-with-idle-timer-nonobtrusive-simple
   7 nil
   #'(lambda (args)
       (apply 'org-context-clocking-add-context-to-org-heading args)) (list context-plist timeout)))

(provide 'org-context-clocking-api-interaction)
;;; org-context-clocking-api-interaction.el ends here
