;;; org-context-clock-api-interaction.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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


(require 'timer-utils-lotus)
(require 'org-misc-utils-lotus)
(eval-when-compile
  '(require 'org-misc-utils-lotus))

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

(defun org-context-clock-set-property (prop value context &rest args)
  (org-set-property prop
                    (if value
                        value
                        (funcall
                         (org-context-clock-property-get-function prop)
                         prop context args)))
  t)

(progn
  (setq org-property-set-functions-alist nil)
  (org-context-clock-property-set-function "Root"
                                           '(lambda (prop context &rest args)
                                             (let* ((file (if context (plist-get context :file)))
                                                    (dir (if (stringp file) (file-name-directory file) default-directory))
                                                    (prompt (concat prop ": ")))
                                               (ido-read-directory-name
                                                prompt
                                                dir dir))))
  (org-context-clock-property-set-function "SubtreeFile"
                                           '(lambda (prop context &rest args)
                                             (let ((prompt (concat prop ": ")))
                                               (file-relative-name
                                                (ido-read-file-name ;; org-iread-file-name
                                                 prompt
                                                 default-directory default-directory))))))

;; (defun task-add-root ()
;;   (interactive)
;;   (if (org-set-property "Root" nil)
;;       (org-clocking-task-update-tasks t)))
;; (defun task-add-subtree-file ()
;;   (interactive)
;;   (if (org-set-property "SubtreeFile" nil)
;;       (org-clocking-task-update-tasks t)))

(defun org-context-clock-select-propetry (&optional prompt)
  ;; (ido-completing-read
  (completing-read
   (or prompt "proptery: ")
   (list "Root" "SubtreeFile" "Edit" "Done") nil t))

(defun org-context-clock-test (context timeout)
  (interactive '(nil nil))
  (lexical-let* ((timeout (or timeout 17))
                 (context (or context (org-context-clock-build-context)))
                 (buff (plist-get context :buffer)))
    (message "test %s" timeout)))

(defun org-flag-proprty-drawer-at-marker (marker flag)
  (let ((buff (marker-buffer marker))
        (loc (marker-position marker)))
    (when (and buff loc)
      (with-current-buffer buff
        (goto-char loc)
        (let ((range (org-get-property-block (point) 'force)))
          ;; first show hreading
          (when (eq org-cycle-subtree-status 'folded)
            (unless flag (org-show-entry)) ; changed from org-show-task to org-show-entry
            (org-unlogged-message "CHILDREN")
            (setq org-cycle-subtree-status 'children))
          ;; show expand property if flag is nil, else hide
          (when range
            (goto-char (1- (car range)))
            (message "reached to drawer")
            (if (org-at-drawer-p)
                ;; show drawer
                (let ((drawer (org-element-at-point)))
                  (when (memq (org-element-type drawer) '(node-property drawer property-drawer))
                    (message "trying to open drawer %s" drawer)
                    (org-flag-drawer flag drawer)
                    ;; Make sure to skip drawer entirely or we might flag
                    ;; it another time when matching its ending line with
                    ;; `org-drawer-regexp'.
                    (goto-char (org-element-property :end drawer))))
                (message "not at drawer"))
            (message "reached to drawer1")))))))

(defvar org-context-clock-add-context-to-org-heading-win-config nil)

;;;###autoload
(defun org-context-clock-add-context-to-org-heading (context timeout)
  (interactive '(nil nil))

  ;; TODO: as clean up reset win configuration
  (lexical-let* ((timeout (or timeout 17))
                 (context (or context (org-context-clock-build-context)))
                 (buff (plist-get context :buffer)))
    (if (and
         (eq (current-buffer) buff)
         (buffer-live-p buff)
         (not
          (eq buff
              (get-buffer "*helm-mode-org-context-clock-add-context-to-org-heading*"))))

        (progn
          (setq org-context-clock-add-context-to-org-heading-win-config (current-window-configuration))
          ;; TODO: do win clean uin in org-timed-miniwin-file-loc-with-refile macro not here.
          ;;       and make and use it own org-context-clock-add-context-to-org-heading-win-config variable for it.
          (progn  ;; let (win file pos timeout)   ;pos is void error is seen ?
            (org-timed-miniwin-file-loc-with-refile
                win file pos timeout
                '((org-context-clock-task-update-files :maxlevel . 4))
                ;; (set-marker marker (point))
                (lexical-let* ((marker (make-marker)))
                  (set-marker marker (point))
                  (message "called add-context-to-org-heading %s" (current-buffer))
                  (let ((timer (run-with-idle-timer timeout nil
                                                    #'(lambda (w)
                                                        (message "triggered timer for win %s" w)
                                                        (save-excursion
                                                          (org-flag-proprty-drawer-at-marker marker t))
                                                        (when (active-minibuffer-window)
                                                          (abort-recursive-edit))
                                                        (when (and w (windowp w) (window-valid-p w))
                                                          (delete-window w))
                                                        (when org-context-clock-add-context-to-org-heading-win-config
                                                          (set-window-configuration org-context-clock-add-context-to-org-heading-win-config)
                                                          (setq org-context-clock-add-context-to-org-heading-win-config nil)))
                                                    win)))
                    (condition-case err
                        (let ((buffer-read-only nil))
                          (message "timer started for win %s" win)

                          ;; show proptery drawer
                          (org-flag-proprty-drawer-at-marker marker nil)

                          ;; try to read values of properties.
                          (let ((prop nil))
                            (while (not
                                    (member (setq prop (org-context-clock-select-propetry)) '("Edit" "Done")))
                              (when (org-context-clock-set-property prop nil context)
                                (org-context-clock-task-update-tasks t)))
                            (cond
                              ((string-equal "Done" prop)
                               (save-excursion
                                 (org-flag-proprty-drawer-at-marker marker t))
                               (when (and win (windowp win) (window-valid-p win))
                                 (delete-window win))
                               (when org-context-clock-add-context-to-org-heading-win-config
                                 (set-window-configuration org-context-clock-add-context-to-org-heading-win-config)
                                 (setq org-context-clock-add-context-to-org-heading-win-config nil))
                               (when timer (cancel-timer timer)))
                              ((string-equal "Edit" prop)
                               (when timer (cancel-timer timer))
                               (when (and win (windowp win) (window-valid-p win))
                                 (select-window win 'norecord)))
                              (t
                               (save-excursion
                                 (org-flag-proprty-drawer-at-marker marker t))
                               (when (and win (windowp win) (window-valid-p win))
                                 (delete-window win))
                               (when org-context-clock-add-context-to-org-heading-win-config
                                 (set-window-configuration org-context-clock-add-context-to-org-heading-win-config)
                                 (setq org-context-clock-add-context-to-org-heading-win-config nil))
                               (when timer (cancel-timer timer))))))
                      ((quit)
                       (progn
                         (when (and win (windowp win) (window-valid-p win))
                           (delete-window win))
                         (when org-context-clock-add-context-to-org-heading-win-config
                           (set-window-configuration org-context-clock-add-context-to-org-heading-win-config)
                           (setq org-context-clock-add-context-to-org-heading-win-config nil))
                         (if timer (cancel-timer timer))
                         (signal (car err) (cdr err))))))))))
        (progn
          (message "not running add-context-to-org-heading 1 %s, 2 %s 3 %s"
                   (eq (current-buffer) buff)
                   (buffer-live-p buff)
                   (eq buff
                       (get-buffer "*helm-mode-org-context-clock-add-context-to-org-heading*")))))))

;;;###autoload
(defun org-context-clock-add-context-to-org-heading-when-idle (context timeout)
  (message "called add-context-to-org-heading-when-idle")
  (run-with-idle-timer-nonobtrusive-simple
   7 nil
   #'(lambda (args)
       (apply 'org-context-clock-add-context-to-org-heading args)) (list context timeout)))

(provide 'org-context-clock-api-interaction)
;;; org-context-clock-api-interaction.el ends here
