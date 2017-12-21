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

(eval-when-compile
  (require 'org-context-clock-assoc-common))

(require 'org-context-clock-assoc-common)

(defun org-context-clock-get-property (prop)
  (org-get-property prop))

(defun org-context-clock-set-property (key value context &rest args)
  (org-set-property prop
                    (if value
                        value
                        (funcall
                         (org-context-clock-key-fun key :getter)
                         nil context args)))
  t)

(defun org-context-clock-select-propetry (&optional prompt)
  ;; (ido-completing-read
  (completing-read
   (or prompt "proptery: ")
   (org-context-clock-keys-with-operation :getter) nil t))

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

;;;###autoload
(defun org-context-clock-add-context-to-org-heading (context timeout)
  "add-context-to-org-heading"

  ;; TODO: make helm conditional when it is used than only it should be handled.

  (interactive '(nil nil))

  (org-with-no-active-minibuffer
      (progn
        (message "add-context-to-org-heading: minibuffer already active quitting")
        (message nil))
    (lexical-let* ((timeout (or timeout 17))
                   (context (or context (org-context-clock-build-context)))
                   (buff (plist-get context :buffer)))
      (if (and
           (eq (current-buffer) buff)
           (buffer-live-p buff)
           (not
            (eq buff
                (get-buffer "*helm-mode-org-context-clock-add-context-to-org-heading*"))))

          (org-with-file-loc-timed-refile
              file pos
              timeout '((org-context-clock-task-update-files :maxlevel . 4))
              (lexical-let* ((marker (make-marker))
                             (local-cleanup
                              #'(lambda ()
                                  (save-excursion ;what to do here
                                    (org-flag-proprty-drawer-at-marker marker t)))))

                (set-marker marker (point))

                (org-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
                    timeout timer cleanup local-cleanup win

                    (let ((target-buffer (find-file-noselect file)))

                      (switch-to-buffer target-buffer)
                      (goto-char pos)


                      (message "called add-context-to-org-heading %s" (current-buffer))
                      (progn
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
                                   (funcall cleanup win local-cleanup)
                                   (when timer (cancel-timer timer)))
                                  ((string-equal "Edit" prop)
                                   (when timer (cancel-timer timer))
                                   (when (and win (windowp win) (window-valid-p win))
                                     (select-window win 'norecord)))
                                  (t
                                   (funcall cleanup win local-cleanup)
                                   (when timer (cancel-timer timer))))))
                          ((quit)
                           (progn
                             (funcall cleanup win local-cleanup)
                             (if timer (cancel-timer timer))
                             (signal (car err) (cdr err))))))))))
          (progn
            (org-context-clock-message 6 "not running add-context-to-org-heading 1 %s, 2 %s 3 %s"
                                       (eq (current-buffer) buff)
                                       (buffer-live-p buff)
                                       (eq buff
                                           (get-buffer "*helm-mode-org-context-clock-add-context-to-org-heading*"))))))))

;;;###autoload
(defun org-context-clock-add-context-to-org-heading-when-idle (context timeout)
  (org-context-clock-message 6 "called add-context-to-org-heading-when-idle")
  (run-with-idle-timer-nonobtrusive-simple
   7 nil
   #'(lambda (args)
       (apply 'org-context-clock-add-context-to-org-heading args)) (list context timeout)))

(provide 'org-context-clock-api-interaction)
;;; org-context-clock-api-interaction.el ends here
