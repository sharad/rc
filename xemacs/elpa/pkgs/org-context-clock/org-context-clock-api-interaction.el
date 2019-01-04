;;; org-context-clock-api-interaction.el --- org-context-clock-api               -*- lexical-binding: t; -*-

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

;;

;;; Code:

;; "Interactive utitlity API's for adding root subtree etc"

(require 'org)

(require 'timer-utils-lotus)
(require 'org-misc-utils-lotus)
(eval-when-compile
  '(require 'org-misc-utils-lotus))
(require 'org-capture+)
(eval-when-compile
  (require 'org-context-clock-assoc-common))
(require 'org-context-clock-assoc-common)



;; BUG `org-get-property' is not known to be defined.

(defun org-get-property (prop-key)
  (org-entry-get nil prop-key))

(defun org-context-clock-get-property (prop-key)
  (org-get-property prop-key))

(defun org-context-clock-set-property (prop-key value context &rest args)
  (let ((prop-key-str (if (eq (elt prop-key 0 ) ?\:) (substring prop-key 1))))
    (org-set-property prop-key
                      (if value
                          value
                          (funcall
                           (org-context-clock-key-fun prop-key :getter)
                           prop-key nil context args))))
  t)

;; (eq (elt ":root" 0) ?\:)

;; (org-context-clock-select-propetry nil)

;; (org-context-clock-keys-with-operation :getter nil)

;; (org-context-clock-set-property (intern ":root") nil (list :file "/home/s/paradise/git/main/src/wnc/security/authenticator/ieee802_1x.cpp" :buffer (get-buffer "ieee802_1x.cpp")))

(defun org-context-clock-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  (let ((helm-always-two-windows nil))
    (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))

(defun org-context-clock-select-propetry (context &optional prompt)
  (let ((prompt (or prompt "proptery: "))
        (keys (mapcar #'(lambda (k) (cons (symbol-name k) k))
                      (append
                       (org-context-clock-keys-with-operation :getter context)
                       '(edit done)))))
    (cdr (assoc (org-context-clock-completing-read prompt keys  nil t) keys))))

(defun org-context-clock-test (context timeout)
  (interactive '(nil nil))
  (lexical-let* ((timeout (or timeout 7))
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

(defun org-get-flag-proprty-drawer-at-marker (marker)
  (let ((buff (marker-buffer marker))
        (loc (marker-position marker)))
    (when (and buff loc)
      (with-current-buffer buff
        (when (goto-char loc)
          (let ((range (org-get-property-block (point) 'force)))
            org-cycle-subtree-status))))))

;;;###autoload
(defun org-context-clock-add-context-to-org-heading (context timeout)
  "add-context-to-org-heading"

  ;; TODO: make helm conditional when it is used than only it should be handled.

  (interactive '(nil 7))

  (lotus-with-no-active-minibuffer-if
   (progn
     (message "add-context-to-org-heading: minibuffer already active quitting")
     (message nil))
   (lexical-let* ((timeout (or timeout 7))
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
                                   (org-flag-proprty-drawer-at-marker marker t))
                                 (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and org-context-clock-select-propetry
                                   (abort-recursive-edit)))))

               (set-marker marker (point))
               ;; (message "1 marker %s" marker)

               (lotus-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
                   timeout timer cleanup local-cleanup win

                   (let ((target-buffer (find-file-noselect file)))

                     (when target-buffer
                       (switch-to-buffer target-buffer)
                       (goto-char pos)
                       (set-marker marker (point)))
                     ;; (message "2 marker %s" marker)

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
                                       (member
                                        (setq prop (org-context-clock-select-propetry context))
                                        '(edit done)))
                                 (when (org-context-clock-set-property prop nil context)
                                   (org-context-clock-task-update-tasks t)))
                               (cond
                                ((eql 'done prop)
                                 (funcall cleanup win local-cleanup)
                                 (when timer (cancel-timer timer)))
                                ((eql 'edit prop)
                                 ;; (funcall cleanup win local-cleanup)
                                 (message "debug editing")
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
  "Return value is important to decide next action to (create unnamed task.)"
  (org-context-clock-message 6 "called add-context-to-org-heading-when-idle")
  ;; timed-newwin of org-context-clock-add-context-to-org-heading pass quit
  ;; signal to caller mean here, so need to be handled, else this function can
  ;; not return any value to its caller, which result into no next-action in
  ;; caller function.
  (condition-case nil
      (org-context-clock-add-context-to-org-heading context timeout)
      ((quit)))
  ;; (run-with-idle-timer-nonobtrusive-simple
  ;;  7 nil
  ;;  #'(lambda (args)
  ;;      (apply 'org-context-clock-add-context-to-org-heading args)) (list context timeout))
  )

;;;###autoload
(defun org-context-clock-helm-select-dyntaskpl (selector action)
  ;; here
  ;; (org-context-clock-debug :debug "sacha marker %s" (car dyntaskpls))
  (let (helm-sources
        (context (org-context-clock-build-context)))

    (let ((dyntaskpls
           (org-context-clock-dyntaskpls-associated-to-context context)))
     (push
      (helm-build-sync-source "Select matching task"
        :candidates (mapcar
                     'sacha-org-context-clock-dyntaskpl-selection-line
                     dyntaskpls)
        :action (list
                 (cons "Clock in and track" selector))
        :history 'org-refile-history)
      helm-sources))

    (when (and
           (org-clocking-p)
           (marker-buffer org-clock-marker))
      (push
       (helm-build-sync-source "Current Clocking Task"
         :candidates (list (sacha-org-context-clock-dyntaskpl-selection-line
                            (org-context-clock-build-dyntaskpl
                             (org-context-clock-task-current-task)
                             context)))
         :action (list
                  (cons "Clock in and track" selector)))
       helm-sources))

    (funcall action (helm helm-sources))))

;;;###autoload
(defun org-context-clock-set-to-marker (marker)
  (if (and
       (markerp marker)
       (marker-buffer marker))
      (progn
        (set-buffer (marker-buffer marker))
        (goto-char marker))
      (error "marker %s invalid." marker)))

;;;###autoload
(defun org-context-clock-set-to-task ()
  (org-context-clock-helm-select-dyntaskpl
   #'org-context-clock-dyntaskpl-get-marker
   #'org-context-clock-set-to-marker))

;;;###autoload
(defun org-context-clock-create-child-task ()
  (interactive)
  (org-capture-alt
   'entry
   '(function org-context-clock-set-to-task)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1))

;;;###autoload
(defun org-context-clock-create-child-task ()
  (interactive)
  (org-capture-immediate                ;TODO
   'entry
   '(function org-context-clock-set-to-task)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1))

;; (org-context-clock-helm-select-dyntaskpl #'org-context-clock-dyntaskpl-get-marker)

(provide 'org-context-clock-api-interaction)
;;; org-context-clock-api-interaction.el ends here
