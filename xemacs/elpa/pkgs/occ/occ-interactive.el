;;; occ-interactive.el --- occ-api               -*- lexical-binding: t; -*-
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

(defun org-get-property (prop-key)
  (org-entry-get nil prop-key))

(defun occ-get-property (prop-key)
  (org-get-property prop-key))

(defun occ-set-property (prop-key value context &rest args)
  (let ((prop-key-str (if (eq (elt prop-key 0 ) ?\:) (substring prop-key 1))))
    (org-set-property prop-key
                      (if value
                          value
                          (funcall
                           (occ-key-fun prop-key :getter)
                           prop-key nil context args))))
  t)

;; (eq (elt ":root" 0) ?\:)

;; (occ-select-propetry nil)

;; (occ-keys-with-operation :getter nil)

;; (occ-set-property (intern ":root") nil (list :file "/home/s/paradise/git/main/src/wnc/security/authenticator/ieee802_1x.cpp" :buffer (get-buffer "ieee802_1x.cpp")))

(defun occ-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  (let ((helm-always-two-windows nil))
    (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))

(defun occ-select-propetry (context &optional prompt)
  (let ((prompt (or prompt "proptery: "))
        (keys (mapcar #'(lambda (k) (cons (symbol-name k) k))
                      (append
                       (occ-keys-with-operation :getter context)
                       '(edit done)))))
    (cdr (assoc (occ-completing-read prompt keys  nil t) keys))))

(defun occ-test (context timeout)
  (interactive '(nil nil))
  (lexical-let* ((timeout (or timeout 7))
                 (context (or context (occ-build-context)))
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
(cl-defmethod occ-add-to-org-heading ((context occ-context) timeout)
  "add-context-to-org-heading"

  ;; TODO: make helm conditional when it is used than only it should be handled.

  (interactive '(nil 7))

  (lotus-with-no-active-minibuffer
      (progn
        (message "add-context-to-org-heading: minibuffer already active quitting")
        (message nil))
    (lexical-let* ((timeout (or timeout 7))
                   (context (or context (occ-build-context)))
                   (buff (plist-get context :buffer)))
      (if (and
           (eq (current-buffer) buff)
           (buffer-live-p buff)
           (not
            (eq buff
                (get-buffer "*helm-mode-occ-add-to-org-heading*"))))

          (org-with-file-loc-timed-refile
              file pos
              timeout '((occ-task-update-files :maxlevel . 4))

              (lexical-let* ((marker (make-marker))
                             (local-cleanup
                              #'(lambda ()
                                  (save-excursion ;what to do here
                                    (org-flag-proprty-drawer-at-marker marker t))
                                  (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and occ-select-propetry
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
                                         (setq prop (occ-select-propetry context))
                                         '(edit done)))
                                  (when (occ-set-property prop nil context)
                                    (occ-task-update-tasks t)))
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
            (occ-message 6 "not running add-context-to-org-heading 1 %s, 2 %s 3 %s"
                                       (eq (current-buffer) buff)
                                       (buffer-live-p buff)
                                       (eq buff
                                           (get-buffer "*helm-mode-occ-add-to-org-heading*"))))))))

;;;###autoload
(cl-defmethod occ-add-to-org-heading-when-idle ((context occ-context) timeout)
  "Return value is important to decide next action to (create unnamed task.)"
  (occ-message 6 "called add-context-to-org-heading-when-idle")
  ;; timed-newwin of occ-add-to-org-heading pass quit
  ;; signal to caller mean here, so need to be handled, else this function can
  ;; not return any value to its caller, which result into no next-action in
  ;; caller function.
  (condition-case nil
      (occ-add-to-org-heading context timeout)
      ((quit)))
  ;; (run-with-idle-timer-nonobtrusive-simple
  ;;  7 nil
  ;;  #'(lambda (args)
  ;;      (apply 'occ-add-to-org-heading args)) (list context timeout))
  )

;;;###autoload
(defun occ-helm-select-contextask (selector
                                   action)
  ;; here
  ;; (occ-debug :debug "sacha marker %s" (car contextasks))
  (let (helm-sources
        (context (occ-make-context)))

    (let ((contextasks
           (occ-matching-contextual-tasks context)))
     (push
      (helm-build-sync-source "Select matching task"
        :candidates (mapcar
                     'occ-sacha-selection-line
                     contextasks)
        :action (list
                 (cons "Clock in and track" selector))
        :history 'org-refile-history)
      helm-sources))

    (when (and
           (org-clocking-p)
           (marker-buffer org-clock-marker))
      (push
       (helm-build-sync-source "Current Clocking Task"
         :candidates (list (occ-sacha-selection-line
                            (occ-make-contextual-task context (occ-current-task) 0)))
         :action (list
                  (cons "Clock in and track" selector)))
       helm-sources))

    (funcall action (helm helm-sources))))

;;;###autoload
(defun occ-set-to-marker (marker)
  (if (and
       (markerp marker)
       (marker-buffer marker))
      (progn
        (set-buffer (marker-buffer marker))
        (goto-char marker))
      (error "marker %s invalid." marker)))

;;;###autoload
(defun occ-set-to-task ()
  (occ-helm-select-dyntaskpl
   #'occ-dyntaskpl-get-marker
   #'occ-set-to-marker))

;;;###autoload
(defun occ-create-child-task ()
  (interactive)
  (org-capture-alt
   'entry
   '(function occ-set-to-task)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1))

;;;###autoload
(defun occ-create-child-task ()
  (interactive)
  (org-capture-immediate                ;TODO
   'entry
   '(function occ-set-to-task)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1))

(provide 'occ-interactive)
;;; occ-interactive.el ends here
