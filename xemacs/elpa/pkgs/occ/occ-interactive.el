;;; occ-interactive.el --- occ-api               -*- lexical-binding: t; -*-
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

(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))


(require 'occ-obj-method)


(provide 'occ-interactive)


;; (defun org-get-property (prop-key)
;;   (org-entry-get nil prop-key))

;; (defun occ-get-property (prop-key)
;;   (org-get-property prop-key))

;; (defun occ-set-property (prop-key value ctx &rest args)
;;   (let ((prop-key-str (if (eq (elt prop-key 0 ) ?\:) (substring prop-key 1))))
;;     (org-set-property prop-key
;;                       (if value
;;                           value
;;                           (funcall
;;                            (occ-key-fun prop-key :getter)
;;                            prop-key nil ctx args))))
;;   t)

;; (eq (elt ":root" 0) ?\:)

;; (occ-select-propetry nil)

;; (occ-keys-with-operation :getter nil)

;; (occ-set-property (intern ":root") nil (list :file "/home/s/paradise/git/main/src/wnc/security/authenticator/ieee802_1x.cpp" :buffer (get-buffer "ieee802_1x.cpp")))

(defun occ-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  (let ((helm-always-two-windows nil))
    (completing-read prompt
                     collection
                     predicate
                     require-match
                     initial-input
                     hist
                     def
                     inherit-input-method)))

(defun occ-select-propetry (ctx &optional prompt)
  (let ((prompt (or prompt "proptery: "))
        (keys (mapcar #'(lambda (k) (cons (symbol-name k) k))
                      (append
                       ;; (cl-method-first-arg 'occ-readprop)
                       (cl-method-matched-arg 'occ-readprop ctx)
                       '(edit done)))))
    (cdr (assoc (occ-completing-read prompt keys  nil t) keys))))

(defun org-flag-proprty-drawer-at-marker (marker flag)
  (let ((buff (marker-buffer marker))
        (loc (marker-position marker)))
    (when (and buff loc)
      (with-current-buffer buff
        (goto-char loc)
        (let ((range (org-get-property-block (point) 'force)))
          ;; first show hreading
          (when (eq org-cycle-subtree-status 'folded)
            (unless flag (org-show-entry)) ; changed from org-show-tsk to org-show-entry
            (org-unlogged-message "CHILDREN")
            (setq org-cycle-subtree-status 'children))
          ;; show expand property if flag is nil, else hide
          (when range
            (goto-char (1- (car range)))
            (occ-debug :debug "reached to drawer")
            (if (org-at-drawer-p)
                ;; show drawer
                (let ((drawer (org-element-at-point)))
                  (when (memq (org-element-type drawer) '(node-property drawer property-drawer))
                    (occ-debug :debug "trying to open drawer %s" drawer)
                    (org-flag-drawer flag drawer)
                    ;; Make sure to skip drawer entirely or we might flag
                    ;; it another time when matching its ending line with
                    ;; `org-drawer-regexp'.
                    (goto-char (org-element-property :end drawer))))
              (occ-debug :debug "not at drawer"))
            (occ-debug :debug "reached to drawer1")))))))

(defun org-get-flag-proprty-drawer-at-marker (marker)
  (let ((buff (marker-buffer marker))
        (loc (marker-position marker)))
    (when (and buff loc)
      (with-current-buffer buff
        (when (goto-char loc)
          (let ((range (org-get-property-block (point) 'force)))
            org-cycle-subtree-status))))))

;;---------------------------------------------------------------;;;###autoload
(cl-defmethod occ-add-to-org-heading ((ctx occ-ctx) timeout)
  "add-ctx-to-org-heading"
  ;; TODO: make helm conditional when it is used than only it should be handled.
  (interactive '((occ-make-ctx) 7))

  (lotus-with-no-active-minibuffer-if
      (progn
        (lwarn 'occ :debug "occ-add-to-org-heading: [minibuffer-body] lotus-with-no-active-minibuffer-if")
        (occ-debug :debug "add-ctx-to-org-heading: minibuffer already active quitting")
        (occ-debug :debug nil))
    (let* ((timeout (or timeout 7))
           (ctx (or ctx (occ-make-ctx)))
           (buff (occ-ctx-buffer ctx)))
      (lwarn 'occ :debug "occ-add-to-org-heading: [body] lotus-with-no-active-minibuffer-if")
      (if (and
           (eq (current-buffer) buff)
           (buffer-live-p buff)
           (not
            (eq buff
                (get-buffer "*helm-mode-occ-add-to-org-heading*"))))

          (org-with-file-loc-timed-refile
              file pos
              timeout '((occ-included-files :maxlevel . 4)) ;heavy task, but present in macro !

            (let* ((marker (make-marker))
                   (local-cleanup
                    #'(lambda ()
                        (save-excursion ;what to do here
                          (org-flag-proprty-drawer-at-marker marker t))
                        (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and occ-select-propetry
                          (abort-recursive-edit)))))

              (set-marker marker (point))
              ;; (occ-debug :debug "1 marker %s" marker)

              (lotus-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
                  timeout timer cleanup local-cleanup win

                  (let ((target-buffer (find-file-noselect file)))

                    (when target-buffer
                      (switch-to-buffer target-buffer)
                      (goto-char pos)
                      (set-marker marker (point)))
                    ;; (occ-debug :debug "2 marker %s" marker)

                    (occ-debug :debug "called add-ctx-to-org-heading %s" (current-buffer))
                    (progn
                      (condition-case err
                          (let ((buffer-read-only nil))
                            (occ-debug :debug "timer started for win %s" win)

                            ;; show proptery drawer
                            (org-flag-proprty-drawer-at-marker marker nil)

                            ;; try to read values of properties.
                            (let ((prop nil))
                              (while (not
                                      (member
                                       (setq prop (occ-select-propetry ctx))
                                       '(edit done)))
                                (when (occ-set-property prop nil ctx)
                                  (occ-tsk-update-tsks t)))
                              (cond
                               ((eql 'done prop)
                                (funcall cleanup win local-cleanup)
                                (when timer (cancel-timer timer)))
                               ((eql 'edit prop)
                                ;; (funcall cleanup win local-cleanup)
                                (occ-debug :debug "debug editing")
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
          (occ-debug 6 "not running add-ctx-to-org-heading 1 %s, 2 %s 3 %s"
                     (eq (current-buffer) buff)
                     (buffer-live-p buff)
                     (eq buff
                         (get-buffer "*helm-mode-occ-add-to-org-heading*"))))))))

;;---------------------------------------------------------------;;;###autoload
(cl-defmethod occ-add-to-org-heading-when-idle ((ctx occ-ctx) timeout)

  ;; either this should also be in occ-obj-method
  ;; or (cl-defmethod occ-clock-in ((ctx occ-ctx))
  ;;    (defun occ-sacha-helm-select (ctxasks)
  ;; should be here.

  ;; NOTE: presently it is not running on idle time, it simply runs immediately

  "Return value is important to decide next action to (create unnamed tsk.)"
  (occ-debug 6 "called occ-add-to-org-heading-when-idle")
  (occ-debug :debug "begin: occ-add-to-org-heading-when-idle")
  ;; timed-newwin of occ-add-to-org-heading pass quit
  ;; signal to caller mean here, so need to be handled, else this function can
  ;; not return any value to its caller, which result into no next-action in
  ;; caller function.
  (condition-case nil
      ;; TODO: Add code to which check if only focus present than only trigger
      ;; else postpone it by calling run-with-idle-plus-timer
      (lotus-with-other-frame-event-debug "occ-add-to-org-heading-when-idle" :cancel
        (lwarn 'occ :debug "occ-add-to-org-heading-when-idle: lotus-with-other-frame-event-debug")
        (occ-add-to-org-heading ctx timeout))
    ((quit)))
  (occ-debug :debug "end: occ-add-to-org-heading-when-idle")
  ;; (run-with-idle-timer-nonobtrusive-simple
  ;;  7 nil
  ;;  #'(lambda (args)
  ;;      (apply 'occ-add-to-org-heading args)) (list ctx timeout))
  )

;;;###autoload
(defun occ-helm-select-tsk (selector
                            action)
  ;; here
  ;; (occ-debug :debug "sacha marker %s" (car tsks))
  (let ()
    (let ((tsks
           (occ-collect-tsk-list (occ-collection-object))))
      (push
       (helm-build-sync-source "Select tsk"
         :candidates (mapcar
                      'occ-sacha-selection-line
                      tsks)
         :action (list
                  (cons "Clock in and track" selector))
         :history 'org-refile-history)
       helm-sources))

    (when (and
           (org-clocking-p)
           (marker-buffer org-clock-marker))
      (push
       (helm-build-sync-source "Current Clocking Tsk"
         :candidates (list (occ-sacha-selection-line (occ-current-tsk)))
         :action (list
                  (cons "Clock in and track" selector)))
       helm-sources))

    (funcall action (helm helm-sources))))

;;;###autoload
(defun occ-helm-select-ctxual-tsk (selector
                                   action)
  ;; here
  ;; (occ-debug :debug "sacha marker %s" (car ctxasks))
  (let (helm-sources
        (ctx (occ-make-ctx)))
    (let ((ctxasks
           (occ-matching-ctxual-tsks (occ-collection-object) ctx)))
      (push
       (helm-build-sync-source "Select matching tsk"
         :candidates (mapcar
                      'occ-sacha-selection-line
                      ctxasks)
         :action (list
                  (cons "Clock in and track" selector))
         :history 'org-refile-history)
       helm-sources))

    (when (and
           (org-clocking-p)
           (marker-buffer org-clock-marker))
      (push
       (helm-build-sync-source "Current Clocking Tsk"
         :candidates (list (occ-sacha-selection-line
                            (occ-build-ctxual-tsk (occ-current-tsk) ctx)))
         :action (list
                  (cons "Clock in and track" selector)))
       helm-sources))

    (funcall action (helm helm-sources))))



;;;###autoload
(defun occ-set-to-ctxual-tsk ()
  (occ-helm-select-ctxual-tsk
   #'occ-ctxual-tsk-marker
   #'occ-set-to-marker))

(defun occ-goto-tsk ()
  (occ-helm-select-tsk
   #'occ-tsk-marker
   #'occ-goto-marker))


;;;###autoload
(defun occ-create-child-tsk ()
  (interactive)
  (org-capture-alt
   'entry
   '(function occ-set-to-ctxual-tsk)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1))

;;;###autoload
(defun occ-create-child-tsk ()
  (interactive)
  (org-capture-immediate                ;TODO
   'entry
   '(function occ-set-to-ctxual-tsk)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1))

(defun occ-goto-test ()
  (interactive)
  (occ-goto-tsk))

;; testing verification
(defun occ-files-with-null-regex ()
  (interactive)
  (let ((files
          (remove-if
            #'(lambda (f)
                (with-current-buffer (find-file-noselect f)
                  org-complex-heading-regexp))
            (occ-included-files))))
    (message "files with null regex %s" files)))

;; testing verification;; testing verification
(defun occ-files-not-in-org-mode ()
  (interactive)
  (let ((files
         (remove-if
          #'(lambda (f)
              (with-current-buffer (find-file-noselect f)
                (eq major-mode 'org-mode)))
          (occ-included-files))))
    (message "files not in org-mode %s" files)))



;; TODO
;; check about org-clock-save-markers-for-cut-and-paste
;;



;;; occ-interactive.el ends here
