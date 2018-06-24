;;; occ-ctor.el --- occ-api               -*- lexical-binding: t; -*-
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

(require 'occ-common)
(require 'occ-base-objects)
(require 'occ-object-methods)

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
(cl-defmethod occ-add-to-org-heading ((ctx occ-ctx) timeout)
  "add-ctx-to-org-heading"
  ;; TODO: make helm conditional when it is used than only it should be handled.
  (interactive '((occ-make-ctx) 7))

  (lotus-with-no-active-minibuffer
      (progn
        (message "add-ctx-to-org-heading: minibuffer already active quitting")
        (message nil))
    (lexical-let* ((timeout (or timeout 7))
                   (ctx (or ctx (occ-make-ctx)))
                   (buff (occ-ctx-buffer ctx)))
      (if (and
           (eq (current-buffer) buff)
           (buffer-live-p buff)
           (not
            (eq buff
                (get-buffer "*helm-mode-occ-add-to-org-heading*"))))

          (org-with-file-loc-timed-refile
              file pos
              timeout '((occ-included-files :maxlevel . 4))

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

                      (message "called add-ctx-to-org-heading %s" (current-buffer))
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
          (occ-debug 6 "not running add-ctx-to-org-heading 1 %s, 2 %s 3 %s"
                       (eq (current-buffer) buff)
                       (buffer-live-p buff)
                       (eq buff
                           (get-buffer "*helm-mode-occ-add-to-org-heading*"))))))))

;;;###autoload
(cl-defmethod occ-add-to-org-heading-when-idle ((ctx occ-ctx) timeout)
  "Return value is important to decide next action to (create unnamed tsk.)"
  (occ-debug 6 "called add-ctx-to-org-heading-when-idle")
  ;; timed-newwin of occ-add-to-org-heading pass quit
  ;; signal to caller mean here, so need to be handled, else this function can
  ;; not return any value to its caller, which result into no next-action in
  ;; caller function.
  (condition-case nil
      (occ-add-to-org-heading ctx timeout)
    ((quit)))
  ;; (run-with-idle-timer-nonobtrusive-simple
  ;;  7 nil
  ;;  #'(lambda (args)
  ;;      (apply 'occ-add-to-org-heading args)) (list ctx timeout))
  )

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
                            (occ-build-ctxual-tsk ctx (occ-current-tsk))))
         :action (list
                  (cons "Clock in and track" selector)))
       helm-sources))

    (funcall action (helm helm-sources))))

;;;###autoload
(defun occ-goto-marker (marker)
  (if (and
       (markerp marker)
       (marker-buffer marker))
      (progn
        (set-buffer (marker-buffer marker))
        (goto-char marker))
    (error "marker %s invalid." marker)))

;;;###autoload
(defun occ-set-to-tsk ()
  (occ-helm-select-ctxual-tsk
   #'occ-ctxual-tsk-marker
   #'occ-goto-marker))

;;;###autoload
(defun occ-create-child-tsk ()
  (interactive)
  (org-capture-alt
   'entry
   '(function occ-set-to-tsk)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1))

;;;###autoload
(defun occ-create-child-tsk ()
  (interactive)
  (org-capture-immediate                ;TODO
   'entry
   '(function occ-set-to-tsk)
   "* TODO %? %^g\n %i\n [%a]\n"
   :empty-lines 1))

(provide 'occ-interactive)
;;; occ-interactive.el ends here

(require 'occ-tree)

(defun occ-heading-content-only ()
  (if (org-at-heading-p)
      (save-excursion
        (save-restriction
          (let ((start (progn
                         (goto-char (org-element-property :contents-begin (org-element-at-point)))
                         (while (org-at-drawer-p)
                           (goto-char (org-element-property :end (org-element-at-point))))
                         ;; (if (org-at-heading-p) (backward-char))
                         (point))))
            (unless (org-at-heading-p)
              (progn
                (outline-next-heading)
                ;; (outline-next-visible-heading 1)
                (backward-char)
                (buffer-substring start (point)))))))))

(defun occ-make-tsk-at-point (builder)
  ;; (org-element-at-point)
  (let (tsk
        (heading-with-string-prop
         (if (org-before-first-heading-p)
             "empty heading"
           (org-get-heading 'notags))))
    (let ((heading (when heading-with-string-prop
                     (substring-no-properties heading-with-string-prop)))
          (heading-prop (if heading-with-string-prop
                            heading-with-string-prop))
          (marker  (move-marker
                    (make-marker)
                    (point)
                    (org-base-buffer (current-buffer))))
          (file    (buffer-file-name))
          (point   (point))
          (clock-sum (if (org-before-first-heading-p)
                         0
                       (org-clock-sum-current-item)))
          (tsk-plist (cadr (org-element-at-point))))
      (when heading
        (setf tsk
              (funcall builder
                       :name    heading
                       :heading heading
                       :heading-prop heading-prop
                       :marker  marker
                       :file file
                       :point point
                       :clock-sum clock-sum
                       :plist tsk-plist))

        (let ((inherited-props (cl-method-first-arg 'occ-readprop)))
          (dolist (prop inherited-props)
            (let* ((propstr (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))
                   (val (org-entry-get nil propstr t)))
              (unless (occ-get-property tsk prop)
                (occ-set-property tsk prop val))))))
      tsk)))

(cl-defmethod occ-make-tsk ((n number)
                            builder)
  (occ-debug :debug "point %s" n)
  (if (<= n (point-max))
      (save-restriction
        (save-excursion
          (goto-char n)
          (occ-make-tsk-at-point builder)))))

(cl-defmethod occ-make-tsk ((m marker)
                            builder)
  (occ-debug :debug "point %s" m)
  (if (and
       (marker-buffer m)
       (numberp (marker-position m)))
      (with-current-buffer (marker-buffer m)
        (if (<= (marker-position m) (point-max))
            (occ-make-tsk (marker-position m) builder)))))

(defun occ-make-ctx (&optional buff)
  (let* ((buff (if buff
                   (if (bufferp buff)
                       buff
                     (if (stringp buff)
                         (or
                          (get-buffer buff)
                          (if (file-exists-p buff)
                              (get-file-buffer buff)))))
                 (window-buffer)))
         (buf (org-base-buffer buf))
         (file (buffer-file-name buff))
         (ctx (make-occ-ctx
               :name (buffer-name buff)
               :file file
               :buffer buff)))
    ctx))

(cl-defmethod occ-make-ctxual-tsk ((tsk occ-tsk)
                                   (ctx occ-ctx)
                                   (rank number))
  ;; use occ-build-ctxual-tsk
  (make-occ-ctxual-tsk
   :name    nil
   :tsk    tsk
   :ctx ctx
   :rank    rank))

(defvar occ-global-tsk-collection-spec nil)
(defvar occ-global-tsk-collection nil)

(cl-defmethod occ-make-tsk-collection ((file-spec (head :tree)))
  (unless occ-global-tsk-collection
    (let ((collection (make-occ-tree-tsk-collection
                       :name "tsk collection tree"
                       :root-files (cdr file-spec))))
      (setf occ-global-tsk-collection collection))))

(cl-defmethod occ-make-tsk-collection ((file-spec (head :list)))
  (unless occ-global-tsk-collection
    (let ((collection (make-occ-list-tsk-collection
                       :name "tsk collection list"
                       :root-files (cdr dir-spec))))
      (setf occ-global-tsk-collection collection))))

(cl-defmethod occ-collect-tsks (collection
                                force)
  (error "first argument should be of type (or occ-tree-tsk-collection occ-list-tsk-collection)"))

(cl-defmethod occ-collect-tsks ((collection occ-tree-tsk-collection)
                                force)
  (unless (occ-tree-tsk-collection-tree collection)
    (setf
     (occ-tree-tsk-collection-tree collection)
     (occ-tree-tsk-build
      #'(lambda ()
          (or
           (occ-make-tsk-at-point #'make-occ-tree-tsk)
           (make-occ-tree-tsk :name "empty tree tsk"))) ;; note: only using first file of root-files
      (car (occ-tree-tsk-collection-root-files collection))))))

(cl-defmethod occ-collect-included-files ((collection occ-tree-tsk-collection)
                                          force)
  (unless (occ-tree-tsk-collection-included-files collection)
    (occ-collect-tsks collection nil)
    (setf
     (occ-tree-tsk-collection-included-files collection)
     (remove nil
             (delete-dups
              (let ((tsks (occ-collection collection))
                    (files '()))
                (occ-mapc-tree-tsks
                 #'(lambda (tsk args)
                     (push (occ-tsk-file tsk) files))
                 tsks
                 nil)
                files))))))

(cl-defmethod occ-collect-tsks ((collection occ-list-tsk-collection)
                                force)
  (unless (occ-list-tsk-collection-list collection)
    (setf
     (occ-list-tsk-collection-list collection)
     (remove nil
             (org-map-entries
              #'(lambda ()
                  (or
                   (occ-make-tsk-at-point #'make-occ-list-tsk)
                   (make-occ-list-tsk :name "empty list tsk")))
              t
              (occ-list-tsk-collection-root-files collection))))))

(cl-defmethod occ-collect-included-files ((collection occ-list-tsk-collection)
                                          force)
  (unless (occ-list-tsk-collection-included-files collection)
    (setf
     (occ-list-tsk-collection-included-files collection)
     (occ-list-tsk-collection-root-files collection))))

(cl-defmethod occ-collection ((collection occ-tree-tsk-collection))
  (unless (occ-tree-tsk-collection-tree occ-global-tsk-collection)
    (occ-collect-tsks occ-global-tsk-collection nil))
  (occ-tree-tsk-collection-tree occ-global-tsk-collection))

(cl-defmethod occ-collection ((collection occ-list-tsk-collection))
  (unless (occ-list-tsk-collection-list occ-global-tsk-collection)
    (occ-collect-tsks occ-global-tsk-collection nil))
  (occ-list-tsk-collection-list occ-global-tsk-collection))

(cl-defmethod occ-collection-included-files ((collection occ-tree-tsk-collection))
  (unless (occ-tree-tsk-collection-included-files occ-global-tsk-collection)
    (occ-collect-included-files occ-global-tsk-collection nil))
  (occ-tree-tsk-collection-included-files occ-global-tsk-collection))
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

(require 'occ-object-methods)

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

;; (defun occ-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
;;   (let ((helm-always-two-windows nil))
;;     (completing-read prompt
;;                      collection
;;                      predicate
;;                      require-match
;;                      initial-input
;;                      hist
;;                      def
;;                      inherit-input-method)))

;; (defun occ-select-propetry (ctx &optional prompt)
;;   (let ((prompt (or prompt "proptery: "))
;;         (keys (mapcar #'(lambda (k) (cons (symbol-name k) k))
;;                       (append
;;                        ;; (cl-method-first-arg 'occ-readprop)
;;                        (cl-method-matched-arg 'occ-readprop ctx)
;;                        '(edit done)))))
;;     (cdr (assoc (occ-completing-read prompt keys  nil t) keys))))

;; (defun org-flag-proprty-drawer-at-marker (marker flag)
;;   (let ((buff (marker-buffer marker))
;;         (loc (marker-position marker)))
;;     (when (and buff loc)
;;       (with-current-buffer buff
;;         (goto-char loc)
;;         (let ((range (org-get-property-block (point) 'force)))
;;           ;; first show hreading
;;           (when (eq org-cycle-subtree-status 'folded)
;;             (unless flag (org-show-entry)) ; changed from org-show-tsk to org-show-entry
;;             (org-unlogged-message "CHILDREN")
;;             (setq org-cycle-subtree-status 'children))
;;           ;; show expand property if flag is nil, else hide
;;           (when range
;;             (goto-char (1- (car range)))
;;             (message "reached to drawer")
;;             (if (org-at-drawer-p)
;;                 ;; show drawer
;;                 (let ((drawer (org-element-at-point)))
;;                   (when (memq (org-element-type drawer) '(node-property drawer property-drawer))
;;                     (message "trying to open drawer %s" drawer)
;;                     (org-flag-drawer flag drawer)
;;                     ;; Make sure to skip drawer entirely or we might flag
;;                     ;; it another time when matching its ending line with
;;                     ;; `org-drawer-regexp'.
;;                     (goto-char (org-element-property :end drawer))))
;;               (message "not at drawer"))
;;             (message "reached to drawer1")))))))

;; (defun org-get-flag-proprty-drawer-at-marker (marker)
;;   (let ((buff (marker-buffer marker))
;;         (loc (marker-position marker)))
;;     (when (and buff loc)
;;       (with-current-buffer buff
;;         (when (goto-char loc)
;;           (let ((range (org-get-property-block (point) 'force)))
;;             org-cycle-subtree-status))))))

;; ;;;###autoload
;; (cl-defmethod occ-add-to-org-heading ((ctx occ-ctx) timeout)
;;   "add-ctx-to-org-heading"
;;   ;; TODO: make helm conditional when it is used than only it should be handled.
;;   (interactive '((occ-make-ctx) 7))

;;   (lotus-with-no-active-minibuffer
;;       (progn
;;         (message "add-ctx-to-org-heading: minibuffer already active quitting")
;;         (message nil))
;;     (lexical-let* ((timeout (or timeout 7))
;;                    (ctx (or ctx (occ-make-ctx)))
;;                    (buff (occ-ctx-buffer ctx)))
;;       (if (and
;;            (eq (current-buffer) buff)
;;            (buffer-live-p buff)
;;            (not
;;             (eq buff
;;                 (get-buffer "*helm-mode-occ-add-to-org-heading*"))))

;;           (org-with-file-loc-timed-refile
;;               file pos
;;               timeout '((occ-included-files :maxlevel . 4))

;;               (lexical-let* ((marker (make-marker))
;;                              (local-cleanup
;;                               #'(lambda ()
;;                                   (save-excursion ;what to do here
;;                                     (org-flag-proprty-drawer-at-marker marker t))
;;                                   (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and occ-select-propetry
;;                                     (abort-recursive-edit)))))

;;                 (set-marker marker (point))
;;                 ;; (message "1 marker %s" marker)

;;                 (lotus-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
;;                     timeout timer cleanup local-cleanup win

;;                     (let ((target-buffer (find-file-noselect file)))

;;                       (when target-buffer
;;                         (switch-to-buffer target-buffer)
;;                         (goto-char pos)
;;                         (set-marker marker (point)))
;;                       ;; (message "2 marker %s" marker)

;;                       (message "called add-ctx-to-org-heading %s" (current-buffer))
;;                       (progn
;;                         (condition-case err
;;                             (let ((buffer-read-only nil))
;;                               (message "timer started for win %s" win)

;;                               ;; show proptery drawer
;;                               (org-flag-proprty-drawer-at-marker marker nil)

;;                               ;; try to read values of properties.
;;                               (let ((prop nil))
;;                                 (while (not
;;                                         (member
;;                                          (setq prop (occ-select-propetry ctx))
;;                                          '(edit done)))
;;                                   (when (occ-set-property prop nil ctx)
;;                                     (occ-tsk-update-tsks t)))
;;                                 (cond
;;                                   ((eql 'done prop)
;;                                    (funcall cleanup win local-cleanup)
;;                                    (when timer (cancel-timer timer)))
;;                                   ((eql 'edit prop)
;;                                    ;; (funcall cleanup win local-cleanup)
;;                                    (message "debug editing")
;;                                    (when timer (cancel-timer timer))
;;                                    (when (and win (windowp win) (window-valid-p win))
;;                                      (select-window win 'norecord)))
;;                                   (t
;;                                    (funcall cleanup win local-cleanup)
;;                                    (when timer (cancel-timer timer))))))
;;                           ((quit)
;;                            (progn
;;                              (funcall cleanup win local-cleanup)
;;                              (if timer (cancel-timer timer))
;;                              (signal (car err) (cdr err))))))))))
;;         (progn
;;           (occ-debug 6 "not running add-ctx-to-org-heading 1 %s, 2 %s 3 %s"
;;                        (eq (current-buffer) buff)
;;                        (buffer-live-p buff)
;;                        (eq buff
;;                            (get-buffer "*helm-mode-occ-add-to-org-heading*"))))))))

;; ;;;###autoload
;; (cl-defmethod occ-add-to-org-heading-when-idle ((ctx occ-ctx) timeout)
;;   "Return value is important to decide next action to (create unnamed tsk.)"
;;   (occ-debug 6 "called add-ctx-to-org-heading-when-idle")
;;   ;; timed-newwin of occ-add-to-org-heading pass quit
;;   ;; signal to caller mean here, so need to be handled, else this function can
;;   ;; not return any value to its caller, which result into no next-action in
;;   ;; caller function.
;;   (condition-case nil
;;       (occ-add-to-org-heading ctx timeout)
;;     ((quit)))
;;   ;; (run-with-idle-timer-nonobtrusive-simple
;;   ;;  7 nil
;;   ;;  #'(lambda (args)
;;   ;;      (apply 'occ-add-to-org-heading args)) (list ctx timeout))
;;   )

;; ;;;###autoload
;; (defun occ-helm-select-ctxual-tsk (selector
;;                                    action)
;;   ;; here
;;   ;; (occ-debug :debug "sacha marker %s" (car ctxasks))
;;   (let (helm-sources
;;         (ctx (occ-make-ctx)))

;;     (let ((ctxasks
;;            (occ-matching-ctxual-tsks (occ-collection-object) ctx)))
;;       (push
;;        (helm-build-sync-source "Select matching tsk"
;;          :candidates (mapcar
;;                       'occ-sacha-selection-line
;;                       ctxasks)
;;          :action (list
;;                   (cons "Clock in and track" selector))
;;          :history 'org-refile-history)
;;        helm-sources))

;;     (when (and
;;            (org-clocking-p)
;;            (marker-buffer org-clock-marker))
;;       (push
;;        (helm-build-sync-source "Current Clocking Tsk"
;;          :candidates (list (occ-sacha-selection-line
;;                             (occ-build-ctxual-tsk ctx (occ-current-tsk))))
;;          :action (list
;;                   (cons "Clock in and track" selector)))
;;        helm-sources))

;;     (funcall action (helm helm-sources))))

;; ;;;###autoload
;; (defun occ-goto-marker (marker)
;;   (if (and
;;        (markerp marker)
;;        (marker-buffer marker))
;;       (progn
;;         (set-buffer (marker-buffer marker))
;;         (goto-char marker))
;;     (error "marker %s invalid." marker)))

;; ;;;###autoload
;; (defun occ-set-to-tsk ()
;;   (occ-helm-select-ctxual-tsk
;;    #'occ-ctxual-tsk-marker
;;    #'occ-goto-marker))

;; ;;;###autoload
;; (defun occ-create-child-tsk ()
;;   (interactive)
;;   (org-capture-alt
;;    'entry
;;    '(function occ-set-to-tsk)
;;    "* TODO %? %^g\n %i\n [%a]\n"
;;    :empty-lines 1))

;; ;;;###autoload
;; (defun occ-create-child-tsk ()
;;   (interactive)
;;   (org-capture-immediate                ;TODO
;;    'entry
;;    '(function occ-set-to-tsk)
;;    "* TODO %? %^g\n %i\n [%a]\n"
;;    :empty-lines 1))

(provide 'occ-interactive)
;;; occ-interactive.el ends here

(cl-defmethod occ-collection-included-files ((collection occ-list-tsk-collection))
  (unless (occ-list-tsk-collection-included-files occ-global-tsk-collection)
    (occ-collect-included-files occ-global-tsk-collection nil))
  (occ-list-tsk-collection-included-files occ-global-tsk-collection))

(defun occ-collection-object ()
  (unless occ-global-tsk-collection
    (occ-make-tsk-collection occ-global-tsk-collection-spec)
    (occ-collect-tsks occ-global-tsk-collection t))
  occ-global-tsk-collection)

(when nil
  (progn
    (setq occ-global-tsk-collection nil)
    (occ-make-tsk-collection (list :tree org-ctx-clock-tsk-tree-tsk-root-org-file))
    (occ-tree-tsk-collection-tree occ-global-tsk-collection)
    (occ-collect-tsks occ-global-tsk-collection t)
    (occ-tree-tsk-collection-root-files occ-global-tsk-collection)
    (setf occ-gtree
          (occ-tree-tsk-collection-tree occ-global-tsk-collection)))

  (setf
   occ-test-gtree
   (occ-tsk-tree-build
    #'(lambda ()
        (or
         (occ-make-tsk-at-point #'make-occ-tree-tsk)
         (make-occ-tree-tsk :name "empty tree tsk"))) ;; note: only using first file of root-files
    "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tsks/xx.org"))

  (setq occ-test-gtree
        (occ-tsk-tree-build
         #'(lambda ()
             (or
              (occ-make-tsk-at-point #'make-occ-tree-tsk)
              (make-occ-tree-tsk :name "empty tree tsk"))) ;; note: only using first file of root-files
         org-ctx-clock-tsk-tree-tsk-root-org-file))

  (with-current-buffer (find-file-noselect "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tsks/xx.org")
    (goto-char (point-min))
    (setf occ-file-subtree
          (occ-tsk-tree-map-subheading
           #'(lambda ()
               (occ-tsk-tree-collect-tsk
                #'(lambda ()
                    (or
                     (occ-make-tsk-at-point #'make-occ-tree-tsk)
                     (make-occ-tree-tsk :name "empty tree tsk")))))))))

(provide 'occ-ctor)
;;; occ-ctor.el ends here
