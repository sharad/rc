;;; org-misc-utils-lotus.el --- copy config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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

;; ORG here is issue of naming heading vs headline

(eval-when-compile
  (require 'lotus-misc-utils))

(require 'lotus-misc-utils)

(eval-when-compile
  (require 'cl))

(require 'cl)

(defvar safe-org-refile-get-location-modes
  '(emacs-lisp-mode org-mode))

(setq
 safe-org-refile-get-location-modes '(org-mode))

;; Misc Macros Starts

(defvar lotus-inhibit-modification-hooks t "lotus-inhibit-modification-hooks")

(defmacro org-with-inhibit-modification-hooks (&rest body)
  `(let ((inhibit-modification-hooks lotus-inhibit-modification-hooks))
     ,@body))
(put 'org-with-inhibit-modification-hooks 'lisp-indent-function 0)

(defmacro org-with-clock-writeable (&rest body)
  `(let ((buff (org-base-buffer (marker-buffer org-clock-marker))))
     (when buff
       (with-current-buffer buff
         (let (buffer-read-only)
           ,@body)))))
(put 'org-with-clock-writeable 'lisp-indent-function 0)

(defmacro org-clock-lotus-with-current-clock (&rest body)
  `(when (marker-buffer org-clock-marker)
     (org-with-clock (cons org-clock-marker org-clock-start-time)
       ,@body)))
(put 'org-clock-lotus-with-current-clock 'lisp-indent-function 0)

(defmacro org-with-heading (heading &rest body)
  `(progn
     (goto-char (point-min))
     (let ((pos (org-find-exact-headline-in-buffer ,heading)))
       (if (and
            (markerp pos)
            (<= (marker-position pos) (point-max)))
           (progn
             (goto-char pos)
             ,@body)
           (error "position %s greater than point max %d" pos (point-max))))))
(put 'org-with-heading 'lisp-indent-function 1)

(defmacro org-with-heading-pos (pos heading &rest body)
  `(progn
     (goto-char (point-min))
     (let ((,pos (org-find-exact-headline-in-buffer ,heading)))
       (progn
         (when (and
                (markerp ,pos)
                (<= (marker-position ,pos) (point-max)))
           (goto-char ,pos))
         ,@body))))
(put 'org-with-heading-pos 'lisp-indent-function 2)

(defmacro org-with-buffer-headline (buffer heading &rest body)
  `(with-current-buffer (if ,buffer ,buffer (current-buffer))
     (org-with-heading ,heading ,@body)))
(put 'org-with-buffer-heading 'lisp-indent-function 2)

(defmacro org-with-file-headline (file heading &rest body)
  `(let ((buff (find-file-noselect ,file)))
     (if buff
         (with-current-buffer buff
           (org-with-heading ,heading ,@body))
         (error "can not open file %s" ,file))))
(put 'org-with-file-heading 'lisp-indent-function 2)

(setq org-refile-targets
      '((nil :maxlevel . 3)           ; only the current file
        (org-agenda-files :maxlevel . 3) ; all agenda files, 1st/2nd level
        (org-files-list :maxlevel . 4)   ; all agenda and all open files
        (lotus-org-files-list :maxlevel . 4))) ;all files returned by `lotus-org-files-list'

(defun lotus-org-files-list ()
  (remove nil
          (mapcar (lambda (buffer)
                    (buffer-file-name buffer))
                  (org-buffer-list 'files t))))

(defvar org-refile-region-format "\n%s\n")

(defvar org-refile-region-position 'top
  "Where to refile a region. Use 'bottom to refile at the
end of the subtree. ")

(defun lotus-org-refile-region (beg end copy)
  "Refile the active region.
If no region is active, refile the current paragraph.
With prefix arg C-u, copy region instad of killing it."
  (interactive "r\nP")
  ;; mark paragraph if no region is set
  (unless (use-region-p)
    (setq beg (save-excursion
                (backward-paragraph)
                (skip-chars-forward "\n\t ")
                (point))
          end (save-excursion
                (forward-paragraph)
                (skip-chars-backward "\n\t ")
                (point))))
  (org-with-refile file pos nil
    (let ((text (buffer-substring-no-properties beg end)))
      (unless copy (kill-region beg end))
      (deactivate-mark)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char pos)
          (if (eql org-refile-region-position 'bottom)
              (org-end-of-subtree)
              ;; (org-end-of-meta-data-and-drawers)
              (org-end-of-subtree))
          (insert (format org-refile-region-format text)))))))

(defvar org-refile-string-format "%s\n")

(defvar org-refile-string-position 'top
  "Where to refile a region. Use 'bottom to refile at the
end of the subtree. ")

(defun org-refile-string (text arg)
  "Refile the active region.
If no region is active, refile the current paragraph.
With prefix arg C-u, copy region instad of killing it."
  (interactive "sorg entry: \nP")
  ;; mark paragraph if no region is set
  (org-with-refile file pos nil
    ;; (unless arg (kill-region beg end))
    ;; (deactivate-mark)
    (with-current-buffer (find-file-noselect file)
      (let ((buffer-read-only nil))
        (save-excursion
          (goto-char pos)
          (if (eql org-refile-string-position 'bottom)
              (org-end-of-subtree)
              ;; (org-end-of-meta-data-and-drawers)
              ;; (org-end-of-meta-data)
              (org-end-of-subtree))
          (org-insert-subheading nil)
          (insert
           (format org-refile-string-format text)))))))

(defun org-find-heading-marker (heading &optional create)
  (let ((heading-marker (org-find-exact-headline-in-buffer heading)))
    (when create
      (unless heading-marker
        (goto-char (point-max))
        (insert (format "* %s\n" heading))
        (setq heading-marker (org-find-exact-headline-in-buffer heading))))
    heading-marker))

(defun org-find-file-heading-marker (file heading &optional create)
  (org-with-cloned-buffer (find-file-noselect file) "-<tree>"
    (org-mode)
    ;; (with-current-buffer (find-file-noselect file)
    (org-find-heading-marker heading create)))

(defmacro org-with-narrow-to-marker (marker &rest body)
  `(if ,marker
       (with-current-buffer (marker-buffer ,marker)
         (goto-char ,marker)
         (save-excursion
           (save-restriction
             (org-narrow-to-subtree)
             (progn
               ,@body))))
       (error "marker is nil")))
(put 'org-with-narrow-to-marker 'lisp-indent-function 1)

(defmacro org-with-narrow-to-heading-subtree (heading create &rest body)
  `(let ((marker (org-find-heading-marker ,heading ,create)))
     (when marker
       (org-with-narrow-to-marker marker ,@body))))
(put 'org-with-narrow-to-heading-subtree 'lisp-indent-function 2)

(defmacro org-with-narrow-to-file-heading-subtree (file heading create &rest body)
  `(let ((marker
          (with-current-buffer (find-file-noselect ,file)
            (org-find-heading-marker ,heading ,create))))
     (when marker
       (org-with-narrow-to-marker marker ,@body))))
(put 'org-with-narrow-to-file-heading-subtree 'lisp-indent-function 3)

(defmacro org-with-cloned-buffer (buff clone &rest body)
  `(with-current-buffer ,buff
     (let ((buff (or ,buff (current-buffer)))
           (clone-name (concat (or ,clone "<clone>") "-" (buffer-name))))
       (let ((pos (point)))
         (unwind-protect
              (progn
                (clone-indirect-buffer clone-name nil t)
                ;; (set-buffer clone-name)
                (with-current-buffer (get-buffer clone-name)
                  (widen)
                  (show-all)
                  ;; (org-mode)
                  ,@body))
           (when buff
             (setq pos (point))
             (set-buffer buff)
             (goto-char pos))
           (kill-buffer clone-name))))))
(put 'org-with-cloned-buffer 'lisp-indent-function 2)


(defmacro org-with-cloned-marker (marker clone &rest body)
  `(with-current-buffer (marker-buffer ,marker)
     (let ((clone-name (concat (or ,clone "<clone>") "-" (buffer-name)))
           (marker ,marker)
           (buff (marker-buffer ,marker)))
       (let ((pos (point)))
         (unwind-protect
              (progn
                (clone-indirect-buffer clone-name nil t)
                ;; (set-buffer clone-name)
                (with-current-buffer (get-buffer clone-name)
                  (goto-char (point-min))
                  (widen)
                  (show-all)
                  (goto-char (marker-position-nonil marker))
                  ;; (org-mode)
                  ,@body))
           (setq pos (point))
           (when buff
             (setq pos (point))
             (set-buffer buff)
             (goto-char pos))
           (kill-buffer clone-name))))))
(put 'org-with-cloned-marker 'lisp-indent-function 2)

(defun org-heading-has-child-p ()
  (save-excursion
    (org-goto-first-child)))

(defun org-number-of-subheadings ()
  (let ((curr-level (org-current-level)))
    (length
     (remove nil
            (org-map-entries
             #'(lambda ()
                 (= (1+ curr-level) (org-current-level)))
             nil
             'tree)))))

(defun org-goto-last-child ()
  (let ((curr-level (org-current-level)))
    (if (numberp curr-level)
        (when (org-heading-has-child-p)
          (org-end-of-subtree)
          (while (/= (1+ curr-level) (org-current-level))
            (outline-previous-visible-heading 1))))))

(defun org-goto-end-of-heading ()
  (let ((element (org-element-at-point)))
    (if (and
         element
         (eq (car element) 'heading))
        (let ((begin (plist-get (cadr element) :begin))
              (level (plist-get (cadr element) :level))
              (title (plist-get (cadr element) :title)))
         (goto-char (+ begin level (length title)))))))

(defun org-insert-subheading-at-point (subheading)
  "return point"
  (org-with-inhibit-modification-hooks
        ;; Debugger entered--Lisp error: (error "Invalid search bound (wrong side of point)")
        ;; re-search-forward(":[a-z0-9\\+_-]+?:" 128068 t)
        ;; emoji-cheat-sheet-plus--display-region(128714 128068)
        ;; emoji-cheat-sheet-plus--changed-hook(128068 128068 4)
        ;; org-move-subtree-down()
        ;; org-insert-heading-after-current()
        ;; org-insert-subheading-at-point("Unnamed task 557")
        ;; org-insert-subheadline-to-headline("Unnamed task 557" "Unnamed tasks" t)
        ;; org-insert-subheadline-to-file-headline("Unnamed task 557" "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/Unnamed.org" "Unnamed tasks" t)
        ;; lotus-org-create-unnamed-task()
        ;; occ-maybe-create-unnamed-tsk()
    (let ((buffer-read-only nil)
          (subheading (cond
                       ((stringp subheading) subheading)
                       ((functionp subheading) (funcall subheading))
                       (t (error "no subheading")))))
      (progn
        (if (org-heading-has-child-p)
            (progn
              (org-goto-last-child)
              (beginning-of-line)
              (end-of-line 1)
              (org-insert-heading-after-current))
          (progn
            (beginning-of-line)
            (end-of-line 1)
            (org-end-of-subtree)
            (org-insert-subheading nil)))
        (insert (format org-refile-string-format subheading))
        (point)))))

(defun org-insert-grandsubheading-at-point (subheading)
  (org-with-inhibit-modification-hooks
        ;; Debugger entered--Lisp error: (error "Invalid search bound (wrong side of point)")
        ;; re-search-forward(":[a-z0-9\\+_-]+?:" 128068 t)
        ;; emoji-cheat-sheet-plus--display-region(128714 128068)
        ;; emoji-cheat-sheet-plus--changed-hook(128068 128068 4)
        ;; org-move-subtree-down()
        ;; org-insert-heading-after-current()
        ;; org-insert-subheading-at-point("Unnamed task 557")
        ;; org-insert-subheadline-to-headline("Unnamed task 557" "Unnamed tasks" t)
        ;; org-insert-subheadline-to-file-headline("Unnamed task 557" "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/Unnamed.org" "Unnamed tasks" t)
        ;; lotus-org-create-unnamed-task()
        ;; occ-maybe-create-unnamed-tsk()
    (let ((buffer-read-only nil)
          (subheading (cond
                       ((stringp subheading) subheading)
                       ((functionp subheading) (funcall subheading))
                       (t (error "no subheading")))))
      (progn
        (if (eql org-refile-string-position 'bottom)
            (org-end-of-subtree)
          ;; (org-end-of-meta-data-and-drawers)
          ;; (org-end-of-meta-data)
          (org-end-of-subtree))
        (org-insert-subheading nil)
        (insert (format org-refile-string-format subheading))
        (point)))))




(defun org-insert-sibling-headline-at-point (subheading)
  (let ((buffer-read-only nil)
        (subheading (cond
                      ((stringp subheading) subheading)
                      ((functionp subheading) (funcall subheading))
                      (t (error "no subheading")))))
    ;; (if (eql org-refile-string-position 'bottom)
    ;;     (org-end-of-subtree)
    ;;     ;; (org-end-of-meta-data-and-drawers)
    ;;     ;; (org-end-of-meta-data)
    ;;     (org-end-of-subtree))

    (progn
      (beginning-of-line)
      (end-of-line 1)
      (org-insert-heading-after-current)
      (insert (format org-refile-string-format subheading))
      (point))))

(defun org-insert-grandsubheading-to-headline (text heading &optional create)
  (let ((pos (org-with-cloned-buffer (current-buffer) "<tree>"
               (org-with-narrow-to-heading-subtree
                heading create
                (org-insert-grandsubheading-at-point text)))))
    (copy-marker pos)))

(defun org-insert-grandsubheading-to-file-headline (text file heading &optional create)
  (let ((buff (find-file-noselect file)))
    (if buff
        (let ((pos (with-current-buffer buff
                     (org-with-cloned-buffer (current-buffer) "<tree>"
                       (org-insert-grandsubheading-to-headline text heading create)))))
          (copy-marker pos))
        (error "can not open file %s" file))))

(defun org-insert-sibling-headline-to-headline (text heading &optional create)
  (let ((pos (org-with-cloned-buffer (current-buffer) "<tree>"
               (org-with-narrow-to-heading-subtree
                heading create
                (org-insert-sibling-headline-at-point text)))))
    (copy-marker pos)))

(defun org-insert-sibling-headline-to-file-headline (text file heading &optional create)
  (let ((buff (find-file-noselect file)))
    (if buff
        (with-current-buffer buff
          (let ((pos (org-with-cloned-buffer (current-buffer) "<tree>"
                       (org-insert-sibling-headline-to-headline text heading create))))
            (copy-marker pos)))
        (error "can not open file %s" file))))

(defun org-insert-subheadline-to-headline (text heading &optional create)
  "return marker"
  (let ((pos (org-with-cloned-buffer (current-buffer) "<tree>"
               (org-with-narrow-to-heading-subtree
                heading create
                (org-insert-subheading-at-point text)))))
    (copy-marker pos)))

(defun org-insert-subheadline-to-file-headline (text file heading &optional create)
  "Create subheading with text in heading, return marker."
  (let ((buff (find-file-noselect file)))
    (if buff
        (with-current-buffer buff
          (org-insert-subheadline-to-headline text heading create))
        (error "can not open file %s" file))))

(defun org-find-exact-subheading-in-heading (heading subheading)
  (org-with-narrow-to-heading-subtree
   heading nil
   (org-find-exact-headline-in-buffer subheading)))
  ;; )


  ;; (progn ;; "property"

(defun org-refile-entry-put (property value)
  (interactive
   (let ((property (read-from-minibuffer "property: "))
         (value    (read-from-minibuffer "value: ")))
     (list property value)))
  (org-with-refile file pos nil
    (let ((buffer-read-only nil))
      (org-entry-put nil property value))))

(defun org-refile-entry-put-multivalued-property (property &rest values)
  (interactive
   (let ((property (read-from-minibuffer "property: "))
         (value    (read-from-minibuffer "value: ")))
     (list property value)))
  (org-with-refile file pos nil
    (let ((buffer-read-only nil))
      (org-entry-put-multivalued-property nil property values))))
    ;; )

(defun org-refile-target-files (org-refile-targets &optional default-buffer)
  (let ( ;; (case-fold-search nil)
         ;; otherwise org confuses "TODO" as a kw and "Todo" as a word
        (entries (or org-refile-targets '((nil . (:level . 1)))))
        files
        desc)
    (with-current-buffer (or default-buffer (current-buffer))
      (dolist (entry entries)
        (setq files (car entry) desc (cdr entry))
        (cond
         ((null files) (setq files (list (current-buffer))))
         ((eq files 'org-agenda-files)
          (setq files (org-agenda-files 'unrestricted)))
         ((and (symbolp files) (fboundp files))
          (setq files (funcall files)))
         ((and (symbolp files) (boundp files))
          (setq files (symbol-value files))))))
    files))

;; (org-refile-target-files '((occ-included-files :maxlevel . 4)))

(defun org-refile-target-check (org-refile-targets &optional default-buffer)
  (let* ((files
          (org-refile-target-files org-refile-targets default-buffer))
         (files
          (remove-if
           #'(lambda (f)
               (with-current-buffer (find-file-noselect f)
                 (eq 'org-mode major-mode)))
           files)))
    (when files
      (error "org-refile-target: files %s not in org-mode for org-refile-targets %s"
             files
             org-refile-targets))))

;; (org-refile-target-check '((occ-included-files :maxlevel . 4)))

;; Refile macros Starts
(defun safe-org-refile-get-location-p ()
  (member major-mode safe-org-refile-get-location-modes))

(defun safe-org-refile-get-location ()
  (let ((org-refile-targets
         (if (safe-org-refile-get-location-p)
             org-refile-targets
           (remove-if '(lambda (e) (null (car e))) org-refile-targets))))
    (org-refile-target-check org-refile-targets)
    (org-refile-get-location)))

;; TODO (replace-buffer-in-windows)

(defun quiet--select-frame (frame &optional norecord)
  ;; (select-frame frame norecord)
  ;; select-frame-set-input-focus should not be used as it will pull window if hidden.
  (select-frame frame norecord)
  ;; (raise-frame frame)
  ;; Ensure, if possible, that FRAME gets input focus.
  ;; (when (memq (window-system frame) '(x w32 ns))
  ;;   (x-focus-frame frame))
  ;; Move mouse cursor if necessary.
  (cond
    (mouse-autoselect-window
     (let ((edges (window-inside-edges (frame-selected-window frame))))
       ;; Move mouse cursor into FRAME's selected window to avoid that
       ;; Emacs mouse-autoselects another window.
       (set-mouse-position frame (nth 2 edges) (nth 1 edges))))
    (focus-follows-mouse
     ;; Move mouse cursor into FRAME to avoid that another frame gets
     ;; selected by the window manager.
     (set-mouse-position frame (1- (frame-width frame)) 0))))

(defmacro helm-timed (timeout &rest body)
  (lexical-let ((temp-win-config (make-symbol "test-helm-timed")))
    `(lexical-let* ((,temp-win-config (current-window-configuration))
                    (current-command (or
                                      (helm-this-command)
                                      this-command))
                    (str-command     (helm-symbol-name current-command))
                    (buf-name        (format "*helm-mode-%s*" str-command))
                    (timer (run-with-idle-plus-timer ,timeout nil
                                                #'(lambda (buffname)
                                                    (let* ((buff (or
                                                                  (get-buffer buffname)
                                                                  (get-buffer "*helm*")))
                                                           (w (if buff (get-buffer-window buff))))
                                                      (message "triggered timer for new-win %s" w)
                                                      (when (and w (windowp w) (window-valid-p w))
                                                        (delete-window w)
                                                        (when (active-minibuffer-window)
                                                          (abort-recursive-edit)
                                                          (message nil))
                                                        (when (fboundp 'remove-function)
                                                          (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame))
                                                        (when ,temp-win-config
                                                          (set-window-configuration ,temp-win-config)
                                                          (setq ,temp-win-config nil)))))
                                                buf-name)))
       (unwind-protect
            (progn
              (when (fboundp 'add-function)
                (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame))
              (progn
                ,@body))
         (when (fboundp 'remove-function)
           (remove-function (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame))
         (cancel-timer timer)))))
(put 'helm-timed 'lisp-indent-function 1)

(defun safe-timed-org-refile-get-location (timeout)
  ;; TODO: org-fit-window-to-buffer
  ;; TODO: as clean up reset newwin configuration
  (lexical-let* ((current-command (or
                                   (helm-this-command)
                                   this-command))
                 (str-command     (helm-symbol-name current-command))
                 (buf-name        (format "*helm-mode-%s*" str-command))
                 (timer (run-with-idle-plus-timer timeout nil
                                             #'(lambda (buffname)
                                                 (let* ((buff (get-buffer buffname))
                                                        (w (if buff (get-buffer-window buff))))
                                                   (message "triggered timer for new-win %s" w)
                                                   (when (and w
                                                              (windowp w)
                                                              (window-valid-p w))
                                                     (delete-window w)
                                                     (when (active-minibuffer-window)
                                                       (abort-recursive-edit)
                                                       (message nil))
                                                     (when (fboundp 'remove-function)
                                                       (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)))))
                                             buf-name)))
    (unwind-protect
         (progn
           (when (fboundp 'add-function)
             (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame))
           (safe-org-refile-get-location))
      (when (fboundp 'remove-function)
        (remove-function (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame))
      (cancel-timer timer))))

(defmacro org-with-refile (file pos refile-targets &rest body)
  "Refile the active region.
If no region is active, refile the current paragraph.
With prefix arg C-u, copy region instad of killing it."
  ;; mark paragraph if no region is set
  `(let* ((org-refile-targets (or ,refile-targets org-refile-targets))
          (target (save-excursion (safe-org-refile-get-location)))
          (,file (nth 1 target))
          (,pos (nth 3 target)))
     (with-current-buffer (find-file-noselect ,file)
       (save-excursion
         (goto-char ,pos)
         ,@body))))
(put 'org-with-refile 'lisp-indent-function 3)

(defmacro org-file-loc-with-refile (file pos refile-targets &rest body)
  "Refile run body with file and loc set."
  ;; mark paragraph if no region is set
  `(let* ((org-refile-targets (or ,refile-targets org-refile-targets))
          (target (save-excursion (safe-org-refile-get-location)))
          (,file (nth 1 target))
          (,pos (nth 3 target)))
     (lotus-with-file-pos ,file ,pos
                          ,@body)))
(put 'org-file-loc-with-refile 'lisp-indent-function 3)

;; (defmacro org-timed-file-loc-with-refile (file pos timeout refile-targets &rest body)
(defmacro org-with-file-loc-timed-refile (file pos timeout refile-targets &rest body)
  "Refile run body with file and loc set."
  ;; mark paragraph if no region is set
  `(let* ((org-refile-targets (or ,refile-targets org-refile-targets))
          (target (save-excursion (safe-timed-org-refile-get-location ,timeout)))
          (,file (nth 1 target))
          (,pos (nth 3 target)))
     (assert ,file)
     (assert ,pos)
     (lotus-with-file-pos ,file ,pos
                          ,@body)))
(put 'org-with-file-loc-timed-refile 'lisp-indent-function 4)

;; TODO: org-fit-window-to-buffer
;; (defmacro org-miniwin-file-loc-with-refile (win file pos refile-targets &rest body)
(defmacro org-with-file-loc-refile-new-win (file pos refile-targets newwin &rest body)
  `(org-file-loc-with-refile
       ,file ,pos ,refile-targets
       (lotus-with-file-pos-new-win
           ,file ,pos ,newwin
           ,@body)))
(put 'org-miniwin-file-loc-with-refile 'lisp-indent-function 4)

;; TODO: org-fit-window-to-buffer
;; (defmacro org-timed-miniwin-file-loc-with-refile (win file pos timeout refile-targets &rest body)
(defmacro org-with-file-loc-timed-refile-new-win (file pos timeout refile-targets newwin &rest body)
  `(org-with-file-loc-timed-refile
       ,file ,pos ,timeout(show-all)
           (read-only-mode)
           (org-previous-visible-heading 1)
           (let ((info (org-context-clock-collect-task)))
             info) ,refile-targets
       (lotus-with-file-pos-new-win
           ,file ,pos ,newwin
           ,@body)))
(put 'org-with-file-loc-timed-refile-new-win 'lisp-indent-function 5)

;; TODO: org-fit-window-to-buffer
;; (defmacro org-timed-miniwin-file-loc-with-refile (win file pos timeout refile-targets &rest body)
(defmacro org-with-file-loc-timed-refile-timed-new-win (file pos
                                                        timeout-refile refile-targets
                                                        timeout-newwin timer-newwin
                                                        cleanupfn-newwin cleanupfn-local
                                                        newwin
                                                        &rest body)
  `(org-with-file-loc-timed-refile
    ,file ,pos ,timeout-refile ,refile-targets
    (lotus-with-file-pos-timed-new-win
     ,file ,pos ,timeout-newwin ,timer-newwin ,cleanupfn-newwin ,cleanupfn-local ,newwin ,@body)))
(put 'org-with-file-loc-timed-refile-timed-new-win 'lisp-indent-function 9)

;; e.g.
;; (org-miniwin-file-loc-with-refile nil nil)
;;)
;; Refile macros Ends




























;; (progn ;; "move org"

(defun jay/refile-to (file heading)
  "Move current heading to specified location"
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer heading))))
    (org-refile nil nil (list heading file nil pos))))

(defun jay/refile-to-bookmarks ()
  "Move current heading to bookmarks"
  (interactive)
  (org-mark-ring-push)
  (jay/refile-to "~/Org/bookmarks.org" "New")
  (org-mark-ring-goto))

;; (save-excursion (safe-org-refile-get-location))


  ;; (progn ;; "org log note"
(setq org-log-into-drawer "LOGBOOK")

;; (defun org-insert-log-note (txt)
;;   "Finish taking a log note, and insert it to where it belongs."
;;   ;; (setq org-log-note-purpose purpose
;;   ;;       org-log-note-state state
;;   ;;       org-log-note-previous-state prev-state
;;   ;;       org-log-note-how how
;;   ;;       org-log-note-extra extra
;;   ;;       org-log-note-effective-time (org-current-effective-time))
;;   (unless (> (marker-position-nonil org-log-note-return-to) 0)
;;     (move-marker org-log-note-return-to (point)))
;;   (unless (> (marker-position-nonil org-log-note-marker) 0)
;;     (move-marker org-log-note-marker (point)))
;;   ;; Preserve position even if a property drawer is inserted in the
;;   ;; process.
;;   (set-marker-insertion-type org-log-note-marker t)
;;   (let ((txt txt)
;;         (org-log-note-purpose 'clock-out)
;;         (org-log-note-effective-time (org-current-effective-time)))
;;     ;; (kill-buffer (current-buffer))
;;     (let ((note (cdr (assq org-log-note-purpose org-log-note-headings)))
;;           lines)
;;       ;; (while (string-match "\\`# .*\n[ \t\n]*" txt)
;;       ;;   (setq txt (replace-match "" t t txt)))
;;       ;; (if (string-match "\\s-+\\'" txt)
;;       ;;     (setq txt (replace-match "" t t txt)))
;;       (setq lines (org-split-string txt "\n"))
;;       (when (and note (string-match "\\S-" note))
;;         (setq note
;;               (org-replace-escapes
;;                note
;;                (list (cons "%u" (user-login-name))
;;                      (cons "%U" user-full-name)
;;                      (cons "%t" (format-time-string
;;                                  (org-time-stamp-format 'long 'inactive)
;;                                  org-log-note-effective-time))
;;                      (cons "%T" (format-time-string
;;                                  (org-time-stamp-format 'long nil)
;;                                  org-log-note-effective-time))
;;                      (cons "%d" (format-time-string
;;                                  (org-time-stamp-format nil 'inactive)
;;                                  org-log-note-effective-time))
;;                      (cons "%D" (format-time-string
;;                                  (org-time-stamp-format nil nil)
;;                                  org-log-note-effective-time))
;;                      (cons "%s" (cond
;;                                   ((not org-log-note-state) "")
;;                                   ((org-string-match-p org-ts-regexp
;;                                                        org-log-note-state)
;;                                    (format "\"[%s]\""
;;                                            (substring org-log-note-state 1 -1)))
;;                                   (t (format "\"%s\"" org-log-note-state))))
;;                      (cons "%S"
;;                            (cond
;;                              ((not org-log-note-previous-state) "")
;;                              ((org-string-match-p org-ts-regexp
;;                                                   org-log-note-previous-state)
;;                               (format "\"[%s]\""
;;                                       (substring
;;                                        org-log-note-previous-state 1 -1)))
;;                              (t (format "\"%s\""
;;                                         org-log-note-previous-state)))))))
;;         (when lines (setq note (concat note " \\\\")))
;;         (push note lines))
;;       (when (or current-prefix-arg org-note-abort)
;;         (when (org-log-into-drawer)
;;           (org-remove-empty-drawer-at org-log-note-marker))
;;         (setq lines nil))
;;       (when lines
;;         (with-current-buffer (marker-buffer org-log-note-marker)
;;           (org-with-wide-buffer
;;            (goto-char org-log-note-marker)
;;            (move-marker org-log-note-marker nil)
;;            ;; Make sure point is at the beginning of an empty line.
;;            (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert "\n")))
;;                  ((looking-at "[ \t]*\\S-") (save-excursion (insert "\n"))))
;;            ;; In an existing list, add a new item at the top level.
;;            ;; Otherwise, indent line like a regular one.
;;            (let ((itemp (org-in-item-p)))
;;              (if itemp
;;                  (indent-line-to
;;                   (let ((struct (save-excursion
;;                                   (goto-char itemp) (org-list-struct))))
;;                     (org-list-get-ind (org-list-get-top-point struct) struct)))
;;                  (org-indent-line)))
;;            (insert (org-list-bullet-string "-") (pop lines))
;;            (let ((ind (org-list-item-body-column (line-beginning-position))))
;;              (dolist (line lines)
;;                (insert "\n")
;;                (indent-line-to ind)
;;                (insert line)))
;;            (message "Note stored")
;;            (org-back-to-heading t)
;;            (org-cycle-hide-drawers 'children))
;;           ;; Fix `buffer-undo-list' when `org-store-log-note' is called
;;           ;; from within `org-add-log-note' because `buffer-undo-list'
;;           ;; is then modified outside of `org-with-remote-undo'.
;;           (when (eq this-command 'org-agenda-todo)
;;             (setcdr buffer-undo-list (cddr buffer-undo-list)))))))
;;   ;; Don't add undo information when called from `org-agenda-todo'
;;   (let ((buffer-undo-list (eq this-command 'org-agenda-todo)))
;;     (set-window-configuration org-log-note-window-configuration)
;;     (with-current-buffer (marker-buffer org-log-note-return-to)
;;       (goto-char org-log-note-return-to))
;;     (move-marker org-log-note-return-to nil)
;;     (move-marker org-log-note-marker nil)
;;     (and org-log-post-message (message "%s" org-log-post-message))))
;; )

;; (org-miniwin-file-loc-with-refile nil nil)

;; https://gist.github.com/tonyday567/4343164
(defun org-random-entry (&optional arg)
  "Select and goto a random todo item from the global agenda"
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq arg org-agenda-overriding-arguments))
  (if (and (stringp arg) (not (string-match "\\S-" arg))) (setq arg nil))
  (let* (entries
         (today (org-today))
         (date (calendar-gregorian-from-absolute today))
         (kwds org-todo-keywords-for-agenda)
         (lucky-entry nil)
         (completion-ignore-case t)
         (org-agenda-buffer (when (buffer-live-p org-agenda-buffer)
                              org-agenda-buffer))
         (org-select-this-todo-keyword
          (if (stringp arg) arg
            (and arg (integerp arg) (> arg 0)
                 (nth (1- arg) kwds))))
         rtn rtnall files file pos marker buffer)
    (when (equal arg '(4))
      (setq org-select-this-todo-keyword
            (completing-read "Keyword (or KWD1|K2D2|...): "
                             (mapcar 'list kwds) nil nil)))
    (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
    (catch 'exit
      (org-compile-prefix-format 'todo)
      (org-set-sorting-strategy 'todo)
      (setq files (org-agenda-files nil 'ifmode)
            rtnall nil)
      (while (setq file (pop files))
        (catch 'nextfile
          (org-check-agenda-file file)
          (setq rtn (org-agenda-get-day-entries file date :todo))
          (setq rtnall (append rtnall rtn))))

      (when rtnall
        (setq lucky-entry
              (nth (random
                    (safe-length
                     (setq entries rtnall)))
                   entries))

        (setq marker (or (get-text-property 0 'org-marker lucky-entry)
                         (org-agenda-error)))
        (setq buffer (marker-buffer marker))
        (setq pos (marker-position marker))
        (pop-to-buffer-same-window buffer)
        (widen)
        (goto-char pos)
        (when (derived-mode-p 'org-mode)
          (org-show-context 'agenda)
          (save-excursion
            (and (outline-next-heading)
                 (org-flag-heading nil))) ; show the next heading
          (when (outline-invisible-p)
            (show-entry))                 ; display invisible text
          (run-hooks 'org-agenda-after-show-hook))))))

(provide 'org-misc-utils-lotus)
;;; org-misc-utils-lotus.el ends here
