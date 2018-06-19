;;; occ-object-methods.el --- occ-api               -*- lexical-binding: t; -*-
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
(require 'occ-tree)
(require 'occ-ctor)

(when nil ;; https://curiousprogrammer.wordpress.com/2010/07/19/emacs-defstruct-vs-other-languages/

  (defun cl-get-field (object field)
    (cl-struct-slot-value (cl-classname object) field object))

  (defun cl-set-field (object field value)
    (setf (cl-struct-slot-value (cl-classname object) field object) value))

  (get-field dave 'name)
  (set-field dave 'name "Simon Smith"))

(cl-defmethod occ-get-property ((obj occ-obj)
                                prop)
  ;; mainly used by occ-task only
  (if (memq prop (cl-class-slots (cl-classname obj)))
      (cl-get-field obj prop)
    (plist-get
     (cl-struct-slot-value (cl-classname obj) 'plist obj)
     (sym2key prop))))
(cl-defmethod occ-set-property ((obj occ-obj)
                                prop
                                val)
  ;; mainly used by occ-task only
  (if (memq prop (cl-class-slots (cl-classname obj)))
      (setf (cl-struct-slot-value (cl-classname obj) prop obj) val)
    (plist-put
     (cl-struct-slot-value (cl-classname obj) 'plist obj)
     (sym2key prop) val)))
(cl-defmethod occ-class-slots ((obj occ-obj))
  (let* ((plist (cl-struct-slot-value (cl-classname obj) 'plist obj))
         (plist-keys (plist-get-keys plist))
         (slots (cl-class-slots (cl-classname obj))))
    (append slots
            (mapcar #'key2sym plist-keys))))


(defun occ-task-builder ()
  (let ((classname (cl-classname occ-global-task-collection)))
    (cond
      ((eq 'occ-list-task-collection classname)
       #'make-occ-list-task)
      ((eq 'occ-tree-task-collection classname)
       #'make-occ-tree-task)
      (t (error "occ-global-task-collection is not from occ-list-task-collection or occ-tree-task-collection class")))))

(cl-defmethod occ-fontify-like-in-org-mode ((task occ-task))
  (let* ((level   (or (occ-get-property task 'level) 0))
         (heading (occ-get-property task 'heading-prop))
         (prefix  (concat (make-string level ?\*) " ")))
    (if nil ;; if test without else with prefix
        (substring
         (org-fontify-like-in-org-mode
          (concat prefix heading)
          org-odd-levels-only)
         (1+ level))

      (org-fontify-like-in-org-mode
       (concat prefix heading)
       org-odd-levels-only))))





(cl-defmethod occ-contextual-task-marker ((contextask occ-contextual-task))
  (let* ((task (occ-contextual-task-task contextask))
         (marker (occ-task-marker task)))
    marker))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun occ-sacha-selection-line (marker)
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (when (marker-buffer marker)
    (with-current-buffer (org-base-buffer (marker-buffer marker))
      (org-with-wide-buffer
       (progn ;; ignore-errors
         (goto-char marker)
         (let* ((cat (org-get-category))
                (heading (org-get-heading 'notags))
                (prefix (save-excursion
                          (org-back-to-heading t)
                          (looking-at org-outline-regexp)
                          (match-string 0)))
                (task (substring
                       (org-fontify-like-in-org-mode
                        (concat prefix heading)
                        org-odd-levels-only)
                       (length prefix))))
           (when task ;; (and cat task)
             ;; (insert (format "[%c] %-12s  %s\n" i cat task))
             ;; marker
             (cons task marker))))))))

(cl-defmethod occ-print ((contextask occ-contextual-task))
  (let ((task (occ-contextual-task-task contextask)))
    (format "[%4d] %s"
            (occ-contextual-task-rank contextask)
            (occ-fontify-like-in-org-mode task))))

;; deprecated
(cl-defmethod occ-sacha-selection-line ((contextask occ-contextual-task))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (let ((marker (occ-contextual-task-marker contextask))
        (rank   (occ-contextual-task-rank   contextask)))
    (when (marker-buffer marker)
      (with-current-buffer (org-base-buffer (marker-buffer marker))
        (org-with-wide-buffer
         (progn ;; ignore-errors
           (goto-char marker)
           (let* ((cat (org-get-category))
                  (heading (org-get-heading 'notags))
                  (prefix (save-excursion
                            (org-back-to-heading t)
                            (looking-at org-outline-regexp)
                            (match-string 0)))
                  (task (substring
                         (org-fontify-like-in-org-mode
                          (concat prefix heading)
                          org-odd-levels-only)
                         (length prefix))))
             (when task ;; (and cat task)
               ;; (insert (format "[%c] %-12s  %s\n" i cat task))
               ;; marker
               (cons (occ-print contextask) contextask))))))))) ;TODO


(cl-defmethod occ-sacha-selection-line ((contextask occ-contextual-task))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-print contextask) contextask))
;; function to setup context clock timer:2 ends here

;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*function%20to%20setup%20context%20clock%20timer][function to setup context clock timer:3]]
;; rank based
(cl-defmethod occ-sacha-helm-select ((contextask occ-contextual-task))
  ;; (occ-debug :debug "sacha marker %s" (car dyntaskpls))
  (helm
   (list
    (helm-build-sync-source "Select matching tasks"
      :candidates (mapcar 'occ-sacha-selection-line contextask)
      :action (list ;; (cons "Select" 'identity)
               (cons "Clock in and track" #'identity))
      :history 'org-refile-history)
    ;; (helm-build-dummy-source "Create task"
    ;;   :action (helm-make-actions
    ;;            "Create task"
    ;;            'sacha/helm-org-create-task))
    )))

(cl-defmethod occ-sacha-helm-select-timed ((contextask occ-contextual-task))
  (helm-timed 7
    (message "running sacha/helm-select-clock")
    (occ-sacha-helm-select contextask)))

(defun occ-sacha-helm-action ((contextask occ-contextual-task) clockin-fn)
  ;; (message "sacha marker %s" (car dyntaskpls))
  ;; (setq sacha/helm-org-refile-locations tbl)
  (progn
    (helm
     (list
      (helm-build-sync-source "Select matching tasks"
        :candidates (mapcar 'occ-sacha-selection-line contextask)
        :action (list ;; (cons "Select" 'identity)
                 (cons "Clock in and track" #'(lambda (c) (funcall clockin-fn c))))
        :history 'org-refile-history)
      ;; (helm-build-dummy-source "Create task"
      ;;   :action (helm-make-actions
      ;;            "Create task"
      ;;            'sacha/helm-org-create-task))
      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *occ-clocked-contextual-task-context-history* nil)

(cl-defmethod occ-clockin ((new-contextask occ-contextual-task))
  ;;TODO add org-insert-log-not
  (occ-debug :debug "occ-clockin-marker %s" new-contextask)
  (let* (retval
         (old-contextual-task (car *occ-clocked-contextual-task-context-history*))
         (old-task            (when old-contextual-task (occ-contextual-task-task old-contextual-task)))
         (old-marker          (or (if old-task (occ-task-marker old-task)) org-clock-hd-marker))
         (old-heading         (if old-task (occ-task-heading old-task)))
         (new-task            (occ-contextual-task-task new-contextask))
         (new-marker          (if new-task (occ-task-marker new-task)))
         (new-heading         (if new-task (occ-task-heading new-task))))
  (when (and
         new-marker
         (marker-buffer new-marker))

    (let* ((org-log-note-clock-out nil)
           (old-marker org-clock-marker)
           (old-buff   (marker-buffer old-marker)))

      (occ-debug :debug "clocking in %s" new-marker)

      (let ((old-buff-read-only
             (if old-buff
                 (with-current-buffer (marker-buffer old-marker)
                   buffer-read-only))))

        (if old-buff
            (with-current-buffer old-buff
              (setq buffer-read-only nil)))

        (setq *occ-update-current-context-msg* old-marker)

        (when (and
               new-heading
               old-marker
               (marker-buffer old-marker))
          (org-insert-log-note old-marker (format "clocking out to clockin to <%s>" new-heading)))

        (with-current-buffer (marker-buffer new-marker)
          (let ((buffer-read-only nil))
            (when old-heading
              (org-insert-log-note new-marker (format "clocking in to here from last clock <%s>" old-heading)))
            (condition-case err
                (progn
                  (org-clock-clock-in (list new-marker))
                  (setq retval t)
                  (push new-contextask *occ-clocked-contextual-task-context-history*))
              ((error)
               (progn
                 (setq retval nil)
                 (signal (car err) (cdr err)))))))
        (if old-buff
            (with-current-buffer old-buff
              (setq buffer-read-only old-buff-read-only)))
        retval)))))

(cl-defmethod occ-run-associated-task ((context occ-context))
  "marker and ranked version"
  (interactive
   (list (occ-make-context)))
  (progn
    (let* ((context (or context (occ-make-context)))
           (matched-contextual-tasks
            (remove-if-not
             #'(lambda (contextual-task)

                 (let* ((marker (occ-contextual-task-marker contextual-task)))
                   (and
                    marker
                    (marker-buffer marker))))
             (occ-matching-contextual-tasks context))))
      (if matched-contextual-tasks
          (let* ((sel-contextual-task
                  (if (> (length matched-contextual-tasks) 1)
                      (occ-sacha-helm-select-timed matched-contextual-tasks)
                      (car matched-contextual-tasks)))
                 ;; (sel-task   (if sel-contextual-task (plist-get sel-contextual-task :task)))
                 ;; (sel-marker (if sel-task      (plist-get sel-task      :task-clock-marker)))
                 )
            ;; (occ-message 6 "sel-contextual-task %s sel-task %s sel-marker %s" sel-contextual-task sel-task sel-marker)
            (if sel-contextual-task (occ-clockin sel-contextual-task)))
          (progn
            ;; here create unnamed task, no need
            (setq *occ-update-current-context-msg* "null clock")
            (occ-message 6
                                       "No clock found please set a match for this context %s, add it using M-x occ-add-to-org-heading."
                                       context)
            (occ-add-to-org-heading-when-idle context 7)
            nil)))))

(cl-defgeneric occ-isassoc (obj context)
  "occ-isassoc"
  )

(cl-defmethod occ-isassoc ((task occ-task)
                       (context occ-context))
  (let ((rank
         (reduce #'+
                 (mapcar
                  #'(lambda (slot)
                      (occ-isassoc (cons slot task) context)) ;TODO: check if method exist or not, or use some default method.
                  (occ-class-slots task)))))
    (occ-make-contextual-task task context rank)))

(cl-defmethod occ-isassoc ((collection occ-tree-task-collection)
                       (context occ-context))
  (let ((tasks (occ-collection collection))
        (matched '()))
    (when tasks
      (occ-debug :debug "occ-entries-associated-to-context-by-keys: BEFORE matched %s[%d]" matched (length matched))
      (occ-tree-mapc-tasks
       #'(lambda (task args)
           ;; (occ-debug :debug "occ-isassoc heading = %s" (occ-task-heading task))
           (let* ((contextual-task (occ-isassoc task args))
                  (rank (occ-contextual-task-rank contextual-task)))
             (unless rank (error "occ-entries-associated-to-context-by-keys[lambda]: rank is null"))
             (when (> (occ-contextual-task-rank contextual-task) 0)
               (push contextual-task matched)
               (occ-debug :debug "occ-entries-associated-to-context-by-keys[lambda]: task %s MATCHED RANK %d"
                          (occ-task-heading task)
                          (length matched)))))
       tasks
       context))
    (occ-debug :debug "occ-entries-associated-to-context-by-keys: AFTER matched %s[%d]" "matched" (length matched))
    matched))

(cl-defmethod occ-isassoc ((collection occ-list-task-collection)
                       (context occ-context))
  (lexical-let ((tasks (occ-collection collection))
                (context context))
    (remove-if-not
     #'(lambda (contextual-task)
         (> (occ-contextual-task-rank contextual-task) 0))
     (mapcar
      #'(lambda (task)
          (occ-isassoc task context))
      tasks))))


(cl-defmethod occ-isassoc (task-pair context)
  0)

(cl-defmethod occ-isassoc ((task-pair (head root))
                       (context occ-context))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((root
          (occ-get-property (cdr task-pair) 'root))
         (root (if root (file-truename root))))
    (let* ((file (occ-context-file context))
           (file (if file (file-truename file))))
      (if root
          (progn
            (occ-debug :debug "task %s root %s" (occ-task-heading (cdr task-pair)) root)
            (occ-debug :debug "task %s file %s" (occ-task-heading (cdr task-pair)) file))
        (occ-debug :debug "task %s root %s not present."
                   (occ-task-heading (cdr task-pair)) root))
      (if (and root file
               (string-match root file))
          (length root)
        0))))

(cl-defmethod occ-isassoc ((task-pair (head currfile))
                       (context occ-context))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((currfile
          (occ-get-property (cdr task-pair) 'currfile))
         (currfile (if currfile (file-truename currfile))))
    (let* ((file (occ-context-file context))
           (file (if file (file-truename file))))
      (if currfile
          (progn
            (occ-debug :debug "task %s currfile %s" (occ-task-heading (cdr task-pair)) currfile)
            (occ-debug :debug "task %s file %s"     (occ-task-heading (cdr task-pair)) file))
        (occ-debug :debug "task %s currfile %s not present."
                   (occ-task-heading (cdr task-pair)) currfile))
      (if (and currfile file
               (string-match currfile file))
          (* 2 (length currfile))     ;as exact match to file giving double matching points.
        0))))

(cl-defmethod occ-isassoc ((task-pair (head status))
                       (context occ-context))
  "Predicate funtion to check if context matches to task's status attribute."
  (let ((todo-type
         (occ-get-property (cdr task-pair) 'todo-type))
        (closed
         (occ-get-property (cdr task-pair) 'closed))
        (status
         (occ-get-property (cdr task-pair) 'todo-keyword)))
    (if (or
         closed
         (eql todo-type 'done)
         (string-equal status "HOLD"))
        -30 0)))

(cl-defmethod occ-isassoc ((task-pair (head subtree))
                       (context occ-context))
  "Predicate funtion to check if context matches to task's status attribute."
  (let ((sub-tree
         (occ-get-property (cdr task-pair) 'subtree)))
    (occ-debug :debug "task %s subtree %s" (occ-task-heading (cdr task-pair)) (null (null sub-tree)))
    (if sub-tree -30 0)))

(cl-defmethod occ-isassoc ((task-pair (head key))
                       (context occ-context))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((key (occ-get-property (cdr task-pair) 'KEY)))
    (if key (string-to-number key) 0)))

(cl-defmethod occ-isassoc ((task-pair (head heading-level))
                       (context occ-context))
  "Predicate funtion to check if context matches to task's file attribute."
  (let* ((level
          (occ-get-property (cdr task-pair) 'level)))
    (if level level 0)))

(cl-defmethod occ-isassoc ((task-pair (head timebeing))
                       (context occ-context))
  (let ((timebeing (occ-get-property (cdr task-pair) 'timebeing)))
    (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
          (clocked-time   (occ-get-property (cdr task-pair) 'clock-sum)))
      (if (and
           (numberp clocked-time)
           (numberp timebeing-time)
           (> timebeing-time clocked-time))
          (- timebeing-time clocked-time)
        0))))

(cl-defmethod occ-isassoc ((task-pair (head current-clock))
                       (context occ-context))
  (let* ((task-marker
          (occ-get-property (cdr task-pair) 'marker)))
    (if (and
         (markerp org-clock-hd-marker)
         (markerp task-marker)
         (equal org-clock-hd-marker org-clock-hd-marker))
        100
      0)))

(cl-defmethod occ-readprop ((task-pair (head root))
                        (context occ-context))
  (let* ((file (if context (occ-context-file context)))
         (dir (if (stringp file) (file-name-directory file) (dirname-of-file file)))
         (prompt (concat (symbol-name (car task-pair)) ": ")))
    (ido-read-directory-name prompt dir dir)))

(cl-defmethod occ-readprop ((task-pair (head subtree))
                        (context occ-context))
  (let ((prompt (concat (symbol-name (car task-pair)) ": ")))
    (file-relative-name
     (ido-read-file-name ;; org-iread-file-name
      prompt
      default-directory default-directory))))

(cl-defmethod occ-writeprop ((task-pair (head subtree)))
  )

(when nil

  (cl-defmethod occ-isassoc (task-pair context)
    0)

  (cl-defmethod occ-isassoc ((task-pair (head root)) (context list))
    (message "%s" task-pair))

  (occ-isassoc '(root  1) nil)

  (occ-isassoc '(n  1) nil)

  (cl-defmethod occ-isassoc ((task occ-task)
                         (context occ-context))
    (message "match occ-isassoc"))

  (occ-isassoc (make-occ-tree-task) (make-occ-context))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmethod occ-matching-contextual-tasks ((context occ-context))
  ;; TODO Here do variance based filtering.
  (let* ((contextual-tasks (occ-isassoc (occ-collection-object) context))
         (rankslist  (mapcar
                      #'(lambda (contextual-task)
                          (occ-contextual-task-rank contextual-task))
                      contextual-tasks))
         (avgrank    (if (= 0 (length rankslist))
                         0
                       (/
                        (reduce #'+ rankslist)
                        (length rankslist))))
         (varirank   (if (= 0 (length rankslist))
                         0
                       (sqrt
                        (/
                         (reduce #'+
                                 (mapcar #'(lambda (rank) (expt (- rank avgrank) 2)) rankslist))
                         (length rankslist))))))
    (occ-debug :debug "matched contexttasks %s" (length contextual-tasks))
    (remove-if-not
     #'(lambda (contextual-task)
         (>= (occ-contextual-task-rank contextual-task) avgrank))
     contextual-tasks)))

(when nil

  (length
   (occ-matching-contextual-tasks
    (occ-make-context
     (find-file-noselect "/home/s/paradise/git/main/src/wnc/security/authenticator/accounting.cpp"))))

  (length
   (occ-matching-contextual-tasks
    (occ-make-context (current-buffer)))))

(provide 'occ-object-methods)
;;; occ-object-methods.el ends here
