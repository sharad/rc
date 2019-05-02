;;; occ-obj-simple.el --- occ simple methods         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <spratap@merunetworks.com>
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

(provide 'occ-obj-simple)


(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(require 'org-capture+-helm)


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-prop)
(require 'occ-helm)


(cl-defmethod occ-fontify-like-in-org-mode ((tsk occ-tsk))
  (let* ((level    (or (occ-get-property tsk 'level) 0))
         (filename (occ-get-property tsk 'file))
         (heading  (occ-get-property tsk 'heading-prop))
         (heading-prefix  " ")
         (prefix  (concat (make-string level ?\*) " ")))
    (if nil ;; if test without else with prefix
        (substring
         (org-fontify-like-in-org-mode
          (concat prefix heading)
          org-odd-levels-only)
         (1+ level))

      (if (eq heading 'noheading)
          (concat "file: " filename)
        (concat
         heading-prefix
         (org-fontify-like-in-org-mode
          (concat prefix heading)
          org-odd-levels-only))))))


(cl-defmethod occ-print ((obj occ-tsk))
  ;; (format "[%4d] %s"
  ;;         0
  ;;         (occ-fontify-like-in-org-mode tsk))
  (let* ((align      100)
         (heading    (occ-fontify-like-in-org-mode obj))
         (headinglen (length heading))
         (tags       (occ-get-property obj 'tags))
         (tagstr     (if tags (concat ":" (mapconcat #'identity tags ":") ":"))))
    (if tags
        (format
         (format "%%-%ds         %%s" align (if (< headinglen align) (- align headinglen) 0))
         heading tagstr)
      (format "%s" heading))))

(cl-defmethod occ-print ((obj occ-ctsk))
  (let ((tsk (occ-ctsk-tsk obj)))
    (occ-fontify-like-in-org-mode tsk)))

(cl-defmethod occ-print ((obj occ-ctxual-tsk))
  (let ((tsk (occ-ctxual-tsk-tsk obj)))
    (format "[%4d] %s"
            (occ-ctxual-tsk-rank obj)
            (occ-fontify-like-in-org-mode tsk))))


;; could be handled with
;;
;; (cl-defmethod occ-rank ((tsk-pair (head current-clock))
;;                            (ctx occ-ctx))
(defun occ-current-tsk ()
  (when (and
         org-clock-marker
         (markerp org-clock-marker)
         (> (marker-position-nonil org-clock-marker) 0))
    (org-with-cloned-marker org-clock-marker "<tree>"
      (let ((view-read-only nil)
            (buffer-read-only t))
        (read-only-mode)
        (org-previous-visible-heading 1)
        (let ((tsk (occ-make-tsk
                    (or org-clock-hd-marker org-clock-marker)
                    (occ-tsk-builder))))
          tsk)))))
;; Create tsk info out of current clock:1 ends here

(cl-defmethod occ-associated-p ((tsk symbol)
                                (ctx occ-ctx))
  0)

(cl-defmethod occ-associated-p ((tsk occ-tsk)
                                (ctx occ-ctx))
  "Test if TSK is associate to CTX"
  (if tsk
      (occ-rank tsk ctx)
    0))
;; Test if TSK is associate to CTX:1 ends here

;; Collect and return tsk matching to CTX

(cl-defmethod occ-current-associated-p ((ctx occ-ctx))
  (let ((tsk (occ-tsk-current-tsk)))
    (when tsk (occ-rank tsk ctx))))


(cl-defgeneric occ-goto (obj)
  "occ-goto")

(cl-defmethod occ-goto ((obj marker))
  (progn
    (switch-to-buffer (marker-buffer obj))
    ;; TODO find about "org-overview"
    ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
    ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
    ;; https://github.com/facetframer/orgnav
    ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
    ;; (outline-show-all)
    (org-content 10)
    (goto-char obj)))

(cl-defmethod occ-goto ((obj occ-tsk))
  (let ((mrk (occ-tsk-marker obj)))
    (if (and
         (markerp mrk)
         (marker-buffer mrk))
        (occ-goto mrk)
      (error "marker %s invalid." mrk))))

(cl-defmethod occ-goto ((obj occ-ctsk))
  (occ-goto (occ-ctsk-tsk obj)))

(cl-defmethod occ-goto ((obj occ-ctxual-tsk))
  (occ-goto (occ-ctxual-tsk-marker obj)))


(cl-defgeneric occ-set-to (obj)
  "occ-set-to")

(cl-defmethod occ-set-to ((obj marker))
  (progn
    (set-buffer (marker-buffer obj))
    ;; TODO find about "org-overview"
    ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
    ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
    ;; https://github.com/facetframer/orgnav
    ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
    ;; (outline-show-all)
    ;; (org-content 10)
    (goto-char obj)))

(cl-defmethod occ-set-to ((obj occ-tsk))
  (let ((mrk (occ-tsk-marker obj)))
    (if (and
         (markerp mrk)
         (marker-buffer mrk))
        (occ-set-to mrk)
      (error "marker %s invalid." mrk))))

(cl-defmethod occ-set-to ((obj occ-ctsk))
  (occ-set-to (occ-ctsk-tsk obj)))

(cl-defmethod occ-set-to ((obj occ-ctxual-tsk))
  (occ-set-to (occ-ctxual-tsk-marker obj)))


(defvar occ-capture+-helm-templates-alist org-capture+-helm-templates-alist)

(defun occ-capture+-helm-select-template ()
  (org-capture+-helm-select-template
   nil
   occ-capture+-helm-templates-alist))


(cl-defgeneric occ-capture (obj)
  "occ-capture")

(cl-defmethod occ-capture ((obj marker))
  (org-capture+
   'entry
   `(marker ,obj)
   'occ-capture+-helm-select-template
   :empty-lines 1))

(cl-defmethod occ-capture ((obj occ-tsk))
  (let ((mrk (occ-tsk-marker obj)))
    (occ-capture mrk)))

(cl-defmethod occ-capture ((obj occ-ctsk))
  (let* ((tsk        (occ-ctsk-tsk obj))
         (ctx        (occ-ctsk-ctx obj))
         (mrk        (occ-tsk-marker tsk))
         (template   (occ-capture+-helm-select-template))
         (clock-in-p helm-current-prefix-arg))
    (when template
     (before-org-capture+ mrk 'entry `(marker ,mrk) template '(:empty-lines 1)
       (progn
         (occ-obj-prop-edit tsk ctx 7)
         (let ((newchild (occ-make-tsk nil)))
           (when newchild
             (occ-induct-child tsk newchild)))
         t)))))


(cl-defmethod occ-induct-child ((obj occ-tree-tsk) (child occ-tree-tsk))
  (push child (occ-tree-tsk-tree obj)))

(cl-defmethod occ-induct-child ((obj occ-list-tsk) (child occ-list-tsk))
  (push child (occ-list-tsk-list obj)))


(cl-defgeneric occ-child (obj)
  "occ-child")

(cl-defmethod occ-child ((obj marker))
  (occ-capture obj)
  nil)

(cl-defmethod occ-child ((obj occ-tsk))
  (occ-capture obj)
  nil)

(cl-defmethod occ-child ((obj occ-ctsk))
  (occ-capture obj)
  nil)

(cl-defmethod occ-child ((obj occ-ctxual-tsk))
  (occ-capture obj)
  nil)

;; (before-org-capture+)

(cl-defgeneric occ-child-with-prop-edit (obj)
  "occ-child")

(cl-defmethod occ-child-with-prop-edit ((obj marker) (ctx occ-ctx))
  (occ-capture obj)
  (occ-obj-prop-edit obj ctx))

(cl-defmethod occ-child-with-prop-edit ((obj occ-tsk) (ctx occ-ctx))
  (occ-capture obj)
  (occ-obj-prop-edit obj ctx))

(cl-defmethod occ-child-with-prop-edit ((obj occ-ctxual-tsk))
  (occ-capture obj)
  (occ-obj-prop-edit (occ-ctxual-tsk-tsk obj) (occ-ctxual-tsk-ctx obj)))


(cl-defgeneric occ-rank (obj ctx)
  "occ-rank")

(cl-defmethod occ-rank (tsk-pair ctx)
  0)

;; ISSUE? should it return rank or occ-ctxual-tsk
(cl-defmethod occ-rank ((tsk occ-tsk)
                        (ctx occ-ctx))
  (let ((rank
         (reduce #'+
                 (mapcar
                  #'(lambda (slot)
                      (occ-rank (cons slot tsk) ctx)) ;TODO: check if method exist or not, or use some default method.
                  (occ-class-slots tsk)))))
    rank))


(cl-defmethod occ-files ()
  (occ-collect-files (occ-collection-object)))


(cl-defgeneric occ-candidate (obj)
  "occ-candidate")

(cl-defmethod occ-candidate ((obj marker))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the obj
pointing to it."
  (when (marker-buffer obj)
    (with-current-buffer (org-base-buffer (marker-buffer obj))
      (org-with-wide-buffer
       (progn ;; ignore-errors
         (goto-char obj)
         (let* ((cat (org-get-category))
                (heading (org-get-heading 'notags))
                (prefix (save-excursion
                          (org-back-to-heading t)
                          (looking-at org-outline-regexp)
                          (match-string 0)))
                (org-heading
                 (substring
                  (org-fontify-like-in-org-mode
                   (concat prefix heading)
                   org-odd-levels-only)
                  (length prefix))))
           (when org-heading ;; (and cat org-heading)
             ;; (insert (format "[%c] %-12s  %s\n" i cat org-heading))
             ;; obj
             (cons org-heading obj))))))))

(cl-defmethod occ-candidate ((obj occ-obj))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-print obj) obj))

;; (cl-defmethod occ-candidate ((obj occ-tsk))
;;   "Insert a line for the clock selection menu.
;; And return a cons cell with the selection character integer and the marker
;; pointing to it."
;;   (cons (occ-print obj) obj))

;; (cl-defmethod occ-candidate ((obj occ-ctxual-tsk))
;;   "Insert a line for the clock selection menu.
;; And return a cons cell with the selection character integer and the marker
;; pointing to it."
;;   (cons (occ-print obj) obj))

;; function to setup ctx clock timer:2 ends here


(defun occ-list-select-internal (candidates actions &optional timeout)
  ;; (occ-debug :debug "sacha marker %s" (car dyntskpls))
  (message "Running occ-sacha-helm-select")
  (prog1
      (helm
       (occ-helm-build-candidates-source
        candidates
        actions))
    (message "Running occ-sacha-helm-select1")))

(defun occ-list-select (candidates actions &optional timeout)
  (let ((timeout (or timeout 7)))
    (helm-timed timeout
      (message "running sacha/helm-select-clock")
      (occ-list-select-internal candidates actions))))


(cl-defmethod occ-collection-obj-matches ((collection occ-list-collection)
                                          (obj null))
  "Return all TSKs for context nil OBJ"
  ;; (message "occ-collection-obj-matches list")
  (occ-collection-list collection))


;; ISSUE? should it return rank or occ-ctxual-tsks list
(cl-defmethod occ-collection-obj-matches ((collection occ-list-collection)
                                          (obj occ-ctx))
  "Return matched CTXUAL-TSKs for context CTX"
  ;; (message "occ-collection-obj-matches list")
  (let ((tsks (occ-collection collection))
        (obj obj))
    (remove-if-not
     #'(lambda (ctxual-tsk)
         (> (occ-ctxual-tsk-rank ctxual-tsk) 0))
     (mapcar
      #'(lambda (tsk)
          (occ-build-ctxual-tsk tsk obj))
      tsks))))

;; ISSUE? should it return rank or occ-ctxual-tsks map
(cl-defmethod occ-collection-obj-matches ((collection occ-tree-collection)
                                          (obj occ-ctx))
  ;; (message "occ-collection-obj-matches tree")
  "Return matched CTXUAL-TSKs for context CTX"
  (let ((tsks (occ-collection collection))
        (matched '()))
    (when tsks
      (occ-debug :debug "occ-collection-obj-matches BEFORE matched %s[%d]" matched (length matched))
      (occ-mapc-tree-tsks
       #'(lambda (tsk args)
           ;; (occ-debug :debug "occ-rank heading = %s" (occ-tsk-heading tsk))
           (let* ((ctxual-tsk (occ-build-ctxual-tsk tsk args))
                  (rank (occ-ctxual-tsk-rank ctxual-tsk)))
             (unless rank (error "occ-entries-associated-to-ctx-by-keys[lambda]: rank is null"))
             (when (> (occ-ctxual-tsk-rank ctxual-tsk) 0)
               (push ctxual-tsk matched)
               (occ-debug :debug "occ-entries-associated-to-ctx-by-keys[lambda]: tsk %s MATCHED RANK %d"
                          (occ-tsk-heading tsk)
                          (length matched)))))
       tsks
       obj))
    (occ-debug :debug "occ-entries-associated-to-ctx-by-keys: AFTER matched %s[%d]" "matched" (length matched))
    matched))

;;TODO: make it after method
(cl-defmethod occ-collection-obj-matches :around  ((collection occ-collection)
                                                   (obj occ-ctx)) ;TODO: make it after method
  "Return matched CTXUAL-TSKs for context CTX"
  (if (occ-collection-object)
      (let* ((ctxual-tsks (cl-call-next-method))
             (rankslist  (mapcar
                          #'(lambda (ctxual-tsk)
                              (occ-ctxual-tsk-rank ctxual-tsk))
                          ctxual-tsks))
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
        ;; (message "occ-collection-obj-matches :around finish")
        (occ-debug :debug "matched ctxtsks %s" (length ctxual-tsks))
        (remove-if-not
         #'(lambda (ctxual-tsk)
             (>= (occ-ctxual-tsk-rank ctxual-tsk) avgrank))
         ctxual-tsks))
    (error "(occ-collection-object) returned nil")))


(cl-defmethod occ-matches ((obj occ-ctx))
  "return CTXUAL-TSKs matches"
  (occ-collection-obj-matches (occ-collection-object) obj))

(cl-defmethod occ-matches ((obj null))
  "return TSKs matches"
  (occ-collection-obj-matches (occ-collection-object) obj))


;; (cl-defmethod occ-collection-obj-list ((collection occ-collection)
;;                                        (obj occ-ctx))
;;   "return CTXUAL-TSKs list"
;;   (occ-collection-obj-matches collection obj))

;; (cl-defmethod occ-collection-obj-list ((collection occ-collection)
;;                                        (obj null))
;;   "return TSKs list"
;;   (occ-collect-list collection))

(cl-defmethod occ-collection-obj-list ((collection occ-collection)
                                       (obj occ-ctx))
  "return CTSKs list"
  (let ((ctsks
         (run-unobtrusively
           (let ((tsks (occ-collect-list collection))) ;;????TODO
             (when tsks
               (mapcar
                #'(lambda (tsk) (occ-build-ctsk tsk obj))
                tsks))))))
    (unless (eq t ctsks)
      ctsks)))


(cl-defmethod occ-collection-obj-list ((collection occ-collection)
                                       (obj null))
  "return TSKs list"
  ;; (occ-make-tsk-container
  ;;  (occ-collect-list collection))
 (occ-collect-list collection))


;; http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

(cl-defgeneric occ-list (obj)
  "occ-list")

(cl-defmethod occ-list ((obj occ-ctx))
  "return CTXUAL-TSKs container"
  (occ-collection-obj-list (occ-collection-object) obj))

(cl-defmethod occ-list ((obj null))
  "return TSKs container"
  (occ-collection-obj-list (occ-collection-object) obj))

;; (occ-list (occ-make-ctx nil))
;; (occ-select (occ-make-ctx nil) #'occ-list 10)
;; (length (occ-collect-list (occ-collection-object)))
;; (length (occ-collect-tsks (occ-collection-object)))


;; TODO: Not to run when frame is not open [visible.]
;; Getting targets...done
;; Error running timer: (error "Window #<window 12> too small for splitting")
;; task-projbuffs-base-dir: changing supplied base-dir nil and task-projbuffs-base-dir to /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/
;; in occ-clock-in occ-ctx 1
;; Getting targets...done
;; Error running timer ‘occ-clock-in-curr-ctx-if-not’: (error "Window #<window 12> too small for splitting")

(cl-defmethod occ-select ((obj null) collector &optional timeout)
  "return interactively selected TSK or NIL"
  (let ((candidates (funcall collector obj)))
    (when candidates
      (occ-list-select candidates (occ-helm-actions obj) timeout))))

(cl-defmethod occ-select ((obj occ-ctx) collector &optional timeout)
  "return interactively selected TSK or NIL"
  (let ((candidates (funcall collector obj)))
    (when candidates
      (occ-list-select candidates (occ-helm-actions obj) timeout))))

;; (cl-defmethod occ-select ((obj occ-ctx) collector &optional timeout)
;;   "return interactively selected CTXUAL-TSK or NIL, marker and ranked version"
;;   (interactive
;;    (list (occ-make-ctx-at-point)))
;;   (progn
;;     (message "in occ-clock-in occ-ctx 1")
;;     (let* ((obj (or obj (occ-make-ctx)))
;;            (matched-ctxual-tsks
;;             (run-unobtrusively           ;heavy task
;;               ;; BUG Urgent TODO: SOLVE ASAP ???? at (occ-clock-in-if-not ctx) and (occ-clock-in ctx)
;;               ;; begin occ-clock-in-curr-ctx-if-not
;;               ;; 2019-03-06 22:55:31 s: occ-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug
;;               ;; occ-clock-in-if-not: Now really going to clock.
;;               ;; in occ-clock-in occ-ctx 1
;;               ;; user input 111 retval t
;;               ;; trying to create unnamed tsk.
;;               ;; occ-maybe-create-unnamed-tsk: Already clockin unnamed tsk
;;               ;; occ-clock-in-if-not: Now really clock done.
;;               (remove-if-not
;;                #'(lambda (ctxual-tsk)
;;                    (let* ((mrk (occ-ctxual-tsk-marker ctxual-tsk)))
;;                      (and
;;                       mrk
;;                       (marker-buffer mrk))))
;;                (funcall collector obj)))))
;;       (unless (eq matched-ctxual-tsks t)
;;         (when matched-ctxual-tsks
;;           (let* ((sel-ctxual-tsk
;;                   (if (> (length matched-ctxual-tsks) 1)
;;                       (occ-list-select matched-ctxual-tsks (occ-helm-actions obj) timeout)
;;                     (car matched-ctxual-tsks))))
;;             sel-ctxual-tsk))))))


(defvar *occ-clocked-ctxual-tsk-ctx-history* nil)
(defvar occ-clock-in-hooks nil "Hook to run on clockin with previous and next markers.")

(cl-defmethod occ-clock-in ((obj marker))
  (let ((org-log-note-clock-out nil))
    (when (marker-buffer obj)
      (with-current-buffer (marker-buffer obj)
        (let ((buffer-read-only nil))
          (condition-case-control t err
            (progn
              (occ-straight-org-clock-clock-in (list obj)))
            ((error)
             (signal (car err) (cdr err)))))))))

(cl-defmethod occ-clock-in ((obj occ-tsk))
  (occ-clock-in (occ-tsk-marker obj)))

(cl-defmethod occ-clock-in ((obj occ-ctxual-tsk))
  ;;TODO add org-insert-log-not
  (occ-debug :debug "occ-clock-in-marker %s" obj)
  (let* (retval
         (old-ctxual-tsk     (car *occ-clocked-ctxual-tsk-ctx-history*))
         (old-tsk            (when old-ctxual-tsk (occ-ctxual-tsk-tsk old-ctxual-tsk)))
         (old-marker         (or (if old-tsk (occ-tsk-marker old-tsk)) org-clock-hd-marker))
         (old-heading        (if old-tsk (occ-tsk-heading old-tsk)))
         (obj-tsk            (occ-ctxual-tsk-tsk obj))
         (new-marker         (if obj-tsk (occ-tsk-marker obj-tsk)))
         (new-heading        (if obj-tsk (occ-tsk-heading obj-tsk))))
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

          (setq *occ-update-current-ctx-msg* old-marker)

          (run-hook-with-args 'occ-clock-in-hooks
                              old-marker
                              new-marker)

          (when (and
                 new-heading
                 old-marker
                 (marker-buffer old-marker))
            (org-insert-log-note old-marker (format "clocking out to clockin to <%s>" new-heading)))
          (when old-heading
            (org-insert-log-note new-marker (format "clocking in to here from last clock <%s>" old-heading)))

          (occ-clock-in obj-tsk)
          (setq retval t)

          (push obj *occ-clocked-ctxual-tsk-ctx-history*)

          (if old-buff
              (with-current-buffer old-buff
                (setq buffer-read-only old-buff-read-only)))
          retval)))))

(cl-defmethod occ-clock-in ((obj occ-ctx))
  "Clock-in selected CTXUAL-TSK for occ-ctx OBJ or open interface for adding properties to heading."
  (let ((ctxual-tsk (occ-select obj #'occ-matches)))
    (if ctxual-tsk
        (occ-clock-in ctxual-tsk)
      (progn
        ;; here create unnamed tsk, no need
        (setq *occ-update-current-ctx-msg* "null clock")
        (occ-debug :debug
                   "No clock found please set a match for this ctx %s, add it using M-x occ-prop-edit-safe."
                   obj)
        (lwarn 'occ
               :debug
               "occ-clock-in(ctx):  with this-command=%s" this-command)
        (occ-delayed-select-obj-prop-edit-when-idle obj obj 7)
        nil))))

(cl-defmethod occ-clock-in ((obj null))
  (error "Can not clock in NIL"))

;; (occ-delayed-select-obj-prop-edit-when-idle (occ-make-ctx nil) (occ-make-ctx nil) 7)
;; (occ-delayed-select-obj-prop-edit-when-idle nil (occ-make-ctx nil) 7)


(defcustom *occ-last-buff-sel-time*            (current-time) "*occ-last-buff-sel-time*")
(defvar    *occ-buff-sel-timer*                nil)
(defvar    *occ-tsk-current-ctx-time-interval* 7)
(defvar    *occ-tsk-previous-ctx*              nil)
(defvar    *occ-tsk-current-ctx*               nil)

(cl-defmethod occ-clock-in-if-not ((ctx occ-ctx))
  (if (or
       (occ-clock-marker-is-unnamed-clock-p)
       (>= 0 (occ-associated-p (occ-current-tsk) ctx)))

      (progn                ;current clock is not matching
        (occ-debug :debug "occ-clock-in-if-not: Now really going to clock with this-command=%s" this-command)
        (unless (occ-clock-in ctx)

          ;; BUG Urgent TODO: SOLVE ASAP ???? at (occ-clock-in-if-not ctx) and (occ-clock-in ctx)

          ;; begin occ-clock-in-curr-ctx-if-not
          ;; 2019-03-06 22:55:31 s: occ-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug
          ;; occ-clock-in-if-not: Now really going to clock.
          ;; in occ-clock-in occ-ctx 1
          ;; user input 111 retval t
          ;; trying to create unnamed tsk.
          ;; occ-maybe-create-unnamed-tsk: Already clockin unnamed tsk
          ;; occ-clock-in-if-not: Now really clock done.


          ;; not able to find associated, or intentionally not selecting a clock
          (occ-debug :debug "trying to create unnamed tsk.")
          (occ-maybe-create-clockedin-unnamed-ctxual-tsk ctx))
        (occ-debug :debug "occ-clock-in-if-not: Now really clock done.")
        t)

      (progn
        (occ-debug :debug "occ-clock-in-if-not: Current tsk already associate to %s" ctx)
        nil)))

(cl-defmethod occ-clock-in-if-chg ((ctx occ-ctx))
  (if (>
       (float-time (time-since *occ-last-buff-sel-time*))
       *occ-tsk-current-ctx-time-interval*)
      (let* ((buff    (occ-ctx-buffer ctx)))
        (setq *occ-tsk-current-ctx* ctx)
        (if (and
             (occ-chgable-p)
             buff (buffer-live-p buff)
             (not (minibufferp buff))
             (not (ignore-p buff))
             (not              ;BUG: Reconsider whether it is catching case after some delay.
              (equal *occ-tsk-previous-ctx* *occ-tsk-current-ctx*)))
            (progn
              (when (occ-clock-in-if-not ctx)
                (setq *occ-tsk-previous-ctx* *occ-tsk-current-ctx*)))
          (occ-debug :nodisplay "occ-clock-in-if-chg: ctx %s not suitable to associate" ctx)))
    (occ-debug :nodisplay "occ-clock-in-if-chg: not enough time passed.")))


;;; occ-obj-simple.el ends here
