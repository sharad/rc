;;; occ-obj-simple.el --- occ simple methods         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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
(require 'occ-obj-utils)
(require 'occ-util-common)
(require 'occ-prop)
(require 'occ-helm)


(defvar occ-idle-timeout 7)


(cl-defmethod occ-uniquify-file ((tsk occ-tsk))
  (let* ((filename (occ-get-property tsk 'file))
         (basename (file-name-nondirectory filename))
         (files (occ-files)))))
    ;; (uniquify-buffer-file-name)

;; (file-name-nondirectory "/aaa/aaa/aaa")

(cl-defmethod occ-class-name (obj)
  "unknown")

(cl-defmethod occ-class-name ((obj symbol))
  "symbol")

(cl-defmethod occ-class-name ((obj null))
  "null")

(cl-defmethod occ-class-name ((obj marker))
  "marker")

(cl-defmethod occ-class-name ((obj occ-tsk))
  "task")

(cl-defmethod occ-class-name ((obj occ-ctsk))
  "context task")

(cl-defmethod occ-class-name ((obj occ-ctxual-tsk))
  "contextual task")


(cl-defmethod occ-obj-tsk ((obj occ-tsk))
  obj)

(cl-defmethod occ-obj-tsk ((obj occ-ctsk))
  (occ-ctsk-tsk obj))

;; (cl-defmethod occ-obj-tsk ((obj occ-ctxual-tsk))
;;   (occ-ctxual-tsk-tsk occ-obj-tsk obj))


(cl-defmethod occ-obj-marker ((obj marker))
  obj)

(cl-defmethod occ-obj-marker ((obj occ-obj-tsk))
  (occ-tsk-marker (occ-obj-tsk obj)))


(cl-defgeneric occ-fontify-like-in-org-mode (obj)
  "occ-fontify-like-in-org-mode")

(cl-defmethod occ-fontify-like-in-org-mode ((obj marker))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the obj
pointing to it."
  (when (marker-buffer obj)
    (with-current-buffer (org-base-buffer (marker-buffer obj))
      (org-with-wide-buffer
       (progn ;; ignore-errors
         (goto-char obj)
         (let* ((cat         (org-get-category))
                (heading     (org-get-heading 'notags))
                (prefix      (save-excursion
                               (org-back-to-heading t)
                               (looking-at org-outline-regexp)
                               (match-string 0)))
                (org-heading (substring
                              (org-fontify-like-in-org-mode
                               (concat prefix heading)
                               org-odd-levels-only)
                              (length prefix))))

           org-heading))))))

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


(defun occ-case (case title)
  (if (fboundp case)
      (funcall case title)
    title))

(cl-defgeneric occ-title (obj
                          case)
  "occ-format")

(cl-defmethod occ-title (obj
                         case)
  (occ-case case
            (occ-class-name obj)))

(cl-defmethod occ-title ((obj marker)
                         (case symbol))
  (occ-case case
            (occ-class-name obj)))

(cl-defmethod occ-title ((obj occ-obj)
                         (case symbol))
  (occ-case case
            (occ-class-name obj)))


(cl-defgeneric occ-format (obj
                           &optional case)
  "occ-format")

(cl-defmethod occ-format (obj
                          &optional case)
  (concat (when case (concat (occ-title obj case) ": "))
          (format "%s" obj)))

(cl-defmethod occ-format ((obj marker)
                          &optional case)
  (concat (when case (concat (occ-title obj case) ": "))
          (occ-fontify-like-in-org-mode obj)))

(cl-defmethod occ-format ((obj occ-tsk)
                          &optional case)
  (let* ((align      100)
         (heading    (occ-fontify-like-in-org-mode obj))
         (headinglen (length heading))
         (tags       (occ-get-property obj 'tags))
         (tagstr     (if tags (concat ":" (mapconcat #'identity tags ":") ":"))))
    (concat (when case (concat (occ-title obj case) ": "))
            (if tags
                (format
                 (format "%%-%ds         %%s" align (if (< headinglen align) (- align headinglen) 0))
                 heading tagstr)
              (format "%s" heading)))))

(cl-defmethod occ-format ((obj occ-ctx)
                          &optional case)
  (format "%s" obj))

(cl-defmethod occ-format ((obj occ-obj-ctx-tsk)
                          &optional case)
  (let ((tsk (occ-ctsk-tsk obj)))
    (concat (when case (concat (occ-title obj case) ": "))
            (occ-fontify-like-in-org-mode tsk))))

;; (cl-defmethod occ-format ((obj occ-ctsk)
;;                           &optional case)
;;   (let ((tsk (occ-ctsk-tsk obj)))
;;     (concat (when case (concat (occ-title obj case) ": "))
;;             (occ-fontify-like-in-org-mode tsk))))

;; (cl-defmethod occ-format ((obj occ-ctxual-tsk)
;;                           &optional case)
;;   (let ((tsk (occ-ctxual-tsk-tsk obj)))
;;     (concat (when case (concat (occ-title obj case) ": "))
;;             (format "[%4d] %s"
;;                     (occ-ctxual-tsk-rank obj)
;;                     (occ-fontify-like-in-org-mode tsk)))))


(cl-defmethod occ-heading-marker ((obj null))
  (make-marker))

(cl-defmethod occ-heading-marker ((obj marker))
  (save-excursion
    (with-current-buffer (marker-buffer obj)
      (goto-char obj)
      (end-of-line)
      (outline-previous-heading)
      (point-marker))))

(cl-defmethod occ-heading-marker ((obj occ-obj-tsk))
  (occ-heading-marker
   (occ-obj-marker obj)))


(cl-defmethod occ-marker= ((obj marker)
                           (mrk marker))
  (let ((obj-marker (occ-heading-marker obj))
        (mrk-marker (occ-heading-marker mrk)))
    (equal obj-marker mrk-marker)))

(cl-defmethod occ-marker= ((obj occ-obj-tsk)
                           (mrk marker))
  (occ-marker= (occ-obj-marker obj) marker))



;; (cl-defmethod occ-find ((collection occ-collection) (mrk marker)))

(cl-defmethod occ-find ((collection list) (mrk marker)))


(cl-defmethod occ-current-tsk-with ((sym null))
  nil)

(cl-defmethod occ-current-tsk-with ((mrk marker))
  "return created occ-tsk from marker"
  (when (and
         mrk
         (markerp mrk)
         (> (marker-position-nonil mrk) 0))
    (org-with-cloned-marker mrk "<tree>"
      (let ((view-read-only nil)
            (buffer-read-only t))
        (read-only-mode)
        (org-previous-visible-heading 1)
        (let ((tsk (occ-make-tsk
                    (or org-clock-hd-marker mrk)
                    (occ-tsk-builder))))
          tsk)))))

(defun occ-valid-marker (marker)
  (when (and
         marker
         (marker-buffer marker))
    marker))

(defun occ-current-tsk (&optional occ-other-allowed)
  (let ((tsk (car
              *occ-clocked-ctxual-tsk-ctx-history*)))
    (let ((clock-marker    (occ-valid-marker org-clock-marker))
          (clock-hd-marker (occ-valid-marker org-clock-hd-marker)))
      (let ((clock (or clock-marker
                       clock-hd-marker)))
        (if (and tsk
                 clock
                 (occ-marker= tsk clock))
            tsk
          (when clock
            (let ((msg
                   (if tsk
                       (format "occ-current-tsk: %s from head of *occ-clocked-ctxual-tsk-ctx-history* is not equal to current clocking clock %s"
                               (occ-format tsk   'captilize)
                               (occ-format clock 'captilize))
                     (format "occ-current-tsk: %s is outside of occ"
                             (occ-format clock 'captilize)))))
              (if occ-other-allowed
                  (occ-debug :warning msg)
                (error msg))
              (occ-current-tsk-with clock))))))))


(cl-defmethod occ-current-associated-p ((ctx occ-ctx))
  (let ((tsk (occ-current-tsk)))
    (occ-associable-with-p tsk ctx)))


(cl-defmethod occ-associable-with-p ((tsk symbol)
                                     (ctx occ-ctx))
  nil)

(cl-defmethod occ-associable-with-p ((tsk occ-tsk)
                                     (ctx occ-ctx))
  "Test if TSK is associate to CTX"
  (if tsk
      (> (occ-rank tsk ctx) 0)))


(cl-defmethod occ-associable-p ((obj occ-ctsk))
  (occ-debug :debug "occ-associable-p(occ-ctsk=%s)" obj)
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-associable-with-p tsk ctx)))

(cl-defmethod occ-associable-p ((obj occ-ctxual-tsk))
  (occ-debug :debug "occ-associable-p(occ-ctxual-tsk=%s)" obj)
  (> (occ-ctxual-tsk-get-rank obj) 0))


(defvar *occ-clocked-ctxual-tsk-ctx-history* nil)
(defvar occ-clock-in-hooks nil "Hook to run on clockin with previous and next markers.")

(cl-defmethod occ-clock-in ((obj null)
                            &key
                            collector
                            action
                            action-transformer
                            timeout)
  (error "Can not clock in NIL"))

(cl-defmethod occ-clock-in ((obj marker)
                            &key
                            collector
                            action
                            action-transformer
                            timeout)
  (occ-debug :debug "occ-clock-in(marker=%s)" obj)
  (let ((org-log-note-clock-out nil))
    (when (marker-buffer obj)
      (with-current-buffer (marker-buffer obj)
        (let ((buffer-read-only nil))
          (condition-case-control err
            (progn
              (occ-straight-org-clock-clock-in (list obj)))
            ((error)
             (signal (car err) (cdr err)))))))))

(cl-defmethod occ-clock-in ((obj occ-tsk)
                            &key
                            collector
                            action
                            action-transformer
                            timeout)
  (occ-debug :debug "occ-clock-in(occ-tsk=%s)" obj)
  (occ-clock-in (occ-obj-marker obj)))

(cl-defmethod occ-clock-in ((obj occ-ctsk)
                            &key
                            collector
                            action
                            action-transformer
                            timeout)
  (occ-debug :debug "occ-clock-in(occ-ctsk=%s)" obj)
  (if (or
       (occ-unnamed-p obj)
       (occ-associable-p obj))
      (occ-clock-in (occ-ctsk-tsk obj)
                    :collector collector
                    :action    action
                    :action-transformer action-transformer
                    :timeout timeout)
    (occ-debug :debug
               "occ-clock-in(occ-ctxual-tsk): not clocking in (occ-unnamed-p obj)=%s (occ-associable-p obj)=%s"
               (occ-unnamed-p obj)
               (occ-associable-p obj))))

(cl-defmethod occ-clock-in ((obj occ-ctxual-tsk)
                            &key
                            collector
                            action
                            action-transformer
                            timeout)
  ;;TODO add org-insert-log-not
  "return "
  (occ-debug :debug "occ-clock-in(occ-ctxual-tsk=%s)" obj)
  (let* (retval
         (old-ctxual-tsk (car *occ-clocked-ctxual-tsk-ctx-history*))
         (old-tsk        (when old-ctxual-tsk (occ-ctxual-tsk-tsk old-ctxual-tsk)))
         (old-marker     (or (if old-tsk (occ-tsk-marker old-tsk)) org-clock-hd-marker))
         (old-heading    (if old-tsk (occ-tsk-heading old-tsk)))
         (obj-tsk        (occ-ctxual-tsk-tsk obj))
         (obj-ctx        (occ-ctxual-tsk-ctx obj))
         (new-marker     (if obj-tsk (occ-tsk-marker obj-tsk)))
         (new-heading    (if obj-tsk (occ-tsk-heading obj-tsk))))
    (when (and
           new-marker
           (marker-buffer new-marker))

      (let* ((org-log-note-clock-out nil)
             (old-marker             org-clock-marker)
             (old-buff               (marker-buffer old-marker)))

        (occ-debug :debug "clocking in %s" new-marker)

        (let ((old-buff-read-only
               (when old-buff
                 (with-current-buffer (marker-buffer old-marker)
                   buffer-read-only))))

          (when old-buff
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

          ;; (occ-clock-in obj-tsk)
          (if (or
               (occ-unnamed-p obj)
               (occ-associable-p obj))
              (occ-clock-in obj-tsk
                            :collector collector
                            :action    action
                            :action-transformer action-transformer
                            :timeout timeout)
            (occ-debug :debug
                       "occ-clock-in(occ-ctxual-tsk): not clocking in (occ-unnamed-p obj)=%s (occ-associable-p obj)=%s"
                       (occ-unnamed-p obj)
                       (occ-associable-p obj)))

          (setq retval t)

          (push obj *occ-clocked-ctxual-tsk-ctx-history*)

          (when old-buff
            (with-current-buffer old-buff
              (setq buffer-read-only old-buff-read-only)))
          retval)))))

(cl-defmethod occ-clock-in ((obj occ-ctx)
                            &key
                            candidate-transformer
                            occ-select-clock-in-tranform
                            occ-select-clock-in-tranformer-fun-transform
                            collector
                            action
                            action-transformer
                            auto-select-if-only
                            timeout)
  "Clock-in selected CTXUAL-TSK for occ-ctx OBJ or open interface for adding properties to heading."
  (unless collector (error "Collector can not be nil"))
  (occ-debug :debug "occ-clock-in(occ-ctx=%s)" obj)
  (let ((collector          (or collector #'occ-matches))
        (action             (or action (occ-helm-actions obj)))
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))

    (let ((return-ctxual-tsk
             (occ-select obj            ;TODO: if only one match then where it is selecting that.
                         :collector           collector
                         :action              action
                         :action-transformer  action-transformer
                         :auto-select-if-only auto-select-if-only
                         :timeout             timeout)))
      (if (occ-return-operate-p return-ctxual-tsk) ;TODO: should return t if action were done than select[=identity] ;; occ-select-clock-in-label
          (let ((ctxual-tsk (occ-return-get-value return-ctxual-tsk)))
            (prog1
                (make-occ-return :label occ-select-clock-in-true-label :value nil)
              (if (occ-ctxual-tsk-p ctxual-tsk)
                  (occ-clock-in ctxual-tsk
                                :collector          collector
                                :action             action
                                :action-transformer action-transformer
                                :timeout            timeout)
                (occ-message "%s is not ctxual-tsk" (occ-format ctxual-tsk 'capitalize)))))
        (progn
          ;; here create unnamed tsk, no need
          (setq *occ-update-current-ctx-msg* "null clock")
          (occ-debug :debug
                     "No clock found please set a match for this ctx %s, add it using M-x occ-prop-edit-safe."
                     obj)
          (occ-debug :debug
                     "occ-clock-in(ctx):  with this-command=%s" this-command)
          ;; (occ-delayed-select-obj-prop-edit-when-idle obj obj occ-idle-timeout)
          (occ-safe-ignore-quit-props-window-edit obj
                                                  :collector          #'occ-list
                                                  :action             action
                                                  :action-transformer action-transformer
                                                  :timeout            timeout))))))


(cl-defmethod occ-clock-in-if-associable ((tsk occ-tsk)
                                          (ctx occ-ctx)
                                          &key
                                          collector
                                          action
                                          action-transformer
                                          timeout)
  (when (occ-associable-with-p tsk ctx)
    (occ-clock-in tsk
                  :collector collector
                  :action    action
                  :action-transformer action-transformer
                  :timeout timeout)))


(cl-defmethod occ-try-clock-in-with ((tsk occ-tsk)
                                     (ctx occ-ctx))
  (let* ((tries 3)
         (try   tries))
    (while (or
            (> try 0)
            (not (occ-associable-with-p tsk ctx)))
      (setq try (1- try))
      (occ-message "%s is not associable with %s [try %d]"
                   (occ-format tsk 'capitalize)
                   (occ-format ctx 'capitalize)
                   (- tries try))
      (occ-obj-prop-edit tsk ctx occ-idle-timeout)))

  (unless (occ-clock-in-if-associable tsk ctx
                                      :collector collector
                                      :action    action
                                      :action-transformer action-transformer
                                      :timeout timeout)
    (occ-message "%s is not associable with %s not clocking-in."
                 (occ-format tsk 'capitalize)
                 (occ-format ctx 'capitalize))))

(cl-defmethod occ-try-clock-in ((obj marker)
                                &key
                                collector
                                action
                                action-transformer
                                timeout)
  (occ-clock-in obj))

(cl-defmethod occ-try-clock-in ((obj occ-obj-tsk)
                                &key
                                collector
                                action
                                action-transformer
                                timeout)
  (occ-clock-in obj))

(cl-defmethod occ-try-clock-in ((obj occ-ctsk)
                                &key
                                collector
                                action
                                action-transformer
                                timeout)
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-try-clock-in-with tsk ctx)))

(cl-defmethod occ-try-clock-in ((obj null)
                                &key
                                collector
                                action
                                action-transformer
                                timeout)
  (occ-clock-in obj))


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

(cl-defmethod occ-goto ((obj occ-obj-tsk))
  (let ((mrk (occ-obj-marker obj)))
    (if (and
         (markerp mrk)
         (marker-buffer mrk))
        (occ-goto mrk)
      (error "marker %s invalid." mrk))))


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

(cl-defmethod occ-set-to ((obj occ-obj-tsk))
  (let ((mrk (occ-obj-marker obj)))
    (if (and
         (markerp mrk)
         (marker-buffer mrk))
        (occ-set-to mrk)
      (error "marker %s invalid." mrk))))


(defvar occ-capture+-helm-templates-alist org-capture+-helm-templates-alist)

(defun occ-capture+-helm-select-template ()
  (org-capture+-helm-select-template
   nil
   occ-capture+-helm-templates-alist))


(cl-defgeneric occ-capture-with (tsk
                                 ctx
                                 &optional clock-in-p)
  "occ-capture-with")

(cl-defmethod occ-capture-with ((tsk occ-tsk)
                                (ctx occ-ctx)
                                &optional clock-in-p)
  (let* ((mrk      (occ-tsk-marker tsk))
         (template (occ-capture+-helm-select-template)))
    (when template
      (with-org-capture+ marker 'entry `(marker ,mrk) template '(:empty-lines 1)
        (progn
          (occ-obj-prop-edit tsk ctx occ-idle-timeout)
          t)
        (let ((child-tsk (occ-make-tsk marker)))
          (when child-tsk
            (occ-induct-child tsk child-tsk)
            (if clock-in-p
                (occ-try-clock-in-with child-tsk ctx))))))))


(cl-defgeneric occ-capture (obj
                            &optional clock-in-p)
  "occ-capture")

(cl-defmethod occ-capture ((obj marker)
                           &optional clock-in-p)
  (org-capture+
   'entry
   `(marker ,obj)
   'occ-capture+-helm-select-template
   :empty-lines 1))

(cl-defmethod occ-capture ((obj occ-tsk)
                           &optional clock-in-p)
  (let ((mrk (occ-tsk-marker obj)))
    (occ-capture mrk)))

(cl-defmethod occ-capture ((obj occ-ctsk)
                           &optional clock-in-p)
  (let* ((tsk        (occ-ctsk-tsk obj))
         (ctx        (occ-ctsk-ctx obj)))
    (occ-capture-with tsk ctx clock-in-p)))


(cl-defmethod occ-induct-child ((obj occ-tree-tsk)
                                (child occ-tree-tsk))
  ;; BUG: put it to next to correct object obj, so need to find obj task here
  (push child (occ-tree-collection-list (occ-collection-object)))
  (push child (occ-tree-tsk-subtree obj)))

(cl-defmethod occ-induct-child ((obj occ-list-tsk)
                                (child occ-list-tsk))
  (push child (occ-list-collection-list (occ-collection-object))))


(cl-defgeneric occ-unammed-p (obj)
  "occ-unnamed-p")

(cl-defmethod occ-unnamed-p ((obj marker))
  (occ-debug :debug "occ-unnamed-p(marker=%s)" obj)
  (occ-clock-marker-unnamed-p obj))

(cl-defmethod occ-unnamed-p ((obj occ-obj-tsk))
  (occ-debug :debug "occ-unnamed-p(occ-tsk=%s)" obj)
  (occ-unnamed-p (occ-obj-marker obj)))


(cl-defgeneric occ-procreate-child (obj)
  "occ-child")

(cl-defmethod occ-procreate-child ((obj marker))
  (if (not (occ-unnamed-p obj))
      (occ-capture obj helm-current-prefix-arg)
    (let ((title (occ-title obj 'captilize)))
     (error "%s is unnamed %s so can not create child "
           (occ-format obj 'captilize)
           title
           title))))

(cl-defmethod occ-procreate-child ((obj occ-obj-tsk))
  (if (not (occ-unnamed-p obj))
      (occ-capture obj helm-current-prefix-arg)
    (let ((title (occ-title obj 'captilize)))
      (error "%s is unnamed %s so can not create child "
             (occ-format obj 'captilize)
             title
             title))))


(cl-defgeneric occ-procreate-child-clock-in (obj)
  "occ-child-clock-in")

(cl-defmethod occ-procreate-child-clock-in ((obj marker))
  (occ-capture obj t))

(cl-defmethod occ-procreate-child-clock-in ((obj occ-obj-tsk))
  (occ-capture obj t))


;; TODO: remove it.
;; (cl-defgeneric occ-procreate-child-prop-edit-with (obj
;;                                                    ctx)
;;   "occ-child")

;; (cl-defmethod occ-procreate-child-prop-edit-with ((obj marker)
;;                                                   (ctx occ-ctx))
;;   (occ-capture obj)
;;   (occ-obj-prop-edit obj ctx))

;; (cl-defmethod occ-procreate-child-prop-edit-with ((obj occ-tsk)
;;                                                   (ctx occ-ctx))
;;   (occ-capture obj)
;;   (occ-obj-prop-edit obj ctx))

;; (cl-defmethod occ-procreate-child-prop-edit ((obj occ-ctsk))
;;   (occ-capture obj)
;;   (occ-obj-prop-edit (occ-ctsk-tsk obj)
;;                      (occ-ctsk-ctx obj)))

;; (cl-defmethod occ-procreate-child-prop-edit ((obj occ-ctxual-tsk))
;;   (occ-capture obj)
;;   (occ-obj-prop-edit (occ-ctxual-tsk-tsk obj)
;;                      (occ-ctxual-tsk-ctx obj)))


(cl-defgeneric occ-rank (obj
                         ctx)
  "occ-rank")

(cl-defmethod occ-rank (tsk-pair
                        ctx)
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  0)

;; ISSUE? should it return rank or occ-ctxual-tsk
(cl-defmethod occ-rank ((tsk occ-tsk)
                        (ctx occ-ctx))
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk=%s ctx=%s)" tsk ctx)
  (let ((rank
         (reduce #'+
                 (mapcar #'(lambda (slot) ;;TODO: check if method exist or not, or use some default method.
                             (occ-rank (cons slot tsk) ctx))
                         (occ-class-slots tsk)))))
    rank))


(cl-defmethod occ-files ()
  (occ-collect-files
   (occ-collection-object)))


(cl-defgeneric occ-candidate (obj)
  "occ-candidate")

(cl-defmethod occ-candidate ((obj marker))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the obj
pointing to it."
  (cons (occ-format obj) obj))

(cl-defmethod occ-candidate ((obj occ-obj-tsk))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-format obj) obj))


(defvar occ-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; (define-key map (kbd "RET")           'helm-ff-RET)
    (define-key map (kbd "C-]")           'helm-ff-run-toggle-basename)
    (define-key map (kbd "S-RET")         'occ-helm-run-child-clock-in)
    (helm-define-key-with-subkeys map (kbd "DEL") ?\d 'helm-ff-delete-char-backward
                                  '((C-backspace . helm-ff-run-toggle-auto-update)
                                    ([C-c DEL] . helm-ff-run-toggle-auto-update))
                                  nil 'helm-ff-delete-char-backward--exit-fn)
    (when helm-ff-lynx-style-map
      (define-key map (kbd "<left>")      'helm-find-files-up-one-level)
      (define-key map (kbd "<right>")     'helm-execute-persistent-action))
    (delq nil map))
  "Keymap for `helm-find-files'.")

(defvar occ-helm-doc-header " (\\<helm-find-files-map>\\[helm-find-files-up-one-level]: Go up one level)"
  "*The doc that is inserted in the Name header of a find-files or dired source.")

(defun occ-helm-run-child-clock-in ()
  "Run mail attach files command action from `helm-source-find-files'."
  (interactive)                         ;TODO: to move to occ-commands.el
  (with-helm-alive-p
    (helm-exit-and-execute-action 'occ-child-clock-in)))
(put 'occ-helm-run-child-clock-in 'helm-only t)
;; add occ-child-clock-in in action


(cl-defun occ-list-select-internal (candidates
                                    &key
                                    action
                                    action-transformer
                                    auto-select-if-only
                                    timeout)
  ;; (occ-debug :debug "sacha marker %s" (car dyntskpls))
  (occ-debug :debug "Running occ-sacha-helm-select")
  (prog1
      (let ((action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
            (timeout            (or timeout occ-idle-timeout)))
        (if (and
             auto-select-if-only
             (= 1 (length candidates)))
            (let* ((candidate (car candidates))
                   (action (car (funcall action-transformer action candidate)))
                   (action (if (consp action) (cdr action) action)))
              (funcall action candidate))
            (helm
             ;; :keymap occ-helm-map
             (occ-helm-build-candidates-source
              candidates
              :action action
              :action-transformer action-transformer))))
    (occ-debug :debug "Running occ-sacha-helm-select1")))

(cl-defun occ-list-select (candidates
                           &key
                           action
                           action-transformer
                           auto-select-if-only
                           timeout)
  (let (;; (action             (or action (occ-helm-actions obj)))
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
    (helm-timed timeout
      (occ-debug :debug "running sacha/helm-select-clock")
      (occ-list-select-internal candidates
                                :action              action
                                :action-transformer  action-transformer
                                :auto-select-if-only auto-select-if-only
                                :timeout             timeout))))


(cl-defmethod occ-collection-obj-matches ((collection occ-list-collection)
                                          (obj null))
  "Return all TSKs for context nil OBJ"
  ;; (occ-debug :debug "occ-collection-obj-matches list")
  (occ-collection-list collection))

;; ISSUE? should it return rank or occ-ctxual-tsks list
(cl-defmethod occ-collection-obj-matches ((collection occ-list-collection)
                                          (obj occ-ctx))
  "Return matched CTXUAL-TSKs for context CTX"
  ;; (occ-debug :debug "occ-collection-obj-matches list")
  (let ((tsks (occ-collection collection))
        (obj obj))
    (remove-if-not
     #'(lambda (ctxual-tsk)
         (> (occ-ctxual-tsk-rank ctxual-tsk) 0))
     (mapcar #'(lambda (tsk)
                 (occ-build-ctxual-tsk tsk obj))
             tsks))))

;; ISSUE? should it return rank or occ-ctxual-tsks map
(cl-defmethod occ-collection-obj-matches ((collection occ-tree-collection)
                                          (obj occ-ctx))
  ;; (occ-debug :debug "occ-collection-obj-matches tree")
  "Return matched CTXUAL-TSKs for context CTX"
  (let ((tsks (occ-collection collection))
        (matched '()))
    (when tsks
      (occ-debug :debug "occ-collection-obj-matches BEFORE matched %s[%d]" matched (length matched))
      (occ-mapc-tree-tsks
       #'(lambda (tsk args)
           ;; (occ-debug :debug "occ-rank heading = %s" (occ-tsk-heading tsk))
           (let* ((ctxual-tsk (occ-build-ctxual-tsk tsk args))
                  (rank       (occ-ctxual-tsk-rank ctxual-tsk)))
             (unless rank (error "occ-collection-obj-matches[lambda]: rank is null"))
             (when (> (occ-ctxual-tsk-rank ctxual-tsk) 0)
               (push ctxual-tsk matched)
               (occ-debug :debug "occ-collection-obj-matches[lambda]: tsk %s MATCHED RANK %d"
                          (occ-tsk-heading tsk)
                          (length matched)))))
       tsks
       obj))
    (occ-debug :debug "occ-collection-obj-matches: AFTER matched %s[%d]" "matched" (length matched))
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
        ;; (occ-debug :debug "occ-collection-obj-matches :around finish")
        (occ-debug :debug "matched ctxtsks %s" (length ctxual-tsks))
        (remove-if-not
         #'(lambda (ctxual-tsk)
             (>= (occ-ctxual-tsk-rank ctxual-tsk) avgrank))
         ctxual-tsks))
    (error "(occ-collection-object) returned nil")))


(cl-defmethod occ-matches ((obj null))
  "return TSKs matches"
  (let ((collection (occ-collection-object)))
    (occ-collection-obj-matches collection
                                obj)))

(cl-defmethod occ-matches ((obj occ-ctx))
  "return CTXUAL-TSKs matches"
  (let ((collection (occ-collection-object)))
    (occ-collection-obj-matches collection
                                obj)))


;; (cl-defmethod occ-collection-obj-list ((collection occ-collection)
;;                                        (obj occ-ctx))
;;   "return CTXUAL-TSKs list"
;;   (occ-collection-obj-matches collection obj))

;; (cl-defmethod occ-collection-obj-list ((collection occ-collection)
;;                                        (obj null))
;;   "return TSKs list"
;;   (occ-collect-list collection))

;; (cl-defmethod occ-collection-obj-list ((collection occ-collection)
;;                                        (obj null))
;;   "return TSKs list"
;;   ;; (occ-make-tsk-container
;;   ;;  (occ-collect-list collection))
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
  (occ-collection-obj-list collection (occ-make-ctx-at-point)))


;; http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

(cl-defgeneric occ-list (obj)
  "occ-list")

;; (cl-defmethod occ-list ((obj null))
;;   "return TSKs container"
;;   (occ-collection-obj-list (occ-collection-object)
;;                            obj))

(cl-defmethod occ-list ((obj occ-ctx))
  "return CTXUAL-TSKs container"
  (occ-collection-obj-list (occ-collection-object)
                           obj))

(cl-defmethod occ-list ((obj null))
  "return TSKs container"
  (occ-collection-obj-list (occ-collection-object)
                           (occ-make-ctx-at-point)))


;; TODO: Not to run when frame is not open [visible.]
;; Getting targets...done
;; Error running timer: (error "Window #<window 12> too small for splitting")
;; task-projbuffs-base-dir: changing supplied base-dir nil and task-projbuffs-base-dir to /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/
;; in occ-clock-in occ-ctx 1
;; Getting targets...done
;; Error running timer ‘occ-clock-in-curr-ctx-if-not’: (error "Window #<window 12> too small for splitting")

;; (cl-defmethod occ-select ((obj null)
;;                           &key
;;                           collector
;;                           action
;;                           action-transformer
;;                           timeout)
;;   "return interactively selected TSK or NIL"
;;   (unless collector (error "Collector can not be nil"))
;;   (let ((candidates         (funcall collector obj))
;;         (action             (or action (occ-helm-actions obj)))
;;         (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
;;         (timeout            (or timeout occ-idle-timeout)))
;;     (when candidates
;;       (occ-list-select candidates
;;                        :action action
;;                        :action-transformer action-transformer
;;                        :timeout timeout))))

(cl-defmethod occ-select ((obj occ-ctx)
                          &key
                          collector
                          action
                          action-transformer
                          auto-select-if-only
                          timeout)
  "return interactively selected TSK or NIL"
  (unless collector (error "Collector can not be nil"))
  (let ((action             (or action (occ-helm-actions obj)))
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
    (let ((candidates (funcall collector obj)))
      (when candidates
        (occ-list-select candidates
                         :action              action
                         :action-transformer  action-transformer
                         :auto-select-if-only auto-select-if-only
                         :timeout             timeout)))))

(cl-defmethod occ-select ((obj null)
                          &key
                          collector
                          action
                          action-transformer
                          auto-select-if-only
                          timeout)
  (occ-select obj
              :collector           collector
              :action              action
              :action-transformer  action-transformer
              :auto-select-if-only auto-select-if-only
              :timeout             timeout))


(defcustom *occ-last-buff-sel-time*            (current-time) "*occ-last-buff-sel-time*")
(defvar    *occ-buff-sel-timer*                nil)
(defvar    *occ-tsk-current-ctx-time-interval* 7)
(defvar    *occ-tsk-previous-ctx*              nil)
(defvar    *occ-tsk-current-ctx*               nil)

(cl-defmethod occ-clock-in-if-not ((ctx occ-ctx)
                                   &key
                                   collector
                                   action
                                   action-transformer
                                   auto-select-if-only
                                   timeout)
  (unless collector (error "Collector can not be nil"))
  (let ((collector          (or collector #'occ-matches))
        ;; (candidates         (funcall collector ctx))
        (action             (or action (occ-helm-actions ctx)))
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
    (if (or
         (occ-clock-marker-unnamed-clock-p)
         (not (occ-associable-with-p (occ-current-tsk)
                                     ctx)))
        (prog1                ;current clock is not matching
            t
          (occ-debug :debug
                     "occ-clock-in-if-not: Now really going to clock with this-command=%s"
                     this-command)
          (let ((retval (occ-clock-in ctx
                                      :collector           collector
                                      :action              action
                                      :action-transformer  action-transformer
                                      :auto-select-if-only auto-select-if-only
                                      :timeout             timeout)))
            (when (occ-return-operate-p retval)
              (unless (occ-return-get-value retval)
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
                (occ-maybe-create-clockedin-unnamed-ctxual-tsk ctx))))
          (occ-debug :debug "occ-clock-in-if-not: Now really clock done."))
      (prog1
          nil
        (occ-debug :debug
                   "occ-clock-in-if-not: Current tsk already associate to %s"
                   (occ-format ctx 'captilize))))))

(defvar occ-clock-in-ctx-auto-select-if-only t)

(cl-defmethod occ-clock-in-if-chg ((ctx occ-ctx)
                                   &key
                                   collector
                                   action
                                   action-transformer
                                   auto-select-if-only
                                   timeout)
  (let* ((collector          (or collector #'occ-matches))
         ;; (candidates         (funcall collector ctx))
         (action             (or action (occ-helm-actions ctx)))
         (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
         (timeout            (or timeout occ-idle-timeout)))
    (if (>
         (float-time (time-since *occ-last-buff-sel-time*))
         *occ-tsk-current-ctx-time-interval*)
        (let* ((buff (occ-ctx-buffer ctx)))
          (setq *occ-tsk-current-ctx* ctx)
          (if (and
               (occ-chgable-p)
               buff (buffer-live-p buff)
               (not (minibufferp buff))
               (not (ignore-p buff))
               (not              ;BUG: Reconsider whether it is catching case after some delay.
                (equal *occ-tsk-previous-ctx* *occ-tsk-current-ctx*)))
              (when (occ-clock-in-if-not ctx
                                         :collector           #'occ-matches
                                         :action              action
                                         :action-transformer  action-transformer
                                         :auto-select-if-only auto-select-if-only
                                         :timeout             timeout)
                (setq *occ-tsk-previous-ctx* *occ-tsk-current-ctx*))
            (occ-debug :nodisplay "occ-clock-in-if-chg: ctx %s not suitable to associate" ctx)))
      (occ-debug :nodisplay "occ-clock-in-if-chg: not enough time passed."))))


;;; occ-obj-simple.el ends here
