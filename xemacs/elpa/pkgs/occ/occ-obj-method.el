;;; occ-obj-method.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  Sharad Pratap

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

(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)

(when nil ;; https://curiousprogrammer.wordpress.com/2010/07/19/emacs-defstruct-vs-other-languages/

  (defun cl-get-field (object field)
    (cl-struct-slot-value (cl-classname object) field object))

  (defun cl-set-field (object field value)
    (setf (cl-struct-slot-value (cl-classname object) field object) value))

  (get-field dave 'name)
  (set-field dave 'name "Simon Smith"))

(cl-defmethod occ-get-property ((obj occ-obj)
                                prop)
  ;; mainly used by occ-tsk only.
  (if (memq prop (cl-class-slots (cl-classname obj)))
      (cl-get-field obj prop)
    (plist-get
     (cl-struct-slot-value (cl-classname obj) 'plist obj)
     (sym2key prop))))
(cl-defmethod occ-set-property ((obj occ-obj)
                                prop
                                val)
  ;; mainly used by occ-tsk only
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
(cl-defmethod occ-obj-defined-slots ((obj occ-obj))
  (let* ((plist (cl-struct-slot-value (cl-classname obj) 'plist obj))
         (plist-keys (plist-get-keys plist))
         (slots
          (append
           (cl-class-slots (cl-classname obj))
           (mapcar #'key2sym plist-keys))))
    (remove-if-not
     #'(lambda (slot)
         (cl-struct-slot-value (cl-classname object) slot object))
     slots)))
(cl-defmethod cl-method-matched-arg ((method symbol) (ctx symbol))
  (cl-method-first-arg method))
(cl-defmethod cl-method-matched-arg ((method symbol) (ctx occ-ctx))
  (let ((slots (occ-obj-defined-slots ctx)))
    (remove-if-not
     #'(lambda (arg) (memq arg slots))
     (cl-method-first-arg method))))

(defun occ-tsk-builder ()
  (let ((classname (cl-classname occ-global-tsk-collection)))
    (cond
      ((eq 'occ-list-tsk-collection classname)
       #'make-occ-list-tsk)
      ((eq 'occ-tree-tsk-collection classname)
       #'make-occ-tree-tsk)
      (t (error "occ-global-tsk-collection is not from occ-list-tsk-collection or occ-tree-tsk-collection class")))))

(cl-defmethod occ-fontify-like-in-org-mode ((tsk occ-tsk))
  (let* ((level   (or (occ-get-property tsk 'level) 0))
         (heading (occ-get-property tsk 'heading-prop))
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

(cl-defmethod occ-ctxual-tsk-marker ((ctxask occ-ctxual-tsk))
  (let* ((tsk    (occ-ctxual-tsk-tsk ctxask))
         (marker (occ-tsk-marker tsk)))
    marker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Test if TSK is associate to CTX

(cl-defmethod occ-associated-p ((tsk symbol)
                                (ctx occ-ctx))
  0)

(cl-defmethod occ-associated-p ((tsk occ-tsk)
                                (ctx occ-ctx))
  (if tsk
      (occ-rank tsk ctx)
    0))
;; Test if TSK is associate to CTX:1 ends here

;; Collect and return tsk matching to CTX
;;---------------------------------------------------------------;;;###autoload
(cl-defmethod occ-current-associated-p ((ctx occ-ctx))
  (let ((tsk (occ-tsk-current-tsk)))
    (when tsk (occ-rank tsk ctx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defmethod occ-print ((tsk occ-tsk))
  ;; (format "[%4d] %s"
  ;;         0
  ;;         (occ-fontify-like-in-org-mode tsk))
  (format "%s"
          (occ-fontify-like-in-org-mode tsk)))

(cl-defmethod occ-print ((ctxask occ-ctxual-tsk))
  (let ((tsk (occ-ctxual-tsk-tsk ctxask)))
    (format "[%4d] %s"
            (occ-ctxual-tsk-rank ctxask)
            (occ-fontify-like-in-org-mode tsk))))

(cl-defgeneric occ-sacha-selection-line (obj)
  )

(cl-defmethod occ-sacha-selection-line ((mrk marker))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the mrk
pointing to it."
  (when (mrk-buffer mrk)
    (with-current-buffer (org-base-buffer (mrk-buffer mrk))
      (org-with-wide-buffer
       (progn ;; ignore-errors
         (goto-char mrk)
         (let* ((cat (org-get-category))
                (heading (org-get-heading 'notags))
                (prefix (save-excursion
                          (org-back-to-heading t)
                          (looking-at org-outline-regexp)
                          (match-string 0)))
                (tsk (substring
                      (org-fontify-like-in-org-mode
                       (concat prefix heading)
                       org-odd-levels-only)
                      (length prefix))))
           (when tsk ;; (and cat tsk)
             ;; (insert (format "[%c] %-12s  %s\n" i cat tsk))
             ;; mrk
             (cons tsk mrk))))))))

;; deprecated
;; (cl-defmethod occ-sacha-selection-line ((ctxask occ-ctxual-tsk))
;;   "Insert a line for the clock selection menu.
;; And return a cons cell with the selection character integer and the marker
;; pointing to it."
;;   (let ((marker (occ-ctxual-tsk-marker ctxask))
;;         (rank   (occ-ctxual-tsk-rank   ctxask)))
;;     (when (marker-buffer marker)
;;       (with-current-buffer (org-base-buffer (marker-buffer marker))
;;         (org-with-wide-buffer
;;          (progn ;; ignore-errors
;;            (goto-char marker)
;;            (let* ((cat (org-get-category))
;;                   (heading (org-get-heading 'notags))
;;                   (prefix (save-excursion
;;                             (org-back-to-heading t)
;;                             (looking-at org-outline-regexp)
;;                             (match-string 0)))
;;                   (tsk (substring
;;                         (org-fontify-like-in-org-mode
;;                          (concat prefix heading)
;;                          org-odd-levels-only)
;;                         (length prefix))))
;;              (when tsk ;; (and cat tsk)
;;                ;; (insert (format "[%c] %-12s  %s\n" i cat tsk))
;;                ;; marker
;;                (cons (occ-print ctxask) ctxask))))))))) ;TODO


(cl-defmethod occ-sacha-selection-line ((tsk occ-tsk))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-print tsk) tsk))

(cl-defmethod occ-sacha-selection-line ((ctxask occ-ctxual-tsk))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-print ctxask) ctxask))
;; function to setup ctx clock timer:2 ends here

(cl-defgeneric occ-goto (obj)
  )

(cl-defmethod occ-goto ((mrk marker))
  (progn
    (switch-to-buffer (marker-buffer marker))
    ;; TODO find about "org-overview"
    ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
    ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
    ;; https://github.com/facetframer/orgnav
    ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
    ;; (outline-show-all)
    (org-content 10)
    (goto-char marker)))

(cl-defmethod occ-goto ((tsk occ-tsk))
  (let ((marker (occ-tsk-marker tsk)))
    (if (and
         (markerp marker)
         (marker-buffer marker))
        (occ-goto marker)
      (error "marker %s invalid." marker))))

(cl-defmethod occ-goto (ctxask occ-ctxual-tsk)
  (occ-goto (occ-ctxual-tsk-marker)))

(cl-defmethod occ-set-to ((mrk marker))
  (progn
    (set-buffer (marker-buffer marker))
    ;; TODO find about "org-overview"
    ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
    ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
    ;; https://github.com/facetframer/orgnav
    ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
    ;; (outline-show-all)
    ;; (org-content 10)
    (goto-char marker)))

(cl-defmethod occ-set-to ((tsk occ-tsk))
  (let ((marker (occ-tsk-marker tsk)))
    (if (and
         (markerp marker)
         (marker-buffer marker))
        (occ-set-to marker)
      (error "marker %s invalid." marker))))

(cl-defmethod occ-set-to (ctxask occ-ctxual-tsk)
  (occ-set-to (occ-ctxual-tsk-marker)))


;; rank based
(defun occ-sacha-helm-select (ctxasks)
  ;; (occ-debug :debug "sacha marker %s" (car dyntskpls))
  (message "Running occ-sacha-helm-select")
  (helm
   (list
    (helm-build-sync-source "Select matching tsks"
      :candidates (mapcar 'occ-sacha-selection-line ctxasks)
      :action (list ;; (cons "Select" 'identity)
               (cons "Clock in and track" #'identity))
      :history 'org-refile-history)
    ;; (helm-build-dummy-source "Create tsk"
    ;;   :action (helm-make-actions
    ;;            "Create tsk"
    ;;            'sacha/helm-org-create-tsk))
    )))

(defun occ-sacha-helm-select-timed (ctxasks)
  (helm-timed 7
    (message "running sacha/helm-select-clock")
    (occ-sacha-helm-select ctxasks)))

(cl-defgeneric occ-sacha-helm-action (ctxask clockin-fn)
  )

(cl-defmethod occ-sacha-helm-action ((ctxask occ-ctxual-tsk) clockin-fn)
  ;; (message "sacha marker %s" (car dyntskpls))
  ;; (setq sacha/helm-org-refile-locations tbl)
  (progn
    (helm
     (list
      (helm-build-sync-source "Select matching tsks"
        :candidates (mapcar 'occ-sacha-selection-line ctxask)
        :action (list ;; (cons "Select" 'identity)
                 (cons "Clock in and track" #'(lambda (c) (funcall clockin-fn c))))
        :history 'org-refile-history)
      ;; (helm-build-dummy-source "Create tsk"
      ;;   :action (helm-make-actions
      ;;            "Create tsk"
      ;;            'sacha/helm-org-create-tsk))
      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl-defgeneric occ-rank (obj ctx)
  "occ-rank"
  )

(cl-defmethod occ-rank (tsk-pair ctx)
  0)

(cl-defmethod occ-rank ((tsk-pair (head root))
                        (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((root
          (occ-get-property (cdr tsk-pair) 'root))
         (root (if root (file-truename root))))
    (let* ((file (occ-ctx-file ctx))
           (file (if file (file-truename file))))
      (if root
          (progn
            (occ-debug :debug "tsk %s root %s" (occ-tsk-heading (cdr tsk-pair)) root)
            (occ-debug :debug "tsk %s file %s" (occ-tsk-heading (cdr tsk-pair)) file))
        (occ-debug :debug "tsk %s root %s not present."
                   (occ-tsk-heading (cdr tsk-pair)) root))
      (if (and root file
               (string-match root file))
          (length root)
        0))))

(cl-defmethod occ-rank ((tsk-pair (head currfile))
                        (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((currfile
          (occ-get-property (cdr tsk-pair) 'currfile))
         (currfile (if currfile (file-truename currfile))))
    (let* ((file (occ-ctx-file ctx))
           (file (if file (file-truename file))))
      (if currfile
          (progn
            (occ-debug :debug "tsk %s currfile %s" (occ-tsk-heading (cdr tsk-pair)) currfile)
            (occ-debug :debug "tsk %s file %s"     (occ-tsk-heading (cdr tsk-pair)) file))
        (occ-debug :debug "tsk %s currfile %s not present."
                   (occ-tsk-heading (cdr tsk-pair)) currfile))
      (if (and currfile file
               (string-match currfile file))
          (* 2 (length currfile))     ;as exact match to file giving double matching points.
        0))))

(cl-defmethod occ-rank ((tsk-pair (head status))
                        (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's status attribute."
  (let ((todo-type
         (occ-get-property (cdr tsk-pair) 'todo-type))
        (closed
         (occ-get-property (cdr tsk-pair) 'closed))
        (status
         (occ-get-property (cdr tsk-pair) 'todo-keyword)))
    (if (or
         closed
         (eql todo-type 'done)
         (string-equal status "HOLD"))
        -30 0)))

(cl-defmethod occ-rank ((tsk-pair (head subtree))
                        (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's status attribute."
  (let ((sub-tree
         (occ-get-property (cdr tsk-pair) 'subtree)))
    (occ-debug :debug "tsk %s subtree %s" (occ-tsk-heading (cdr tsk-pair)) (null (null sub-tree)))
    (if sub-tree -30 0)))

(cl-defmethod occ-rank ((tsk-pair (head key))
                        (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((key (occ-get-property (cdr tsk-pair) 'KEY)))
    (if key (string-to-number key) 0)))

(cl-defmethod occ-rank ((tsk-pair (head heading-level))
                        (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((level
          (occ-get-property (cdr tsk-pair) 'level)))
    (if level level 0)))

(cl-defmethod occ-rank ((tsk-pair (head timebeing))
                        (ctx occ-ctx))
  (let ((timebeing (occ-get-property (cdr tsk-pair) 'timebeing)))
    (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
          (clocked-time   (occ-get-property (cdr tsk-pair) 'clock-sum)))
      (if (and
           (numberp clocked-time)
           (numberp timebeing-time)
           (> timebeing-time clocked-time))
          (- timebeing-time clocked-time)
        0))))

(cl-defmethod occ-rank ((tsk-pair (head current-clock))
                        (ctx occ-ctx))
  (let* ((tsk-marker
          (occ-get-property (cdr tsk-pair) 'marker)))
    (if (and
         (markerp org-clock-hd-marker)
         (markerp tsk-marker)
         (equal org-clock-hd-marker org-clock-hd-marker))
        100
      0)))

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

(cl-defmethod occ-build-ctxual-tsk ((tsk occ-tsk) ;ctor
                                    (ctx occ-ctx))
  (occ-make-ctxual-tsk tsk
                       ctx
                       (occ-rank tsk ctx)))

(cl-defmethod occ-included-files ()
  (occ-collection-included-files (occ-collection-object)))

;; (occ-tree-tsk-collection-included-files (occ-collection-object))

(occ-tree-tsk-collection-included-files (make-occ-tree-tsk-collection))

;; ISSUE? should it return rank or occ-ctxual-tsks list
(cl-defmethod occ-matching-ctxual-tsks ((collection occ-list-tsk-collection)
                                        (ctx occ-ctx))
  ;; (message "occ-matching-ctxual-tsks list")
  (lexical-let ((tsks (occ-collection collection))
                (ctx ctx))
    (remove-if-not
     #'(lambda (ctxual-tsk)
         (> (occ-ctxual-tsk-rank ctxual-tsk) 0))
     (mapcar
      #'(lambda (tsk)
          (occ-build-ctxual-tsk tsk ctx))
      tsks))))

;; ISSUE? should it return rank or occ-ctxual-tsks map
(cl-defmethod occ-matching-ctxual-tsks ((collection occ-tree-tsk-collection)
                                        (ctx occ-ctx))
  ;; (message "occ-matching-ctxual-tsks tree")
  (let ((tsks (occ-collection collection))
        (matched '()))
    (when tsks
      (occ-debug :debug "occ-matching-ctxual-tsks BEFORE matched %s[%d]" matched (length matched))
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
       ctx))
    (occ-debug :debug "occ-entries-associated-to-ctx-by-keys: AFTER matched %s[%d]" "matched" (length matched))
    matched))

;;TODO: make it after method
(cl-defmethod occ-matching-ctxual-tsks :around ((collection occ-tsk-collection)
                                                (ctx occ-ctx)) ;TODO: make it after method
  ;; TODO Here do variance based filtering.
  ;; (message "occ-matching-ctxual-tsks :around start")
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
        ;; (message "occ-matching-ctxual-tsks :around finish")
        (occ-debug :debug "matched ctxtsks %s" (length ctxual-tsks))
        (remove-if-not
         #'(lambda (ctxual-tsk)
             (>= (occ-ctxual-tsk-rank ctxual-tsk) avgrank))
         ctxual-tsks))
    (error "(occ-collection-object) returned nil")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmethod occ-readprop ((tsk-pair (head root))
                            (ctx occ-ctx))
  (let* ((file (if ctx (occ-ctx-file ctx)))
         (dir (if (stringp file) (file-name-directory file) (dirname-of-file file)))
         (prompt (concat (symbol-name (car tsk-pair)) ": ")))
    (ido-read-directory-name prompt dir dir)))

(cl-defmethod occ-readprop ((tsk-pair (head subtree))
                            (ctx occ-ctx))
  (let ((prompt (concat (symbol-name (car tsk-pair)) ": ")))
    (file-relative-name
     (ido-read-file-name ;; org-iread-file-name
      prompt
      default-directory default-directory))))

(cl-defmethod occ-writeprop ((tsk-pair (head subtree)))
  )

(when nil

  (cl-defmethod occ-rank (tsk-pair ctx)
    0)

  (cl-defmethod occ-rank ((tsk-pair (head root)) (ctx list))
    (message "%s" tsk-pair))

  (occ-rank '(root  1) nil)

  (occ-rank '(n  1) nil)

  (cl-defmethod occ-rank ((tsk occ-tsk)
                          (ctx occ-ctx))
    (message "match occ-rank"))

  (occ-rank (make-occ-tree-tsk) (make-occ-ctx))
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *occ-clocked-ctxual-tsk-ctx-history* nil)
(defvar occ-clock-in-hooks nil "Hook to run on clockin with previous and next markers.")

(cl-defmethod occ-clock-in ((new-ctxask occ-ctxual-tsk))
              ;;TODO add org-insert-log-not
              (occ-debug :debug "occ-clock-in-marker %s" new-ctxask)
              (let* (retval
                     (old-ctxual-tsk     (car *occ-clocked-ctxual-tsk-ctx-history*))
                     (old-tsk            (when old-ctxual-tsk (occ-ctxual-tsk-tsk old-ctxual-tsk)))
                     (old-marker         (or (if old-tsk (occ-tsk-marker old-tsk)) org-clock-hd-marker))
                     (old-heading        (if old-tsk (occ-tsk-heading old-tsk)))
                     (new-tsk            (occ-ctxual-tsk-tsk new-ctxask))
                     (new-marker         (if new-tsk (occ-tsk-marker new-tsk)))
                     (new-heading        (if new-tsk (occ-tsk-heading new-tsk))))
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

                      (with-current-buffer (marker-buffer new-marker)
                        (let ((buffer-read-only nil))
                          (when old-heading
                            (org-insert-log-note new-marker (format "clocking in to here from last clock <%s>" old-heading)))
                          (condition-case err
                                          (progn
                                            (org-clock-clock-in (list new-marker))
                                            (setq retval t)
                                            (push new-ctxask *occ-clocked-ctxual-tsk-ctx-history*))
                                          ((error)
                                           (progn
                                             (setq retval nil)
                                             (signal (car err) (cdr err)))))))
                      (if old-buff
                          (with-current-buffer old-buff
                            (setq buffer-read-only old-buff-read-only)))
                      retval)))))

(cl-defmethod occ-clock-in ((ctx occ-ctx))
  "marker and ranked version"
  (interactive
   (list (occ-make-ctx)))
  (run-unobtrusively
   (let* ((ctx (or ctx (occ-make-ctx)))
          (matched-ctxual-tsks
           (run-unobtrusively           ;heavy task
            (remove-if-not
             #'(lambda (ctxual-tsk)
                 (let* ((marker (occ-ctxual-tsk-marker ctxual-tsk)))
                   (and
                    marker
                    (marker-buffer marker))))
             (occ-matching-ctxual-tsks (occ-collection-object) ctx)))))
     (unless (eq matched-ctxual-tsks t)
       (if matched-ctxual-tsks
           (let* ((sel-ctxual-tsk
                   (if (> (length matched-ctxual-tsks) 1)
                       (occ-sacha-helm-select-timed matched-ctxual-tsks)
                     (car matched-ctxual-tsks)))
                  ;; (sel-tsk   (if sel-ctxual-tsk (plist-get sel-ctxual-tsk :tsk)))
                  ;; (sel-marker (if sel-tsk      (plist-get sel-tsk      :tsk-clock-marker)))
                  )
             ;; (occ-debug 6 "sel-ctxual-tsk %s sel-tsk %s sel-marker %s" sel-ctxual-tsk sel-tsk sel-marker)
             (if sel-ctxual-tsk (occ-clock-in sel-ctxual-tsk)))
         (progn
           ;; here create unnamed tsk, no need
           (setq *occ-update-current-ctx-msg* "null clock")
           (occ-debug 6
                      "No clock found please set a match for this ctx %s, add it using M-x occ-add-to-org-heading."
                      ctx)
           (occ-add-to-org-heading-when-idle ctx 7)
           nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when nil

  (occ-add-to-org-heading-when-idle (occ-make-ctx) 7)

  (length
   (occ-matching-ctxual-tsks
    (occ-collection-object)
    (occ-make-ctx
     (find-file-noselect "/home/s/paradise/git/main/src/wnc/security/authenticator/accounting.cpp"))))

  (occ-ctxual-tsk-tsk
   (car
    (occ-matching-ctxual-tsks
     (occ-collection-object)
     (occ-make-ctx
      (find-file-noselect "/home/s/paradise/git/main/src/wnc/security/authenticator/accounting.cpp")))))

  (length
   (occ-matching-ctxual-tsks
    (occ-collection-object)
    (occ-make-ctx (current-buffer)))))

(provide 'occ-obj-method)
;;; occ-obj-method.el ends here
