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

(provide 'occ-obj-method)


(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))

(require 'occ-cl-utils)
(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-prop)
(require 'occ-obj-simple)


;; ISSUE? should it return rank or occ-ctxual-tsks list
(cl-defmethod occ-matching-ctxual-tsks ((collection occ-list-tsk-collection)
                                        (ctx occ-ctx))
  ;; (message "occ-matching-ctxual-tsks list")
  (let ((tsks (occ-collection collection))
        (ctx  ctx))
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
(cl-defmethod occ-matching-ctxual-tsks :around

  ((collection occ-tsk-collection)
   (ctx occ-ctx)) ;TODO: make it after method

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


(cl-defmethod occ-list ((collection occ-tree-tsk-collection)
                        (ctx occ-ctx))
  "return CTXUAL-TSKs list"
  (occ-matching-ctxual-tsks collection ctx))

(cl-defmethod occ-list ((collection occ-tree-tsk-collection)
                        (ctx null))
  "return TSKs list"
  (occ-collect-tsk-list collection))

;; (occ-list (occ-collection-object) nil)

(defun occ-sacha-helm-select (ctxasks)
  ;; (occ-debug :debug "sacha marker %s" (car dyntskpls))
  (message "Running occ-sacha-helm-select")
  (helm
   (list
    (helm-build-sync-source "Select matching tsks"
      :candidates (mapcar 'occ-sacha-selection-line ctxasks)
      :action (list ;; (cons "Select" 'identity)
               (cons "Clock in and track" #'identity))
      :history 'org-refile-history))))
    ;; (helm-build-dummy-source "Create tsk"
    ;;   :action (helm-make-actions
    ;;            "Create tsk"
    ;;            'sacha/helm-org-create-tsk))


(defun occ-sacha-helm-select-timed (ctxasks)
  (helm-timed 7
    (message "running sacha/helm-select-clock")
    (occ-sacha-helm-select ctxasks)))

(cl-defgeneric occ-sacha-helm-action (ctxask clockin-fn)
  "occ-sacha-helm-action")

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
        :history 'org-refile-history)))))
      ;; (helm-build-dummy-source "Create tsk"
      ;;   :action (helm-make-actions
      ;;            "Create tsk"
      ;;            'sacha/helm-org-create-tsk))


;;;###autoload
(defun occ-helm-select-tsk (selector
                            action)
  ;; here
  ;; (occ-debug :debug "sacha marker %s" (car tsks))
  (let ()
    (let ((tsks
           (occ-list (occ-collection-object) nil)))
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
           (occ-list (occ-collection-object) ctx)))
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


;;; occ-obj-method.el ends here
