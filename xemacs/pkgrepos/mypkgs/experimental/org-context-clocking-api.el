;;; org-context-clocking-api.el --- org-context-clocking-api               -*- lexical-binding: t; -*-

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


(require 'org-clock)





;; (org-clocking-api-entries-associated-to-file-p file)
;; (org-clocking-api-entry-associated-to-file-p task-info file)
;; (org-clocking-api-entry-update-task-infos &optional force)

(progn ;; "org-entry-clocking-api interface"

  (defvar org-entry-clocking-api nil)

  (defun org-entry-clocking-api-set (name api fn)
    (let ((pl (plist-get org-entry-clocking-api name)))
      ;; (message "org-entry-clocking-api: %s, pl: %s " org-entry-clocking-api pl)
      (setq org-entry-clocking-api
            (plist-put
             org-entry-clocking-api
             name
             (plist-put pl api fn)))))
  (defun org-entry-clocking-api-get (name api)
    (plist-get
     (plist-get org-entry-clocking-api name)
     api)))

(progn ;; "org entries clocking's APIs' API"

  (progn ;; "org entries accss common api"
    ;; (defvar org-)

    (defun org-entry-collect-task-info ()
      ;; (org-element-at-point)
      (let ((heading-with-string-prop
             (unless (org-before-first-heading-p)
               (org-get-heading))))
        (let ((heading (if heading-with-string-prop
                           (substring-no-properties heading-with-string-prop)))
              (marker  (move-marker
                        (make-marker)
                        (point)
                        (org-base-buffer (current-buffer))))
              (file    (buffer-file-name))
              (point   (point))
              (clock-sum (if (org-before-first-heading-p)
                             0
                             (org-clock-sum-current-item)))
              (task-info (cadr (org-element-at-point))))
          (when heading
            ;; (if root   (push (cons "Root" root) task-info))
            (if marker    (org-entry-task-info-set-property task-info :task-clock-marker marker))
            (if file      (org-entry-task-info-set-property task-info :task-clock-file file))
            (if point     (org-entry-task-info-set-property task-info :task-clock-point point))
            (if heading   (org-entry-task-info-set-property task-info :task-clock-heading heading))
            (if clock-sum (org-entry-task-info-set-property task-info :task-clock-clock-sum clock-sum)))
          task-info)))

    (defun org-entry-collect-task-clock-info ()
      ;; (org-element-at-point)
      (let ((heading-with-string-prop
             (unless (org-before-first-heading-p)
               (org-get-heading))))
        (let ((heading (if heading-with-string-prop
                           (substring-no-properties heading-with-string-prop)))
              (marker  (move-marker
                        (make-marker)
                        (point)
                        (org-base-buffer (current-buffer))))
              (file    (buffer-file-name))
              (point   (point))
              (clock-sum (if (org-before-first-heading-p)
                             0
                             (org-clock-sum-current-item)))
              (task-info (cadr (org-element-at-point)))
              (task-content-start ))
          (when heading
            ;; (if root   (push (cons "Root" root) task-info))
            (if marker    (org-entry-task-info-set-property task-info :task-clock-marker marker))
            (if file      (org-entry-task-info-set-property task-info :task-clock-file file))
            (if point     (org-entry-task-info-set-property task-info :task-clock-point point))
            (if heading   (org-entry-task-info-set-property task-info :task-clock-heading heading))
            (if clock-sum (org-entry-task-info-set-property task-info :task-clock-clock-sum clock-sum))
            (if heading-with-string-prop
                (org-entry-task-info-set-property task-info :task-clock-content (org-heading-content-only))))
          task-info)))

    (defun org-heading-content-only ()
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

    (let ((re org-clock-string))
      (re-search-backward re nil t))

    (defun org-clock-items (&optional tstart tend)
      "Return time, clocked on current item in total."
      (if (org-at-heading-p)
          (save-excursion
            (save-restriction
              (let ((ele (org-element-at-point))
                    (re org-clock-string))
                (let ((start (org-element-property :contents-begin ele))
                      (end (progn
                             (outline-next-heading)
                             ;; (org-next-visible-heading 1)
                             (point))))
                  (narrow-to-region start end)
                  (goto-char (point-max))
                  (while (re-search-backward re nil t)
                    (let ((clock (org-element-at-point)))
                      ))

                  ))))))

    (defun org-entry-task-info-get-property (task-info property)
      (plist-get task-info property))

    (defun org-entry-task-info-set-property (task-info property value)
      (plist-put task-info property value))

    (defun org-markers-associated-to-file (file)
      (mapcar '(lambda (e)
                (org-entry-task-info-get-property e :task-clock-marker))
              (funcall org-clocking-api-entries-associated-to-file-p file))))

  (progn ;; "org entries access api for list org"
    (defvar org-entry-list-task-infos nil "org entry task infos")
    (defun org-entry-list-build (collector files)
      (let ()
        (remove nil
                (org-map-entries
                 collector
                 t
                 files))))
    (defun org-entry-list-collect-task-infos (files)
      (org-entry-list-build 'org-entry-collect-task-info files))

    (defun org-entry-list-update-task-infos (&optional force)
      (interactive "P")
      (unless (and (not force)
                   org-entry-list-task-infos)
        (setq org-entry-list-task-infos
              (org-entry-list-collect-task-infos (org-all-task-files))))
      org-entry-list-task-infos))

  (progn ;; "org entries access api for recursive task"
    (defvar org-entry-tree-task-infos nil "org entry task infos")
    (defun org-entry-get-task-infos (files)
      )
    (defvar org-entry-tree-task-info-root-org-file nil "org-entry-tree-task-info-root-org-file")

    (setq org-entry-tree-task-info-root-org-file
          (expand-file-name
           "start.org"
           *task-party-base-dir*))

    (defun org-entry-tree-update-task-infos (&optional force)
      (interactive "P")
      (unless (and (not force)
                   org-entry-tree-task-infos)
        (setq org-entry-tree-task-infos
              (org-entry-tree-get-task-infos
               org-entry-tree-task-info-root-org-file)))
      org-entry-tree-task-infos)

    (defun org-entry-tree-map-subheading (fun)
      "Call FUN for every heading underneath the current one."
      ;; (org-back-to-heading)
      (let ((level (funcall outline-level))
            (collection nil))
        (save-excursion
          (while (and (progn
                        (outline-next-heading)
                        (> (funcall outline-level) level))
                      (not (eobp)))
            (if (= (funcall outline-level) (1+ level))
                (push (funcall fun) collection))))
        collection))

    (defun org-entry-tree-build (collector &optional file)
      "org-collect-task-info"
      (with-current-buffer (if file
                               (find-file-noselect file)
                               (current-buffer))
        (if file (goto-char (point-min)))
        (let* ((entry (funcall collector))
               (sub-tree
                (append
                 (org-entry-tree-map-subheading 'org-entry-tree-collect-task-info)
                 (let* ((file (if file file (buffer-file-name)))
                        (subtree-file
                         (org-entry-task-info-get-property entry :SUBTREEFILE))
                        (subtree-file
                         (if (and subtree-file
                                  (file-relative-name subtree-file))
                             (expand-file-name subtree-file
                                               (if file
                                                   (file-name-directory file)
                                                   default-directory))
                             subtree-file)))
                   (if (and
                        subtree-file
                        (file-readable-p subtree-file))
                       (list
                        (org-entry-tree-collect-task-info subtree-file)))))))
          (if sub-tree
              (org-entry-task-info-set-property entry :sub-tree sub-tree)
              entry))))

    (defun org-entry-tree-collect-task-info (&optional file)
      (org-entry-tree-build 'org-entry-collect-task-info file))

    (defun org-entry-tree-task-infos-tree (file)
      (org-entry-tree-collect-task-info file))

    (defun org-entry-tree-get-task-infos (file)
      (let ()
        (org-entry-tree-collect-task-info file)))

    (defun org-entry-tree-task-node-p (tx)
      (org-entry-task-info-get-property tx :sub-tree))

    (progn ;; "tree api"
      (defun tree-mapcar-nodes (nodep fn tree)
        (list
         (funcall fn tree)
         :sub-tree
         (mapcar
          '(lambda (e)
            (tree-mapcar-nodes nodep fn e))
          (funcall nodep tree))))

      (defun tree-mapc-nodes (nodep fn tree)
        (funcall fn tree)
        (mapc
         '(lambda (e)
           (tree-mapc-nodes nodep fn e))
         (funcall nodep tree)))

      (defun tree-remove-if-not-nodes (nodep fn tree)
        (if (funcall nodep tree)
            (let ((rootele
                   (if (funcall fn tree) tree))
                  (subtree
                   (remove
                    nil
                    (mapcar
                     '(lambda (e)
                       (tree-remove-if-not-nodes nodep fn e))
                     (funcall nodep tree)))))
              (if (or rootele subtree)
                  (plist-put tree :sub-tree subtree)))
            (if (funcall fn tree) tree)))

      (defun tree-mapcar-task-infos (fn tree)
        (tree-mapcar-nodes
         'org-entry-tree-task-node-p fn tree))

      (defun tree-mapc-task-infos (fn tree)
        (tree-mapc-nodes
         'org-entry-tree-task-node-p fn tree))

      (defun tree-remove-if-not-task-infos (fn tree)
        (tree-remove-if-not-nodes
         'org-entry-tree-task-node-p fn tree))

      ;; (testing
      ;;  (setq
      ;;   testxx-remove
      ;;   (tree-remove-if-not-task-infos
      ;;    '(lambda (e) (eq (plist-get e :pre-blank) 4))
      ;;    testxx))

      ;;  (setq testxxmapcar
      ;;        (tree-mapcar-nodes '(lambda (tx) (plist-get tx :sub-tree))
      ;;                           '(lambda (tx) (plist-get tx :title))
      ;;                           ;; testxx
      ;;                           (car (plist-get testxx :sub-tree))
      ;;                           ))

      ;;  (setq testxxmapc
      ;;        (tree-mapc-nodes '(lambda (tx) (plist-get tx :sub-tree))
      ;;                         '(lambda (tx) (plist-get tx :title))
      ;;                         ;; testxx
      ;;                         (car (plist-get testxx :sub-tree))
      ;;                         )))
      ))

  (progn ;; "Interactive utitlity API's for adding root subtree etc"

    (defun org-property-set-function (property fun)
      "fun is like org-icompleting-read
 (completing-read PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)"
      (push
       (cons property fun)
       org-property-set-functions-alist))


    ;; (setq org-property-set-functions-alist nil)
    (org-property-set-function "Root"
                               '(lambda (&rest args)
                                 (ido-read-directory-name
                                  (car args)
                                  default-directory default-directory)))
    (org-property-set-function "SubtreeFile"
                               '(lambda (&rest args)
                                 (file-relative-name
                                  (ido-read-file-name ;; org-iread-file-name
                                   (car args)
                                   default-directory default-directory))))

    (defun task-info-add-root ()
      (interactive)
      (if (org-set-property "Root" nil)
          (org-clocking-entry-update-task-infos t)))
    (defun task-info-add-subtree-file ()
      (interactive)
      (if (org-set-property "SubtreeFile" nil)
          (org-clocking-entry-update-task-infos t)))) ;; "Interactive utitlity API's for adding root subtree etc"

  ) ;; (progn ;; "org entries clocking APIs' API"

(progn ;; "org entries clocking's API"

  (progn ;; "Org entries associated to file predicate functions"

    (defvar org-entry-associated-file-predicate-fns nil)

    (progn ;; api
      (defun org-entries-associated-to-file-by-predicate-p (file)
        (let ((task-infos (org-entry-list-update-task-infos))
              (matched '()))
          (dolist (fn org-entry-associated-file-predicate-fns matched)
            (let ((partitions
                   (reduce (lambda (task-info result)
                             (if (funcall fn file task-info)
                                 (push task-info (first  result))
                                 (push task-info (second result)))
                             result)
                           task-infos
                           :initial-value (list nil nil)
                           :from-end t)))
              (setq
               task-infos (second partitions)
               matched    (append matched (first partitions)))))))

      (defun org-entry-associated-to-file-by-predicate-p (task-info file)
        (if file
            (some
             '(lambda (fn) (funcall fn file task-info))
             org-entry-associated-file-predicate-fns)))
      (org-entry-clocking-api-set :predicate :entries 'org-entries-associated-to-file-by-predicate-p)
      (org-entry-clocking-api-set :predicate :entry   'org-entry-associated-to-file-by-predicate-p)
      (org-entry-clocking-api-set :predicate :update  'org-entry-list-update-task-infos))


    (progn ;; functions
      (setq org-entry-associated-file-predicate-fns nil)

      (defun org-entries-register-associated-to-file-predicate-function (fn)
        (add-to-list
         'org-entry-associated-file-predicate-fns
         fn))

      (defun org-entry-associated-file-org-file-predicate (file task-info)
        "Predicate funtion to check if file matches to task-info's file attribute."
        (let ((org-file (org-entry-task-info-get-property task-info :task-clock-file)))
          (if (and file org-file)
              (string-equal
               (file-truename file)
               (file-truename org-file)))))
      (org-entries-register-associated-to-file-predicate-function 'org-entry-associated-file-org-file-predicate)

      (defun org-entry-associated-file-root-dir-predicate (file task-info)
        "Predicate funtion to check if file matches to task-info's file attribute."
        (let ((root
               (org-entry-task-info-get-property task-info :ROOT)))
          (if (and root file)
              (string-match
               (file-truename root)
               (file-truename file)))))
      (org-entries-register-associated-to-file-predicate-function 'org-entry-associated-file-root-dir-predicate)

      ;; (defun org-entry-associated-file-xx-p (file task-info)
      ;;   )
      ;; (org-entries-register-associated-to-file-predicate-function 'org-entry-associated-file-xx-p)
      ;; )
      )
    )

  (progn ;; "Org entries associated to file rank functions"
    ;; TODO: matching should be merit based.
    ;; TODO: logical AND OR method should be possible in match-fn results
    ;; TODO: exclusion fecelities also should be present.
    '(
      '(matches
        '(file based)x
        '(dir based -merit) x
        '(status based) x
        '(user input based)
        '(config based) x
        '(time based recently opened)
        '(heading level based)))




    (defvar org-entry-associated-file-rank-fns nil)

    (progn ;; api
      (defun org-entries-associated-to-file-by-rank-p (file)
        (let ((task-infos (org-entry-list-update-task-infos))
              (matched '()))
          (dolist (fn org-entry-associated-file-rank-fns matched)
            (let ((partitions
                   (reduce (lambda (task-info result)
                             (if (funcall fn file task-info)
                                 (push task-info (first  result))
                                 (push task-info (second result)))
                             result)
                           task-infos
                           :initial-value (list nil nil)
                           :from-end t)))
              (setq
               task-infos (second partitions)
               matched    (append matched (first partitions)))))))
      (defun org-entry-associated-to-file-by-rank-p (task-info file)
        (if file
            (apply '+
                   (mapcar
                    '(lambda (fn)
                      (funcall fn file task-info))
                    org-entry-associated-file-rank-fns))
            0))
      (org-entry-clocking-api-set :rank :entries 'org-entries-associated-to-file-by-rank-p)
      (org-entry-clocking-api-set :rank :entry   'org-entry-associated-to-file-by-rank-p)
      (org-entry-clocking-api-set :rank :update  'org-entry-list-update-task-infos))

    (progn ;; functions
      (setq org-entry-associated-file-rank-fns nil)

      (defun org-entries-register-associated-to-file-rank-function (fn)
        (add-to-list
         'org-entry-associated-file-rank-fns
         fn))

      (defun org-entry-associated-file-org-file-rank (file task-info)
        "Predicate funtion to check if file matches to task-info's file attribute."
        (if (string-equal
             (file-truename file)
             (file-truename
              (org-entry-task-info-get-property task-info :task-clock-file)))
            10
            0))
      (org-entries-register-associated-to-file-rank-function 'org-entry-associated-file-org-file-rank)

      (defun org-entry-associated-file-root-dir-rank (file task-info)
        "Predicate funtion to check if file matches to task-info's file attribute."
        (let* ((root
                (org-entry-task-info-get-property task-info :ROOT))
               (root (if root (file-truename root))))
          (if (and
               root
               (string-match root file))
              (length root)
              0)))
      (org-entries-register-associated-to-file-rank-function 'org-entry-associated-file-root-dir-rank)

      (defun org-entry-associated-file-status-rank (file task-info)
        "Predicate funtion to check if file matches to task-info's file attribute."
        (let* ((status
                (org-entry-task-info-get-property task-info 'status)))
          (if (string-equal status "CLOSED") -30 0)))
      (org-entries-register-associated-to-file-rank-function 'org-entry-associated-file-status-rank)

      (defun org-entry-associated-file-task-rank (file task-info)
        "Predicate funtion to check if file matches to task-info's file attribute."
        (let* ((rank
                (org-entry-task-info-get-property task-info :RANK)))
          (if rank (string-to-number rank) 0)))
      (org-entries-register-associated-to-file-rank-function 'org-entry-associated-file-task-rank)

      (defun org-entry-associated-file-level-rank (file task-info)
        "Predicate funtion to check if file matches to task-info's file attribute."
        (let* ((level
                (org-entry-task-info-get-property task-info :task-clock-level)))
          level))
      (org-entries-register-associated-to-file-rank-function 'org-entry-associated-file-level-rank))
    )

  (progn ;; "Org entries associated to file key functions on recursive taskinfos"
    ;; TODO: matching should be merit based.
    ;; TODO: logical AND OR method should be possible in match-fn results
    ;; TODO: exclusion fecelities also should be present.
    '(
      '(matches
        '(file based)x
        '(dir based -merit) x
        '(status based) x
        '(user input based)
        '(config based) x
        '(time based recently opened)
        '(heading level based)))

    (defvar org-entry-associated-file-key-fns nil)

    (progn ;; api
      (defun org-entries-associated-to-file-by-keys-p (file)
        (let ((task-infos (org-entry-tree-update-task-infos))
              (matched '()))
          (tree-mapc-task-infos
           '(lambda (task)
             (let ((result (org-entry-associated-to-file-by-keys-p task file)))
               (when result
                 (push task matched))))
           task-infos)
          matched))

      (defun org-entry-associated-to-file-by-keys-p (task-info file)
        (if file
            (if (> (org-entries-associated-key-fn-value :status task-info file) -20)
                (>
                 (+
                  (org-entries-associated-key-fn-value :timebeing task-info file)
                  (org-entries-associated-key-fn-value :root task-info file)
                  ;; (org-entries-associated-key-fn-value :org-file task-info file)
                  (org-entries-associated-key-fn-value :task-info-key task-info file)
                  (org-entries-associated-key-fn-value :heading-level task-info file))
                 0)))))

    (org-entry-clocking-api-set :keys :entries 'org-entries-associated-to-file-by-keys-p)
    (org-entry-clocking-api-set :keys :entry   'org-entry-associated-to-file-by-keys-p)
    (org-entry-clocking-api-set :keys :update  'org-entry-tree-update-task-infos)) ;; api

  (progn ;; functions
    (progn
      (setq org-entry-associated-file-key-fns nil)

      (defun org-entries-register-associated-to-file-key-function (key fn)
        (setq
         org-entry-associated-file-key-fns
         (plist-put
          org-entry-associated-file-key-fns key fn)))

      (eval-when-compile
        (defmacro defassoc-file-key (name key args &rest body)
          `(progn
             (defun ,name ,args
               ,@body)
             (org-entries-register-associated-to-file-key-function ,key ',name))))

      (put 'defassoc-file-key 'lisp-indent-function 3)
      (defun org-entries-associated-key-function (key)
        (plist-get org-entry-associated-file-key-fns key))
      (defun org-entries-associated-key-fn-value (key task-info file)
        (let ((keyfn (org-entries-associated-key-function key)))
          (if keyfn
              (funcall keyfn task-info file)
              0))))

    (defassoc-file-key org-entry-associated-file-org-file-key :org-file (task-info file)
                       "Predicate funtion to check if file matches to task-info's file attribute."
                       (let ((org-file (org-entry-task-info-get-property task-info :task-clock-file)))
                         (if (and file org-file
                                  (string-equal
                                   (file-truename file)
                                   (file-truename org-file)))
                             10
                             0)))

    (defassoc-file-key org-entry-associated-file-root-dir-key :root (task-info file)
                       "Predicate funtion to check if file matches to task-info's file attribute."
                       (let* ((root
                               (org-entry-task-info-get-property task-info :ROOT))
                              (root (if root (file-truename root)))
                              (file (if file (file-truename file))))
                         (if (and root file
                                  (string-match root file))
                             (length root)
                             0)))

    (defassoc-file-key org-entry-associated-file-status-key :status (task-info file)
                       "Predicate funtion to check if file matches to task-info's file attribute."
                       (let* ((status
                               (org-entry-task-info-get-property task-info 'status)))
                         (if (string-equal status "CLOSED") -30 0)))

    (defassoc-file-key org-entry-associated-file-task-key :task-key (task-info file)
                       "Predicate funtion to check if file matches to task-info's file attribute."
                       (let* ((key (org-entry-task-info-get-property task-info :KEY)))
                         (if key (string-to-number key) 0)))

    (defassoc-file-key org-entry-associated-file-level-key :heading-level (task-info file)
                       "Predicate funtion to check if file matches to task-info's file attribute."
                       (let* ((level
                               (org-entry-task-info-get-property task-info :task-clock-level)))
                         (if level level 0)))

    (defassoc-file-key org-entry-associated-file-timebeing-key :timebeing (task-info file)
                       (let ((timebeing (org-entry-task-info-get-property task-info :TIMEBEING)))
                         (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
                               (clocked-time   (org-entry-task-info-get-property task-info :task-clock-clock-sum)))
                           (if (and
                                (numberp clocked-time)
                                (numberp timebeing-time)
                                (> timebeing-time clocked-time))
                               (- timebeing-time clocked-time)
                               0))))

    ;; (defassoc-file-key org-entry-associated-file-current-clock-key :current-clock (task-info file)
    ;;   "Predicate funtion to check if file matches to task-info's file attribute."
    ;;   (let* ((task-marker
    ;;           (org-entry-task-info-get-property task-info :task-clock-marker)))
    ;;     (if (and
    ;;          org-clock-marker
    ;;          task-marker
    ;;          (equal
    ;;           (marker-buffer org-clock-marker)
    ;;           (marker-buffer task-marker))
    ;;          (equal
    ;;           (marker-position org-clock-marker)
    ;;           (marker-position task-marker)))
    ;;         100
    ;;         0)))
    ))



;; API end here






















(provide 'org-context-clocking-api)
;;; org-context-clocking-api.el ends here
