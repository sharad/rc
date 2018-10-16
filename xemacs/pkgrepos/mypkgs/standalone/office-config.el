;;
;; office.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Wed Dec  1 17:11:05 2010 Sharad Pratap
;; $Id: office-config-backup.el.backup,v 1.1 2016/06/15 14:45:02 s Exp s $
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;

(require 'init-config "~/.xemacs/init-config.el")
(require 'files-config)
(eval-when-compile
  (require 'use-package)
  (require 'orgmode-config))



(deh-require-maybe orgmode-config

  (defun org-task-files (&optional party)
    (let ((party (or party task-current-party)))
      (directory-files-recursive
       (task-party-dir party) "\\.org$" 7)))

  (defun org-all-task-files ()
    (let ()
      (directory-files-recursive
       (org-publish-get-attribute "tasks" "org" :base-directory)
       "\\.org$" 7)))

  (defun org-task-refile-target (party)
    ;; (interactive)
    (let* ((party (or party task-current-party))
           (task-files (org-task-files party)))
                                        ;all files returned by `org-task-files'
      `((,task-files :maxlevel . 3))))

  (defun org-clock-in-refile-task (party)
    (interactive
     (list (ido-completing-read
            "Seletc Party: "
            (mapcar 'car task-parties)
            nil
            t
            task-current-party)))
    (org-clock-in-refile (org-task-refile-target party)))

  (when nil
    (defun org-clock-select-task-from-clocks (clocks &optional prompt)
      "Select a task that was recently associated with clocking."
      (interactive)
      (let (och chl sel-list rpl (i 0) s)
        ;; Remove successive dups from the clock history to consider
        (mapc (lambda (c) (if (not (equal c (car och))) (push c och)))
              clocks)
        (setq och (reverse och) chl (length och))
        (if (zerop chl)
            (user-error "No recent clock")
            (save-window-excursion
              (org-switch-to-buffer-other-window
               (get-buffer-create "*Clock Task Select*"))
              (erase-buffer)
              ;; (when (marker-buffer org-clock-default-task)
              ;;   (insert (org-add-props "Default Task\n" nil 'face 'bold))
              ;;   (setq s (org-clock-insert-selection-line ?d org-clock-default-task))
              ;;   (push s sel-list))
              ;; (when (marker-buffer org-clock-interrupted-task)
              ;;   (insert (org-add-props "The task interrupted by starting the last one\n" nil 'face 'bold))
              ;;   (setq s (org-clock-insert-selection-line ?i org-clock-interrupted-task))
              ;;   (push s sel-list))
              ;; (when (org-clocking-p)
              ;;   (insert (org-add-props "Current Clocking Task\n" nil 'face 'bold))
              ;;   (setq s (org-clock-insert-selection-line ?c org-clock-marker))
              ;;   (push s sel-list))
              (insert (org-add-props "Recent Tasks\n" nil 'face 'bold))
              (mapc
               (lambda (m)
                 (when (marker-buffer m)
                   (setq i (1+ i)
                         s (org-clock-insert-selection-line
                            (if (< i 10)
                                (+ i ?0)
                                (+ i (- ?A 10))) m))
                   (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
                   (push s sel-list)))
               och)
              (run-hooks 'org-clock-before-select-task-hook)
              (goto-char (point-min))
              ;; Set min-height relatively to circumvent a possible but in
              ;; `fit-window-to-buffer'
              (fit-window-to-buffer nil nil (if (< chl 10) chl (+ 5 chl)))
              (message (or prompt "Select task for clocking:"))
              (setq cursor-type nil rpl (read-char-exclusive))
              (kill-buffer)
              (cond
                ((eq rpl ?q) nil)
                ((eq rpl ?x) nil)
                ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
                (t (user-error "Invalid task choice %c" rpl)))))))

    ;;     (defun org-clock-insert-selection-line (i marker)
    ;;       "Insert a line for the clock selection menu.
    ;; And return a cons cell with the selection character integer and the marker
    ;; pointing to it."
    ;;       (when (marker-buffer marker)
    ;;         (let (file cat task heading prefix)
    ;;           (with-current-buffer (org-base-buffer (marker-buffer marker))
    ;;             (save-excursion
    ;;               (save-restriction
    ;;                 (widen)
    ;;                 (ignore-errors
    ;;                   (goto-char marker)
    ;;                   (setq file (buffer-file-name (marker-buffer marker))
    ;;                         cat (org-get-category)
    ;;                         heading (org-get-heading 'notags)
    ;;                         prefix (save-excursion
    ;;                                  (org-back-to-heading t)
    ;;                                  (looking-at org-outline-regexp)
    ;;                                  (match-string 0))
    ;;                         task (substring
    ;;                               (org-fontify-like-in-org-mode
    ;;                                (concat prefix heading)
    ;;                                org-odd-levels-only)
    ;;                               (length prefix)))))))
    ;;           (when (and cat task)
    ;;             (insert (format "[%c] %-12s  %s\n" i cat task))
    ;;             (cons i marker)))))
    )

  ) ;; (deh-require-maybe orgmode-config

(deh-require-mustbe (and
                     iproject
                     orgmode-config
                     publishing-config
                     project-buffer-file
                     utils-config)

  (eval-when-compile
    '(progn
      (require 'orgmode-config)))

  (deh-section "File based clocking"

    ;; API
    ;; (org-clocking-api-entries-associated-to-file-p file)
    ;; (org-clocking-api-entry-associated-to-file-p task-info file)
    ;; (org-clocking-api-entry-update-task-infos &optional force)

    (deh-section "org-entry-clocking-api interface"

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

    (deh-section "org entries clocking's APIs' API"

      (deh-section "org entries accss common api"
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

      (deh-section "org entries access api for list org"
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

      (deh-section "org entries access api for recursive task"
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
          (if org-entry-tree-task-info-root-org-file
              (if (file-exists-p org-entry-tree-task-info-root-org-file)
                  (unless (and (not force)
                               org-entry-tree-task-infos)
                    (setq org-entry-tree-task-infos
                          (org-entry-tree-get-task-infos
                           org-entry-tree-task-info-root-org-file)))
                (message "file %s not exists." org-entry-tree-task-info-root-org-file))
            (message "org-entry-tree-task-info-root-org-file is nil"))
          org-entry-tree-task-infos)

        ;; )

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

        (deh-section "tree api"
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

      (deh-section "Interactive utitlity API's for adding root subtree etc"

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

        ) ;; (deh-section "org entries clocking APIs' API"

      (deh-section "org entries clocking's API"

        (deh-section "Org entries associated to file predicate functions"

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

        (deh-section "Org entries associated to file rank functions"
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

        (deh-section "Org entries associated to file key functions on recursive taskinfos"
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
          )))

  (deh-section "task main work"
    (defvar task-current-file  nil)
    (defvar task-previous-file nil)
    (defvar task-current-file-time 2)
    (defvar last-buffer-select-time (current-time))
    (defvar buffer-select-timer nil)
    (defvar update-current-file-msg "")
    ;; (defvar org-entry-clocking-api-name :predicate "API")
    (defvar org-entry-clocking-api-name :keys "API")
    (defvar org-clocking-api-entries-associated-to-file-p (org-entry-clocking-api-get org-entry-clocking-api-name :entries))
    (defvar org-clocking-api-entry-associated-to-file-p   (org-entry-clocking-api-get org-entry-clocking-api-name :entry))
    (defvar org-clocking-api-entry-update-task-infos      (org-entry-clocking-api-get org-entry-clocking-api-name :update))


    (defun custom-plist-keys (in-plist)
      (if (null in-plist)
          in-plist
          (cons (car in-plist) (custom-plist-keys (cddr in-plist)))))

    (defun org-task-clocking-api ()
      "org task clocking select api to use."
      (interactive)
      (let* ((api-keys (custom-plist-keys org-entry-clocking-api))
             (api-name (ido-completing-read
                        "org task clocking api name: "
                        (mapcar 'symbol-name api-keys)
                        nil
                        t
                        (symbol-name org-entry-clocking-api-name)))
             (api-key (intern api-name)))
        (setq org-entry-clocking-api-name api-key)
        (if (and
             (org-entry-clocking-api-get org-entry-clocking-api-name :entries)
             (org-entry-clocking-api-get org-entry-clocking-api-name :entry)
             (org-entry-clocking-api-get org-entry-clocking-api-name :update))
            (setq
             org-clocking-api-entries-associated-to-file-p (org-entry-clocking-api-get org-entry-clocking-api-name :entries)
             org-clocking-api-entry-associated-to-file-p   (org-entry-clocking-api-get org-entry-clocking-api-name :entry)
             org-clocking-api-entry-update-task-infos      (org-entry-clocking-api-get org-entry-clocking-api-name :update)))))

    (defun org-clocking-entry-update-task-infos (&optional force)
      "Update task infos"
      (interactive "P")
      (funcall org-clocking-api-entry-update-task-infos force))

    (defun update-current-file ()
      (if (> (float-time
              (time-since last-buffer-select-time))
             task-current-file-time)
          (let* ((buff (window-buffer))
                 (file (buffer-file-name buff)))
            (unless (or
                     (and
                      (string-equal task-previous-file file)
                      (string-equal task-current-file  file))
                     (minibufferp buff))
              (setq
               task-previous-file task-current-file
               task-current-file  file)

              (unless (org-clock-entry-associated-to-file-p file)
                (org-entry-run-associated-clock file))))))

    (defun org-clock-entry-current-entry ()
      (and
       ;; file
       org-clock-marker
       (> (marker-position-nonil org-clock-marker) 0)
       (org-with-clock-position (list org-clock-marker)
         (org-previous-visible-heading 1)
         (let ((info (org-entry-collect-task-info)))
           info))))

    ;; not workiong
    ;; (defun org-clock-entry-associated-to-file-p (file)
    ;;   (and
    ;;    ;; file
    ;;    org-clock-marker
    ;;    (> (marker-position-nonil org-clock-marker) 0)
    ;;    (org-with-clock-position (list org-clock-marker)
    ;;      (org-previous-visible-heading 1)
    ;;      (let ((info (org-entry-collect-task-info)))
    ;;        (if (funcall org-clocking-api-entry-associated-to-file-p info file)
    ;;            info)))))

    (defun org-clock-entry-associated-to-file-p (file)
      (let ((info (org-clock-entry-current-entry)))
        (funcall org-clocking-api-entry-associated-to-file-p info file)))

    (defun org-entry-run-associated-clock (file)
      (let ()
        (let* ((matched-clocks
                (remove-if-not
                 '(lambda (marker) (marker-buffer marker))
                 (org-markers-associated-to-file file)))
               (selected-clock (if (> (length matched-clocks) 1)
                                   (org-clock-select-task-from-clocks matched-clocks)
                                   (car matched-clocks))))
          (if selected-clock
              (let ((org-log-note-clock-out nil)
                    (prev-org-clock-buff (marker-buffer org-clock-marker)))

                (let ((prev-clock-buff-read-only
                       (if prev-org-clock-buff
                           (with-current-buffer (marker-buffer org-clock-marker)
                             buffer-read-only))))

                  (if prev-org-clock-buff
                      (with-current-buffer prev-org-clock-buff
                        (setq buffer-read-only nil)))

                  (setq update-current-file-msg org-clock-marker)

                  (with-current-buffer (marker-buffer selected-clock)
                    (let ((buffer-read-only nil))
                      (org-clock-clock-in (list selected-clock))))

                  (if prev-org-clock-buff
                      (with-current-buffer prev-org-clock-buff
                        (setq buffer-read-only prev-clock-buff-read-only)))))
              (setq update-current-file-msg "null clock")))))

    ;; (defun run-task-current-file-timer ()
    ;;   (let ()
    ;;     (setq last-buffer-select-time (current-time))
    ;;     (when buffer-select-timer
    ;;       (cancel-timer buffer-select-timer)
    ;;       (setq buffer-select-timer nil))
    ;;     (setq buffer-select-timer
    ;;           (run-with-timer
    ;;            (1+ task-current-file-time)
    ;;            nil
    ;;            'update-current-file))))

    (defun run-task-current-file-timer ()
      (let ()
        (setq last-buffer-select-time (current-time))
        (when buffer-select-timer
          (cancel-timer buffer-select-timer)
          (setq buffer-select-timer nil))
        (setq buffer-select-timer
              ;; distrubing while editing.
              ;; (run-with-timer
              (run-with-idle-timer
               (1+ task-current-file-time)
               nil
               'update-current-file))))

    (defun org-entries-select (entries &optional prompt)
      "Select a task that was recently associated with clocking."
      (interactive)
      (let (och
            chl
            sel-list
            rpl
            (i 0)
            s)
        ;; Remove successive dups from the clock history to consider
        ;; (mapc (lambda (c) (if (not (equal c (car och))) (push c och)))
        ;;       clocks)
        (setq och (reverse och) chl (length och))
        (if (zerop chl)
            (user-error "No recent clock")
            (save-window-excursion
              (org-switch-to-buffer-other-window
               (get-buffer-create "*Clock Task Select*"))
              (erase-buffer)
              (insert (org-add-props "Recent Tasks\n" nil 'face 'bold))
              (mapc
               (lambda (m)
                 (when (marker-buffer m)
                   (setq i (1+ i)
                         s (org-clock-insert-selection-line
                            (if (< i 10)
                                (+ i ?0)
                                (+ i (- ?A 10))) m))
                   (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
                   (push s sel-list)))
               och)
              (run-hooks 'org-clock-before-select-task-hook)
              (goto-char (point-min))
              ;; Set min-height relatively to circumvent a possible but in
              ;; `fit-window-to-buffer'
              (fit-window-to-buffer nil nil (if (< chl 10) chl (+ 5 chl)))
              (message (or prompt "Select task for clocking:"))
              (setq cursor-type nil rpl (read-char-exclusive))
              (kill-buffer)
              (cond
                ((eq rpl ?q) nil)
                ((eq rpl ?x) nil)
                ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
                (t (user-error "Invalid task choice %c" rpl)))))))

    (defun org-clock-select-task-from-clocks (clocks &optional prompt)
      "Select a task that was recently associated with clocking."
      (interactive)
      (let (och chl sel-list rpl (i 0) s)
        ;; Remove successive dups from the clock history to consider
        (mapc (lambda (c) (if (not (equal c (car och))) (push c och)))
              clocks)
        (setq och (reverse och) chl (length och))
        (if (zerop chl)
            (user-error "No recent clock")
            (save-window-excursion
              (org-switch-to-buffer-other-window
               (get-buffer-create "*Clock Task Select*"))
              (erase-buffer)
              ;; (when (marker-buffer org-clock-default-task)
              ;;   (insert (org-add-props "Default Task\n" nil 'face 'bold))
              ;;   (setq s (org-clock-insert-selection-line ?d org-clock-default-task))
              ;;   (push s sel-list))
              ;; (when (marker-buffer org-clock-interrupted-task)
              ;;   (insert (org-add-props "The task interrupted by starting the last one\n" nil 'face 'bold))
              ;;   (setq s (org-clock-insert-selection-line ?i org-clock-interrupted-task))
              ;;   (push s sel-list))
              ;; (when (org-clocking-p)
              ;;   (insert (org-add-props "Current Clocking Task\n" nil 'face 'bold))
              ;;   (setq s (org-clock-insert-selection-line ?c org-clock-marker))
              ;;   (push s sel-list))
              (insert (org-add-props "Guessed Tasks\n" nil 'face 'bold))
              (mapc
               (lambda (m)
                 (when (marker-buffer m)
                   (setq i (1+ i)
                         s (org-clock-insert-selection-line
                            (if (< i 10)
                                (+ i ?0)
                                (+ i (- ?A 10))) m))
                   (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
                   (push s sel-list)))
               och)
              (run-hooks 'org-clock-before-select-task-hook)
              (goto-char (point-min))
              ;; Set min-height relatively to circumvent a possible but in
              ;; `fit-window-to-buffer'
              (fit-window-to-buffer nil nil (if (< chl 10) chl (+ 5 chl)))
              (message (or prompt "Select task for clocking:"))
              (setq cursor-type nil rpl (read-char-exclusive))
              (kill-buffer)
              (cond
                ((eq rpl ?q) nil)
                ((eq rpl ?x) nil)
                ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
                (t (user-error "Invalid task choice %c" rpl)))))))

    (progn
      (add-hook 'buffer-list-update-hook     'run-task-current-file-timer)
      (add-hook 'elscreen-screen-update-hook 'run-task-current-file-timer)
      (add-hook 'elscreen-goto-hook          'run-task-current-file-timer))

    (when nil
      (remove-hook 'buffer-list-update-hook 'run-task-current-file-timer)
      (setq buffer-list-update-hook nil)
      (remove-hook 'elscreen-screen-update-hook 'run-task-current-file-timer)
      (remove-hook 'elscreen-goto-hook 'run-task-current-file-timer))


    (deh-section "Org task clock reporting"
     ;; #+BEGIN: task-clock-report-with-comment :parameter1 value1 :parameter2 value2 ...
     ;; #+END:
      (defun org-dblock-write:task-clock-report-with-comment (params)
        (let ((fmt (or (plist-get params :format) "%d. %m. %Y")))
          (insert "Last block update at: "
                  (format-time-string fmt))))


      (deh-section "nagora report"
        (defun dfeich/org-clock-get-tr-for-ivl (buffer tstart-str tend-str &optional limit)
          "Return clocking information touching a given time interval."
          (cl-assert (and buffer (get-buffer buffer)) nil "Error: :buffer must be defined")
          (with-current-buffer buffer
            (save-excursion
              (let ((re (concat "^\\(\\*+[ \t]*.*\\)\\|^[ \t]*"
                                org-clock-string
                                "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
                    (counter 0)
                    (tmphd "BEFORE FIRST HEADING")
                    (tstart (org-time-string-to-seconds tstart-str))
                    (tend (org-time-string-to-seconds tend-str))
                    (limit (or limit (point-max)))
                    headings timelst
                    lvl title result ts te)
                (goto-char (point-min))
                (cl-block myblock
                  (while (re-search-forward re nil t)
                    (cond
                      ;; found a org heading
                      ((match-end 1)
                       (if (> (length timelst) 0)
                           (setq result (nconc result (list (list
                                                             (copy-sequence headings)
                                                             timelst)))))
                       (setq tmphd (org-heading-components)
                             lvl (car tmphd)
                             title (nth 4 tmphd)
                             timelst nil)
                       ;; maintain a list of the current heading hierarchy
                       (cond
                         ((> lvl (length headings))
                          (setq headings  (nconc headings `(,title))))
                         ((= lvl (length headings))
                          (setf (nth (1- lvl) headings) title))
                         ((< lvl (length headings))
                          (setq headings (cl-subseq headings 0 lvl))
                          (setf (nth (1- lvl) headings) title))))
                      ;; found a clock line with 2 timestamps
                      ((match-end 3)
                       (setq ts (save-match-data (org-time-string-to-seconds
                                                  (match-string-no-properties 2)))
                             te (save-match-data (org-time-string-to-seconds
                                                  (match-string-no-properties 3))))
                       ;; the clock lines progress from newest to oldest. This
                       ;; enables skipping the rest if this condition is true
                       (if (> tstart te)
                           (if (re-search-forward "^\\(\\*+[ \t]*.*\\)" nil t)
                               (beginning-of-line)
                               (goto-char (point-max)))
                           (when (> tend ts)
                             (setq timelst (nconc timelst (list
                                                           (list (match-string-no-properties 2)
                                                                 (match-string-no-properties 3)))))))))
                    (when (>= (point) limit)
                      (cl-return-from myblock))))
                (if (> (length timelst) 0)
                    (setq result (nconc result (list (list (copy-sequence headings)
                                                           timelst)))))
                result))))

        (defun dfeich/org-slice-tr (tstart-str tend-str cutstart-str cutend-str)
          "Return time slice of a time range in minutes."
          (let ((tstart (org-time-string-to-seconds tstart-str))
                (tend (org-time-string-to-seconds tend-str))
                (cutstart (if (stringp cutstart-str)
                              (org-time-string-to-seconds cutstart-str)
                              cutstart-str))
                (cutend (if (stringp cutend-str)
                            (org-time-string-to-seconds cutend-str)
                            cutend-str))
                result)
            (setq result (max 0
                              (/  (- (min tend cutend) (max tstart cutstart))
                                  60)))))

        (defun dfeich/org-clock-hourly-report (struct tstart-str tend-str)
          "Return a structure containing a per hour report within an interval."
          (let* ((tstart (org-time-string-to-seconds tstart-str))
                 (tend (org-time-string-to-seconds tend-str))
                 (delta 3600)
                 (intvls (cl-loop for tm from tstart to (- tend delta) by delta
                                  collect `(,tm ,(+ tm delta))))
                 result)
            ;; iterate over the intervals for the final table
            (cl-loop for iv in intvls
                     collect (list
                              iv
                              (let* ((cutstart (car iv))
                                     (cutend (cadr iv))
                                     (tmsum 0.0)
                                     headings trlst)
                                ;; iterate over the task structure
                                (cl-loop
                                 for item in struct
                                 do (progn
                                      (setq headings (car item)
                                            trlst (cadr item)
                                            ;; sum up the parts of the time
                                            ;; ranges falling into this
                                            ;; interval
                                            tmsum (apply
                                                   #'+
                                                   (mapcar
                                                    (lambda (tr)
                                                      (dfeich/org-slice-tr (car tr)
                                                                           (cadr tr)
                                                                           cutstart
                                                                           cutend))
                                                    trlst))))
                                 if (> tmsum 0) collect `(,headings ,tmsum) into lst
                                 finally return lst))))))

        (defun org-dblock-write:nagora-report (params)
          "Fill in a dynamic timesheet reporting block."
          (let* ((buffer (plist-get params :buffer))
                 (day (symbol-name (plist-get params :day)))
                 (tstart (if (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" day)
                             day
                             (error "Error: day format must be in YYYY-mm-dd format")))
                 (tend (concat day " 23:59"))
                 (table (dfeich/org-clock-hourly-report
                         (dfeich/org-clock-get-tr-for-ivl buffer tstart tend)
                         tstart tend)))
            (insert (format "#+CAPTION: timesheet for day %s\n" day))
            (insert "|Time|Customer| Task |Minutes|\n|------\n")
            (cl-loop
             for item in table
             do (let ((ivl (car item))
                      (entries (cadr item)))
                  (cl-loop for e in entries
                           do (let ((headings (car e))
                                    (minutes (cadr e)))
                                (insert (concat
                                         "|"
                                         (format-time-string "%H:%M" (seconds-to-time
                                                                      (car ivl)))
                                         "-"
                                         (format-time-string "%H:%M" (seconds-to-time
                                                                      (cadr ivl)))
                                         "|" (nth 1 headings)
                                         "|" (car (last headings))
                                         "|" (format "%d" minutes)
                                         "|\n"))))))
            (insert "|----\n|TOTAL||||\n#+TBLFM: @>$>=vsum(@I..@II)")
            (search-backward "Time")
            (org-table-align)
            (org-table-recalculate '(16)))))


      (deh-section "time sheet"
        (defcustom org-time-sheet-date-formatter
          (lambda (day month year) (format "%4d-%02d-%02d" year month day))
          "Function to format date in time sheets.
It takes three numbers as arguments: day month year."
          :type 'function
          :group 'org-clock)

        (defcustom org-time-sheet-time-formatter
          (lambda (start end hour minutes headings)
            (list (format-time-string "%F %R" (apply 'encode-time minutes-start))
                  (format-time-string "%F %R" (apply 'encode-time minutes-end))
                  (format "%2d00--%2d00" hour (1+ hour)) (or (nth 1 headings) "") (or (nth 2 headings) "") minutes))
          "Callback function returning one table line in a time sheet (as list).
The arguments of the function are:
START:    start time with format as in `decode-time'
END:     end time with format as in `decode-time'
MINUTES:  number of minutes between start time and end time
HEADINGS: the heading titles of the current entry and all its parents as a list starting with the top-parent."
          :type 'function
          :group 'org-clock)

        (eval-when-compile
          (require 'cl-lib))
        (require 'org-element)
        (require 'ob-core)

        (defun org-element-parent (element &optional type)
          "Get parent of ELEMENT or nil if there is none.
If TYPE is non-nil get next parent of that type."
          (let* ((props (cadr element))
                 (parent (plist-get props :parent)))
            (if type
                (when parent
                  (if (eq (car parent) type)
                      parent
                      (org-element-parent parent type)))
                parent)))

        (defun org-element-timestamp-less-p (ts1 ts2 &optional end)
          "Non-nil if timestamp TS1 is less than timestamp TS2.
TS1 and TS2 is timestamp data as returned by `org-element-timestamp-parser'.
If end is non-nil the end-time of TS1 and TS2 is compared else the start time."
          (cl-assert (eq (car ts1) 'timestamp) "TS1 is not a timestamp")
          (cl-assert (eq (car ts2) 'timestamp) "TS2 is not a timestamp")
          (let ((p1 (cadr ts1))
                (p2 (cadr ts2))
                (tests '("year" "month" "day" "hour" "minute"))
                ret)
            (while (and (let* ((what (intern-soft (concat ":" (car tests) (if end "-end" "-start"))))
                               (t1 (plist-get p1 what))
                               (t2 (plist-get p2 what)))
                          (cond
                            ((< t1 t2)
                             (setq ret t)
                             nil)
                            ((= t1 t2) t)))
                        (setq tests (cdr tests))))
            ret))

        (defun time-day-month-year (time)
          "Return the list (day month year) from TIME.
TIME may be the time as returned by `current-time' or by `decode-time'."
          (if (<= (length time) 4)
              (setq time (decode-time time)))
          (mapcar (lambda (el) (nth el time)) '(3 4 5)))

        (defun org-element-timestamp-to-time (timestamp &optional start/end encode)
          "Convert start or end of TIMESTAMP returned by `org-element-timestamp-parser'
to time format as defined in the documentation of `decode-time'.
START/END is either the symbol 'start or 'end or nil which is equivalent to 'start.
If ENCODE is non-nil the return value is encoded as described in the documentation for `current-time'."
          (cl-assert (eq (car timestamp) 'timestamp) "Argument is not a timestamp")
          (unless start/end (setq start/end 'start))
          (let* ((p (cadr timestamp))
                 (ret (append
                       '(0)
                       (mapcar (lambda (what) (plist-get p (intern-soft (concat ":" what "-" (symbol-name start/end))))) '("minute" "hour" "day" "month" "year"))
                       (list 0 nil (car (current-time-zone))))))
            (if encode
                (apply #'encode-time ret)
                ret)))

        (defmacro decoded-time-complete-timezone (t1 t2)
          "If only one of the time specifications T1 and T2 has time-zone information
append that to the other one."
          `(let ((n1 (length ,t1))
                 (n2 (length ,t2)))
             (cond
               ((> n1 n2)
                (setq ,t2 (copy-sequence ,t2))
                (setf (nthcdr n2 ,t2) (nthcdr n2 ,t1)))
               ((< n1 n2)
                (setq ,t1 (copy-sequence ,t1))
                (setf (nthcdr n1 ,t1) (nthcdr n1 ,t2))))))

        (defun decoded-time-less-p (t1 t2)
          "Like `time-less-p' but for decoded time values as `decode-time' returns."
          (decoded-time-complete-timezone t1 t2)
          (time-less-p (apply 'encode-time t1) (apply 'encode-time t2)))

        (defun decoded-time-advance (time dt)
          "Return TIME advanced by DT but for decoded time values as `decode-time' returns.
The time zone information of time is used for the result."
          (decode-time (apply 'encode-time (append (cl-mapcar #'+ (butlast time (- (length time) 6)) (butlast dt (- (length dt) 6))) (nthcdr 6 time)))))

        (defun org-time-sheet (&optional tStart tEnd &optional dont-sum)
          "Create time sheet for time span from tStart to tEnd from current org buffer.
When called non-interactively each of the parameters tStart and tEnd may be nil
or must be decoded time (see `decode-time').
Do not sum up minutest of a project within an hour if dont-sum is non-nil.
Interactively do not sum if called with prefix arg."
          (interactive (list
                        (decode-time (org-read-date t t nil "Start time:" '(0 0)))
                        (decode-time (org-read-date t t nil "End time:"))
                        current-prefix-arg))
          (org-time-sheet-shedule (org-time-sheet-collect tStart tEnd) (called-interactively-p 'any) dont-sum))

        (defun org-time-sheet-collect (tStart tEnd)
          "Returns ordered time sheet collection of current buffer
for clocked items with start time within the range from tStart to tEnd."
          (if (> (length tStart) 4)
              (setq tStart (apply 'encode-time tStart)))
          (if (> (length tEnd) 4)
              (setq tEnd (apply 'encode-time tEnd)))
          (let ((tree (org-element-parse-buffer)))
            (cl-stable-sort
             (org-element-map tree 'clock
               (lambda (clock)
                 ;; get the relevant data of the clocks
                 (let* ((timestamp (plist-get (cadr clock) :value))
                        (parent clock)
                        (headers (nreverse (cl-loop while (setq parent (org-element-parent parent 'headline)) collect (car (plist-get (cadr parent) :title))))))
                   (cl-assert timestamp nil "Clock line without timestamp")
                   (when (and (or (null tStart) (null (time-less-p (org-element-timestamp-to-time timestamp 'start t) tStart)))
                              (or (null tEnd) (time-less-p (org-element-timestamp-to-time timestamp 'end t) tEnd)))
                     (list (org-element-timestamp-to-time timestamp 'start)
                           (org-element-timestamp-to-time timestamp 'end)
                           headers))
                   )))
             #'time-less-p
             :key (lambda (clock) (apply 'encode-time (car clock))))))

        (defun org-time-sheet-shedule (clocks &optional interactive dont-sum)
          "Creates time sheet shedule from ordered time sheet clock collection (see `org-time-sheet-collect')."
          ;; sheduling
          (when clocks
            (setq clocks (cons nil clocks))
            (let* ((start (copy-sequence (caadr clocks)))
                   (day-month-year (time-day-month-year start))
                   (shedule (list (list (apply org-time-sheet-date-formatter day-month-year)))))
              (setf (nth 1 start) 0) ;; clear minutes
              (while (cdr clocks)
                (let ((end (decoded-time-advance start '(0 0 1 0 0 0)))
                      project-alist
                      (iter clocks))
                  (while (decoded-time-less-p (cl-caadr iter) end) ;; collect clocks starting before the end of current hour
                    (let* ((start-time (cl-caadr iter))
                           (end-time (cl-cadadr iter))
                           (minutes-start (if (decoded-time-less-p start-time start) start start-time))
                           (minutes-end (if (decoded-time-less-p end end-time) end end-time))
                           (minutes (/ (nth 1 (time-subtract (apply 'encode-time minutes-end) (apply 'encode-time minutes-start))) 60))
                           (headlines (nth 2 (cadr iter)))
                           (project (assoc headlines project-alist)))
                      (if (and project (null dont-sum))
                          (setcdr project (list (+ (cadr project) minutes) minutes-start minutes-end))
                          (setq project-alist (cons (list headlines minutes minutes-start minutes-end) project-alist)))
                      (if (decoded-time-less-p end end-time)
                          (setq iter (cdr iter))
                          ;; delete clock that also finishes in this hour:
                          (setcdr iter (cddr iter))) ;; delete clock entry
                      ))
                  (setq project-alist (nreverse project-alist))
                  ;; Compose shedule for hour:
                  (while project-alist
                    (let ((headlines (caar project-alist))
                          (minutes (nth 1 (car project-alist)))
                          (minutes-start (nth 2 (car project-alist)))
                          (minutes-end (nth 3 (car project-alist))))
                      (setq shedule (cons (funcall org-time-sheet-time-formatter minutes-start minutes-end (nth 2 start) minutes headlines) shedule)))
                    (setq project-alist (cdr project-alist)))
                  ;; calculate new time:
                  (when (cdr clocks)
                    (let ((next-hour-start-time (decoded-time-advance start '(0 0 1 0 0 0)))
                          (next-hour-end-time (decoded-time-advance start '(0 0 2 0 0 0))))
                      (setq start (copy-sequence (caadr clocks)))
                      (setf (nth 1 start) 0) ;; minutes
                      (when (decoded-time-less-p start next-hour-end-time)
                        (setq start next-hour-start-time))
                      (let ((new-day-month-year (time-day-month-year start)))
                        (unless (equal day-month-year new-day-month-year)
                          (setq shedule (cons (list (apply org-time-sheet-date-formatter new-day-month-year)) shedule)
                                day-month-year new-day-month-year)))))))
              (setq shedule (nreverse shedule))
              (when interactive
                (insert (with-temp-buffer
                          (insert "#+begin_src emacs-lisp\n#+end_src\n")
                          (let ((pt (point)))
                            (org-babel-insert-result shedule)
                            (delete-region (point-min) pt))
                          (buffer-string))))
              shedule)))))
    )


      (testing

       (org-entry-run-associated-clock
        (buffer-file-name))

       (org-entry-run-associated-clock
        "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")

       (org-markers-associated-to-file
        (buffer-file-name))

       (org-clock-entry-associated-to-file-p
        (buffer-file-name))

       ;; sharad
       (setq test-info-entry
             (let ((xfile (buffer-file-name)))
               (org-with-clock-position (list org-clock-marker)
                 (org-previous-visible-heading 1)
                 (let ((info (org-entry-collect-task-info)))
                   (if (funcall org-clocking-api-entry-associated-to-file-p info xfile)
                       info))))
             )

       (funcall org-clocking-api-entry-associated-to-file-p (org-clock-entry-current-entry) (buffer-file-name))




       (test-info-entry)
       (funcall org-clocking-api-entry-associated-to-file-p test-info-entry (buffer-file-name))

       ;; org-clock-marker
       (org-entries-associated-key-fn-value :current-clock test-info-entry (buffer-file-name))

       (org-clock-entry-associated-to-file-p
        "~/Docume1nts/CreatedContent/contents/org/tasks/meru/report.org")

       (org-clock-entry-associated-to-file-p
        "~/Documents/CreatedContent/contents/org/tasks/meru/features/patch-mgm/todo.org")

       ;; (org-entry-associated-file-org-file-p
       ;;  "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"
       ;;  (cadr org-entry-list-task-infos)))


       (length
        (funcall org-clocking-api-entries-associated-to-file-p (buffer-file-name))
        )

       (length (funcall org-clocking-api-entries-associated-to-file-p "/home/s/paradise/releases/global/patch-upgrade/Makefile"))

       (org-markers-associated-to-file (buffer-file-name))
       (length
        (funcall org-clocking-api-entries-associated-to-file-p "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")
        )

       (org-entries-associated-to-file-by-keys-p (buffer-file-name))

       (length
        (org-entries-associated-to-file-by-keys-p "/home/s/paradise/releases/global/patch-upgrade/Makefile")
        )

       (org-clock-entry-associated-to-file-p "/home/s/paradise/releases/global/patch-upgrade/Makefile")

       ;; (org-entry-associated-to-file-by-keys-p "/home/s/paradise/releases/global/patch-upgrade/Makefile")

       (if (org-clock-entry-associated-to-file-p (buffer-file-name))
         (message "current clock is with file")
       ))

  (define-minor-mode office-mode
      "Prepare for working with collarative office project. This
is the mode to be enabled when I am working in some files on
which other peoples are also working."
    :initial-value nil
    :lighter " Office"
    :global nil
    (condition-case e
        (when office-mode
          (message "calling office mode")
          (if (or (eq major-mode 'c-mode)
                  (eq major-mode 'c++-mode))
              (c-set-style "stroustrup" 1))
          (set (make-local-variable 'before-save-hook) before-save-hook)
          (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
          (message "called office mode"))
      (error (message "Error: %s" e))))

  )


(use-package timesheet
    ;; https://github.com/tmarble/timesheet.el
    :ensure t
    :config
    '())

(when nil
 (use-package wakatime-mode
    ;; https://github.com/tmarble/timesheet.el
    :ensure t
    :config
    (global-wakatime-mode)))



(provide 'office-config)
