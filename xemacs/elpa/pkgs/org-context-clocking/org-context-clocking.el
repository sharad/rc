;;; org-context-clocking.el --- org-context-clocking               -*- lexical-binding: t; -*-

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

(defgroup org-context-clocking nil
  "Emacs Org Context Clocking."
  :tag "Org Clock"
  :group 'org-progress)


(require 'org-clock)
(require 'org-context-clocking-api)



(defvar org-context-clocking-task-current-context-plist  nil)
(defvar org-context-clocking-task-previous-context-plist nil)
(defvar org-context-clocking-task-current-context-plist-time 2)
(defvar org-context-clocking-last-buffer-select-time (current-time))
(defvar org-context-clocking-buffer-select-timer nil)
(defvar org-context-clocking-update-current-context-plist-msg "")
;; (defvar org-context-clocking-api-name :predicate "API")
(defvar org-context-clocking-api-name :keys "API")
(defvar org-context-clocking-api-entries-associated-to-context-plist (org-context-clocking-api-get org-context-clocking-api-name :entries))
(defvar org-context-clocking-api-entry-associated-to-context-plist-p   (org-context-clocking-api-get org-context-clocking-api-name :entryp))
(defvar org-context-clocking-api-entry-update-task-infos      (org-context-clocking-api-get org-context-clocking-api-name :update))


(defun custom-plist-keys (in-plist)
  (if (null in-plist)
      in-plist
      (cons (car in-plist) (custom-plist-keys (cddr in-plist)))))

;; defun org-task-clocking-api
(defun org-context-clocking-api ()
  "org task clocking select api to use."
  (interactive)
  (let* ((api-keys (custom-plist-keys org-context-clocking-entry-clocking-api))
         (api-name (ido-completing-read
                    "org task clocking api name: "
                    (mapcar 'symbol-name api-keys)
                    nil
                    t
                    (symbol-name org-context-clocking-api-name)))
         (api-key (intern api-name)))
    (setq org-context-clocking-api-name api-key)
    (if (and
         (org-context-clocking-api-get org-context-clocking-api-name :entries)
         (org-context-clocking-api-get org-context-clocking-api-name :entryp)
         (org-context-clocking-api-get org-context-clocking-api-name :update))
        (setq
         org-context-clocking-api-entries-associated-to-context-plist (org-context-clocking-api-get org-context-clocking-api-name :entries)
         org-context-clocking-api-entry-associated-to-context-plist-p   (org-context-clocking-api-get org-context-clocking-api-name :entryp)
         org-context-clocking-api-entry-update-task-infos      (org-context-clocking-api-get org-context-clocking-api-name :update)))))

;; (defun org-clocking-entry-update-task-infos (&optional force)
(defun org-context-clocking-entry-update-task-infos (&optional force)
  "Update task infos"
  (interactive "P")
  (funcall org-context-clocking-api-entry-update-task-infos force))

(defun org-context-clocking-update-current-context-plist ()
  (if (> (float-time
          (time-since org-context-clocking-last-buffer-select-time))
         org-context-clocking-task-current-context-plist-time)
      (let* ((buff (window-buffer))
             (file (buffer-file-name buff))
             (context-plist (list :file file :buffer buff)))
        (if (or
             (and
              (equal org-context-clocking-task-previous-context-plist context-plist)
              (equal org-context-clocking-task-current-context-plist  context-plist))
             (if buff (minibufferp (plist-get context-plist :buffer)) t))

            (org-context-clocking-debug "org-context-clocking-update-current-context-plist: context-plist %s not suitable to associate" context-plist)

            (progn
              (setq
               org-context-clocking-task-previous-context-plist org-context-clocking-task-current-context-plist
               org-context-clocking-task-current-context-plist  context-plist)

              (if (> (org-context-clocking-entry-associated-to-context-plist-p context-plist) 0)
                  (org-context-clocking-debug "org-context-clocking-update-current-context-plist: Current entry already associate to %s" context-plist)
                  (progn
                    (org-context-clocking-debug "org-context-clocking-update-current-context-plist: Now really going to clock.")
                    (org-context-clocking-entry-run-associated-clock context-plist)
                    (org-context-clocking-debug "org-context-clocking-update-current-context-plist: Now really clock done."))))))
      (org-context-clocking-debug "org-context-clocking-update-current-context-plist: not enough time passed.")))


(defun org-context-clocking-update-current-context-plist-x ()
  (if t
      (let* ((buff (window-buffer))
             (file (buffer-file-name buff))
             (context-plist (list :file file :buffer buff)))
        (unless nil
          (setq
           org-context-clocking-task-previous-context-plist org-context-clocking-task-current-context-plist
           org-context-clocking-task-current-context-plist  context-plist)

          (unless (org-context-clocking-entry-associated-to-context-plist-p context-plist)
            (org-context-clocking-entry-run-associated-clock context-plist))))))

;;(defun org-context-Xclock-entry-current-entry ()
(defun org-context-clocking-entry-current-entry ()
  (and
   ;; file
   org-clock-marker
   (> (marker-position-nonil org-clock-marker) 0)
   (org-with-clock-position (list org-clock-marker)
     (org-previous-visible-heading 1)
     (let ((info (org-context-clocking-entry-collect-task-info)))
       info))))

;; not workiong
;; (defun org-context-clocking-entry-associated-to-context-plist-p (context-plist)
;;   (and
;;    ;; file
;;    org-clock-marker
;;    (> (marker-position-nonil org-clock-marker) 0)
;;    (org-with-clock-position (list org-clock-marker)
;;      (org-previous-visible-heading 1)
;;      (let ((info (org-context-clocking-entry-collect-task-info)))
;;        (if (funcall org-context-clocking-api-entry-associated-to-context-plist-p info context-plist)
;;            info)))))

;; (defun org-clock-entry-associated-to-context-plist-p (context-plist)
(defun org-context-clocking-entry-associated-to-context-plist-p (context-plist)
  (let ((info (org-context-clocking-entry-current-entry)))
    (if info
        (funcall org-context-clocking-api-entry-associated-to-context-plist-p info context-plist)
        0)))

;; (defun org-Xentry-run-associated-clock (context-plist)
(defun org-context-clocking-entry-run-associated-clock (context-plist)
  (let ()
    (let* ((matched-clocks
            (remove-if-not
             #'(lambda (marker) (marker-buffer marker))
             (org-context-clocking-markers-associated-to-context-plist context-plist)))
           (selected-clock (if (> (length matched-clocks) 1)
                               (org-context-clocking-select-task-from-clocks matched-clocks)
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

              (setq org-context-clocking-update-current-context-plist-msg org-clock-marker)

              (with-current-buffer (marker-buffer selected-clock)
                (let ((buffer-read-only nil))
                  (org-clock-clock-in (list selected-clock))))

              (if prev-org-clock-buff
                  (with-current-buffer prev-org-clock-buff
                    (setq buffer-read-only prev-clock-buff-read-only)))))
          (progn
            (setq org-context-clocking-update-current-context-plist-msg "null clock")
            (message "No clock found please set a match fot this context-plist %s." context-plist))))))

;; (defun org-context-clocking-run-task-current-context-plist-timer ()
;;   (let ()
;;     (setq org-context-clocking-last-buffer-select-time (current-time))
;;     (when org-context-clocking-buffer-select-timer
;;       (cancel-timer org-context-clocking-buffer-select-timer)
;;       (setq org-context-clocking-buffer-select-timer nil))
;;     (setq org-context-clocking-buffer-select-timer
;;           (run-with-timer
;;            (1+ org-context-clocking-task-current-context-plist-time)
;;            nil
;;            'org-context-clocking-update-current-context-plist))))

;; (defun run-org-Xcontext-clocking-task-current-context-plist-timer ()
(defun org-context-clocking-run-task-current-context-plist-timer ()
  (let ()
    (setq org-context-clocking-last-buffer-select-time (current-time))
    (when org-context-clocking-buffer-select-timer
      (cancel-timer org-context-clocking-buffer-select-timer)
      (setq org-context-clocking-buffer-select-timer nil))
    (setq org-context-clocking-buffer-select-timer
          ;; distrubing while editing.
          ;; (run-with-timer
          (run-with-idle-timer
           (1+ org-context-clocking-task-current-context-plist-time)
           nil
           'org-context-clocking-update-current-context-plist))))

;; (defun org-Xentries-select (entries &optional prompt)
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

;; (defun org-clock-Xselect-task-from-clocks (clocks &optional prompt)
(defun org-context-clocking-select-task-from-clocks (clocks &optional prompt)
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

;;;###autoload
(defun org-context-clocking-insinuate ()
  (interactive)
  (progn
    (add-hook 'buffer-list-update-hook     'org-context-clocking-run-task-current-context-plist-timer)
    (add-hook 'elscreen-screen-update-hook 'org-context-clocking-run-task-current-context-plist-timer)
    (add-hook 'elscreen-goto-hook          'org-context-clocking-run-task-current-context-plist-timer)))

;;;###autoload
(defun org-context-clocking-uninsinuate ()
  (interactive)
  (remove-hook 'buffer-list-update-hook 'org-context-clocking-run-task-current-context-plist-timer)
  (setq buffer-list-update-hook nil)
  (remove-hook 'elscreen-screen-update-hook 'org-context-clocking-run-task-current-context-plist-timer)
  (remove-hook 'elscreen-goto-hook 'org-context-clocking-run-task-current-context-plist-timer))


(progn ;; "Org task clock reporting"
  ;; #+BEGIN: task-clock-report-with-comment :parameter1 value1 :parameter2 value2 ...
  ;; #+END:
  (defun org-dblock-write:task-clock-report-with-comment (params)
    (let ((fmt (or (plist-get params :format) "%d. %m. %Y")))
      (insert "Last block update at: "
              (format-time-string fmt))))

  (progn ;; "time sheet"
    ))








(when nil

  (org-context-clocking-entry-run-associated-clock
   (buffer-file-name))

  (org-context-clocking-entry-run-associated-clock
   (list
    :file "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"
    :buffer (find-buffer-visiting "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")))

  (org-context-clocking-markers-associated-to-context-plist
   (list
    :file "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"
    :buffer (find-buffer-visiting "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")))

  (org-context-clocking-entry-associated-to-context-plist-p
   (list
    :file "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"
    :buffer (find-buffer-visiting "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")))

  (org-context-clocking-markers-associated-to-context-plist
   (buffer-file-name))

  (org-context-clocking-entry-associated-to-context-plist-p
   (list
    :file (buffer-file-name)
    :buffer (current-buffer)))

  ;; sharad
  (setq test-info-entry
        (let ((xcontext-plist
               (list
                :file (buffer-file-name)
                :buffer (current-buffer))))
          (org-with-clock-position (list org-clock-marker)
            (org-previous-visible-heading 1)
            (let ((info (org-context-clocking-entry-collect-task-info)))
              (if (funcall org-context-clocking-api-entry-associated-to-context-plist-p info xcontext-plist)
                  info)))))

  (funcall org-context-clocking-api-entry-associated-to-context-plist-p
           (org-context-clocking-entry-current-entry)
           (list
            :file (buffer-file-name)
            :buffer (current-buffer)))




  (test-info-entry)
  (funcall org-context-clocking-api-entry-associated-to-context-plist-p
           test-info-entry
           (list
            :file (buffer-file-name)
            :buffer (current-buffer)))

  ;; org-clock-marker
  (org-entries-associated-key-fn-value
   :current-clock test-info-entry
   (list
    :file (buffer-file-name)
    :buffer (current-buffer)) )

  (org-context-clocking-entry-associated-to-context-plist-p
   (list
    :file "~/Docume1nts/CreatedContent/contents/org/tasks/meru/report.org"
    :buffer (find-buffer-visiting
             "~/Docume1nts/CreatedContent/contents/org/tasks/meru/report.org")))

  (org-context-clocking-entry-associated-to-context-plist-p
   (list
    :file "~/Documents/CreatedContent/contents/org/tasks/meru/features/patch-mgm/todo.org"
    :buffer (find-buffer-visiting
             "~/Documents/CreatedContent/contents/org/tasks/meru/features/patch-mgm/todo.org")))

  ;; (org-entry-associated-context-plist-org-context-plist-p
  ;;  "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"
  ;;  (cadr org-entry-list-task-infos)))


  (length
   (funcall org-context-clocking-api-entries-associated-to-context-plist
            (list
             :file (buffer-file-name)
             :buffer (current-buffer))))

  (length
   (funcall org-context-clocking-api-entries-associated-to-context-plist
            (list
             :file "/home/s/paradise/releases/global/patch-upgrade/Makefile"
             :buffer (find-buffer-visiting
                      "/home/s/paradise/releases/global/patch-upgrade/Makefile"))))

  (org-context-clocking-markers-associated-to-context-plist (buffer-file-name))
  (length
   (funcall org-context-clocking-api-entries-associated-to-context-plist
            (list
             :file "~/Docume1nts/CreatedContent/contents/org/tasks/meru/report.org"
             :buffer (find-buffer-visiting
                      "~/Docume1nts/CreatedContent/contents/org/tasks/meru/report.org"))))

  (length
   (org-context-clocking-entries-associated-to-context-plist-by-keys
    (list
     :file (buffer-file-name)
     :buffer (current-buffer))))


  (length
   (org-context-clocking-entries-associated-to-context-plist-by-keys
    (list
     :file "/home/s/paradise/releases/global/patch-upgrade/Makefile"
     :buffer (find-buffer-visiting
              "/home/s/paradise/releases/global/patch-upgrade/Makefile")))
   )

  (org-context-clocking-entry-associated-to-context-plist-p
   (list
    :file "/home/s/paradise/releases/global/patch-upgrade/Makefile"
    :buffer (find-buffer-visiting
             "/home/s/paradise/releases/global/patch-upgrade/Makefile")))

  ;; (org-context-clocking-entry-associated-to-context-plist-by-keys "/home/s/paradise/releases/global/patch-upgrade/Makefile")

  (if (org-context-clocking-entry-associated-to-context-plist-p
       (list
        :file (buffer-file-name)
        :buffer (current-buffer)))
      (message "current clock is with current context or file")))

(provide 'org-context-clocking)
;;; org-context-clocking.el ends here
