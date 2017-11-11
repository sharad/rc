;;; org-context-clock.el --- org-context-clock               -*- lexical-binding: t; -*-

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

(defgroup org-context-clock nil
  "Emacs Org Context Clocking."
  :tag "Org Clock"
  :group 'org-progress)


(require 'org-clock)
(require 'org-context-clock-api)



(defvar *org-context-clock-task-current-context*  nil)
(defvar *org-context-clock-task-previous-context* nil)
(defvar *org-context-clock-task-current-context-time* 2)
(defvar *org-context-clock-last-buffer-select-time* (current-time))
(defvar *org-context-clock-buffer-select-timer* nil)
(defvar *org-context-clock-update-current-context-msg* "")
;; (defvar org-context-clock-api-name :predicate "API")
(defvar org-context-clock-access-api-name :recursive "Aceess API")
(defvar org-context-clock-assoc-api-name :keys "Assoc API")
(defvar org-context-clock-api-tasks-associated-to-context     (org-context-clock-access-api-get org-context-clock-access-api-name :tasks))
(defvar org-context-clock-api-task-associated-to-context-p    (org-context-clock-assoc-api-get org-context-clock-assoc-api-name :taskp))
(defvar org-context-clock-api-task-update-tasks               (org-context-clock-access-api-get org-context-clock-access-api-name :update))
(defvar org-context-clock-api-task-update-files               (org-context-clock-access-api-get org-context-clock-access-api-name :files))


(defun custom-plist-keys (in-plist)
  (if (null in-plist)
      in-plist
      (cons (car in-plist) (custom-plist-keys (cddr in-plist)))))

;;;###autoload
(defun org-context-clock-api ()
  "org task clocking select api to use."
  (interactive)
  (let* ((assoc-api-keys (custom-plist-keys org-context-clock-task-clocking-assoc-api))
         (assoc-api-name (ido-completing-read
                          "org task clocking api name: "
                          (mapcar 'symbol-name assoc-api-keys)
                          nil
                          t
                          (symbol-name org-context-clock-assoc-api-name)))
         (assoc-api-key (intern assoc-api-name))

         (access-api-keys (custom-plist-keys org-context-clock-task-clocking-access-api))
         (access-api-name (ido-completing-read
                          "org task clocking api name: "
                          (mapcar 'symbol-name access-api-keys)
                          nil
                          t
                          (symbol-name org-context-clock-access-api-name)))
         (access-api-key (intern access-api-name)))
    (setq
     org-context-clock-assoc-api-name assoc-api-key
     org-context-clock-access-api-name access-api-key)
    (if (and
         (org-context-clock-access-api-get org-context-clock-access-api-name :tasks)
         (org-context-clock-assoc-api-get org-context-clock-assoc-api-name :taskp)
         (org-context-clock-access-api-get org-context-clock-access-api-name :update))
        (setq
         org-context-clock-api-tasks-associated-to-context   (org-context-clock-access-api-get org-context-clock-access-api-name :tasks)
         org-context-clock-api-task-associated-to-context-p  (org-context-clock-assoc-api-get org-context-clock-assoc-api-name :taskp)
         org-context-clock-api-task-update-tasks             (org-context-clock-access-api-get org-context-clock-access-api-name :update)))))

;;;###autoload
(defun org-context-clock-task-update-tasks (&optional force)
  "Update task infos"
  (interactive "P")
  (funcall org-context-clock-api-task-update-tasks force))

;;;###autoload
(defun org-context-clock-task-update-files (&optional force)
  "Update task infos"
  (interactive "P")
  (funcall org-context-clock-api-task-update-files force))

(defun org-context-clock-build-context (&optional buff)
  (let* ((buff (if buff
                   (if (bufferp buff)
                       buff
                       (if (stringp buff)
                           (or
                            (get-buffer buff)
                            (if (file-exists-p buff)
                                (get-file-buffer buff)))))
                   (window-buffer)))
         (file (buffer-file-name buff))
         (context (list :file file :buffer buff)))
    context))

;;;###autoload
(defun org-context-clock-update-current-context ()
  (if (> (float-time
          (time-since *org-context-clock-last-buffer-select-time*))
         *org-context-clock-task-current-context-time*)
      (let* ((context (org-context-clock-build-context))
             (buff          (plist-get context :buffer)))
        (if (and buff
                 (buffer-live-p buff)
                 (not (minibufferp buff))
                 (not (and
                       (equal *org-context-clock-task-previous-context* context)
                       (equal *org-context-clock-task-current-context*  context))))
            (progn
              (setq
               *org-context-clock-task-previous-context* *org-context-clock-task-current-context*
               *org-context-clock-task-current-context*  context)

              (if (> (org-context-clock-task-associated-to-context-p context) 0)
                  (org-context-clock-debug "org-context-clock-update-current-context: Current task already associate to %s" context)
                  (progn
                    (org-context-clock-debug "org-context-clock-update-current-context: Now really going to clock.")
                    (org-context-clock-task-run-associated-clock context)
                    (org-context-clock-debug "org-context-clock-update-current-context: Now really clock done."))))

            (org-context-clock-debug "org-context-clock-update-current-context: context %s not suitable to associate" context)))
      (org-context-clock-debug "org-context-clock-update-current-context: not enough time passed.")))


(defun org-context-clock-update-current-context-x ()
  (if t
      (let* ((context (org-context-clock-build-context)))
        (unless nil
          (setq
           *org-context-clock-task-previous-context* *org-context-clock-task-current-context*
           *org-context-clock-task-current-context*  context)

          (unless (org-context-clock-task-associated-to-context-p context)
            (org-context-clock-task-run-associated-clock context))))))

;;;###autoload
(defun org-context-clock-task-current-task ()
  (and
   ;; file
   org-clock-marker
   (> (marker-position-nonil org-clock-marker) 0)
   (org-with-clock-position (list org-clock-marker)
     (org-previous-visible-heading 1)
     (let ((info (org-context-clock-collect-task)))
       info))))

;; not workiong
;; (defun org-context-clock-task-associated-to-context-p (context)
;;   (and
;;    ;; file
;;    org-clock-marker
;;    (> (marker-position-nonil org-clock-marker) 0)
;;    (org-with-clock-position (list org-clock-marker)
;;      (org-previous-visible-heading 1)
;;      (let ((info (org-context-clock-collect-task)))
;;        (if (funcall org-context-clock-api-task-associated-to-context-p info context)
;;            info)))))

;;;###autoload
(defun org-context-clock-task-associated-to-context-p (context)
  (let ((info (org-context-clock-task-current-task)))
    (if info
        (funcall org-context-clock-api-task-associated-to-context-p info context)
        0)))

;;;###autoload
(defun org-context-clock-task-run-associated-clock (context)
  (interactive
   (list (org-context-clock-build-context)))
  (let ()
    (let* ((matched-clocks
            (remove-if-not
             #'(lambda (marker) (marker-buffer marker))
             (org-context-clock-markers-associated-to-context context)))
           (selected-clock (if (> (length matched-clocks) 1)
                               (org-context-clock-select-task-from-clocks matched-clocks)
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

              (setq *org-context-clock-update-current-context-msg* org-clock-marker)

              (with-current-buffer (marker-buffer selected-clock)
                (let ((buffer-read-only nil))
                  (org-clock-clock-in (list selected-clock))))

              (if prev-org-clock-buff
                  (with-current-buffer prev-org-clock-buff
                    (setq buffer-read-only prev-clock-buff-read-only)))))
          (progn
            (setq *org-context-clock-update-current-context-msg* "null clock")
            (message "No clock found please set a match for this context %s, add it using M-x org-context-clock-add-context-to-org-heading."
                     context)
            (org-context-clock-add-context-to-org-heading-when-idle context 17))))))

;;;###autoload
(defun org-context-clock-run-task-current-context-timer ()
  (interactive)
  (let ()
    (setq *org-context-clock-last-buffer-select-time* (current-time))
    (when *org-context-clock-buffer-select-timer*
      (cancel-timer *org-context-clock-buffer-select-timer*)
      (setq *org-context-clock-buffer-select-timer* nil))
    (setq *org-context-clock-buffer-select-timer*
          ;; distrubing while editing.
          ;; (run-with-timer
          (run-with-idle-timer
           (1+ *org-context-clock-task-current-context-time*)
           nil
           'org-context-clock-update-current-context))))

;;;###autoload
(defun org-context-clock-select-task-from-clocks (clocks &optional prompt)
  "Select a task that was recently associated with clocking."
  (interactive)
  (let (och chl sel-list rpl (i 0) s)
    ;; Remove successive dups from the clock history to consider
    (mapc (lambda (c) (if (not (equal c (car och))) (push c och)))
          clocks)
    (setq och (reverse och) chl (length och))
    (if (zerop chl)
        (user-error "No matched org heading")
        (save-window-excursion
          (org-switch-to-buffer-other-window
           (get-buffer-create "*Clock Task Select*"))
          (erase-buffer)
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
(defun org-context-clock-insinuate ()
  (interactive)
  (progn
    (add-hook 'buffer-list-update-hook     'org-context-clock-run-task-current-context-timer)
    (add-hook 'elscreen-screen-update-hook 'org-context-clock-run-task-current-context-timer)
    (add-hook 'elscreen-goto-hook          'org-context-clock-run-task-current-context-timer)))

;;;###autoload
(defun org-context-clock-uninsinuate ()
  (interactive)
  (remove-hook 'buffer-list-update-hook 'org-context-clock-run-task-current-context-timer)
  (setq buffer-list-update-hook nil)
  (remove-hook 'elscreen-screen-update-hook 'org-context-clock-run-task-current-context-timer)
  (remove-hook 'elscreen-goto-hook 'org-context-clock-run-task-current-context-timer))


(progn ;; "Org task clock reporting"
  ;; #+BEGIN: task-clock-report-with-comment :parameter1 value1 :parameter2 value2 ...
  ;; #+END:
  (defun org-dblock-write:task-clock-report-with-comment (params)
    (let ((fmt (or (plist-get params :format) "%d. %m. %Y")))
      (insert "Last block update at: "
              (format-time-string fmt))))

  (progn ;; "time sheet"
    ))








(when nil                               ;testing

  (org-context-clock-task-run-associated-clock (org-context-clock-build-context))

  (org-context-clock-task-run-associated-clock
   (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")))

  (org-context-clock-markers-associated-to-context
   (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")))

  (org-context-clock-task-associated-to-context-p
   (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")))

  (org-context-clock-markers-associated-to-context (org-context-clock-build-context))

  (org-context-clock-task-associated-to-context-p (org-context-clock-build-context))

  ;; sharad
  (setq test-info-task
        (let ((xcontext
               (list
                :file (buffer-file-name)
                :buffer (current-buffer))))
          (org-with-clock-position (list org-clock-marker)
            (org-previous-visible-heading 1)
            (let ((info (org-context-clock-collect-task)))
              (if (funcall org-context-clock-api-task-associated-to-context-p info xcontext)
                  info)))))

  (funcall org-context-clock-api-task-associated-to-context-p
           (org-context-clock-task-current-task)
           (org-context-clock-build-context))




  ;; (test-info-task)

  (funcall org-context-clock-api-task-associated-to-context-p
           test-info-task
           (org-context-clock-build-context))

  ;; org-clock-marker
  (org-tasks-associated-key-fn-value
   :current-clock test-info-task
   (org-context-clock-build-context) )

  (org-context-clock-task-associated-to-context-p
   (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")))

  (org-context-clock-task-associated-to-context-p
   (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/features/patch-mgm/todo.org")))

  ;; (org-task-associated-context-org-context-p
  ;;  "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"
  ;;  (cadr org-task-list-tasks)))


  (length
   (funcall org-context-clock-api-tasks-associated-to-context
            (org-context-clock-build-context)))

  (length
   (funcall org-context-clock-api-tasks-associated-to-context
            (org-context-clock-build-context (find-file-noselect "/home/s/paradise/releases/global/patch-upgrade/Makefile"))))

  (org-context-clock-markers-associated-to-context (org-context-clock-build-context))

  (length
   (funcall org-context-clock-api-tasks-associated-to-context
            (org-context-clock-build-context (find-file-noselect "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"))))

  (length
   (org-context-clock-tasks-associated-to-context-by-keys
    (org-context-clock-build-context)))


  (length
   (org-context-clock-tasks-associated-to-context-by-keys
    (org-context-clock-build-context (find-file-noselect "/home/s/paradise/releases/global/patch-upgrade/Makefile"))))

  (org-context-clock-task-associated-to-context-p
   (org-context-clock-build-context (find-file-noselect "/home/s/paradise/releases/global/patch-upgrade/Makefile")))

  ;; (org-context-clock-task-associated-to-context-by-keys "/home/s/paradise/releases/global/patch-upgrade/Makefile")

  (if (org-context-clock-task-associated-to-context-p
       (org-context-clock-build-context))
      (message "current clock is with current context or file")))

(provide 'org-context-clock)
;;; org-context-clock.el ends here
