;;; occ-scratch-space.el --- occ scratch space       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
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

(provide 'occ-scratch-space)




(progn  ;; http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/
  (progn
    (ert-deftest sacha/org-capture-prefill-template ()
      (should
       ;; It should fill things in one field at a time
       (string=
        (sacha/org-capture-prefill-template
         "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
         "Hello World")
        "* TODO Hello World\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
        ))
      (should
       (string=
        (sacha/org-capture-prefill-template
         "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
         "Hello World" "<2015-01-01>")
        "* TODO Hello World\nSCHEDULED: <2015-01-01>\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"))
      (should
       (string=
        (sacha/org-capture-prefill-template
         "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
         "Hello World" "<2015-01-01>" "0:05")
        "* TODO Hello World\nSCHEDULED: <2015-01-01>\n:PROPERTIES:\n:Effort: 0:05\n:END:\n%?\n")))

    (defun sacha/org-capture-prefill-template (template &rest values)
      "Pre-fill TEMPLATE with VALUES."
      (setq template (or template (org-capture-get :template)))
      (with-temp-buffer
        (insert template)
        (goto-char (point-min))
        (while (re-search-forward
                (concat "%\\("
                        "\\[\\(.+\\)\\]\\|"
                        "<\\([^>\n]+\\)>\\|"
                        "\\([tTuUaliAcxkKInfF]\\)\\|"
                        "\\(:[-a-zA-Z]+\\)\\|"
                        "\\^\\({\\([^}]*\\)}\\)"
                        "?\\([gGtTuUCLp]\\)?\\|"
                        "%\\\\\\([1-9][0-9]*\\)"
                        "\\)") nil t)
          (if (car values)
              (replace-match (car values) nil t))
          (setq values (cdr values)))
        (buffer-string)))

    (defun sacha/helm-org-create-task (candidate)
      (let ((entry (org-capture-select-template "T")))
        (org-capture-set-plist entry)
        (org-capture-get-template)
        (org-capture-set-target-location)
        (condition-case error
            (progn
              (org-capture-put
               :template
               (org-capture-fill-template
                (sacha/org-capture-prefill-template (org-capture-get :template)
                                                    candidate)))
              (org-capture-place-template
               (equal (car (org-capture-get :target)) 'function)))
          ((error quit)
           (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
           (error "Capture abort: %s" error)))) t))


  (progn
    (defvar sacha/helm-org-refile-locations nil)

    (defun sacha/helm-org-clock-in-and-track-from-refile (candidate)
      (let ((location (org-refile--get-location candidate sacha/helm-org-refile-locations)))
        (save-window-excursion
          (org-refile 4 nil location)
          (sacha/org-clock-in-and-track)
          t)))

    (defun sacha/helm-org-refile-read-location (tbl)
      (setq sacha/helm-org-refile-locations tbl)
      (helm
       (list
        (helm-build-sync-source "Refile targets"
          :candidates (mapcar 'car tbl)
          :action '(("Select" . identity)
                    ("Clock in and track" . sacha/helm-org-clock-in-and-track-from-refile))
          :history 'org-refile-history)
        (helm-build-dummy-source "Create task"
          :action (helm-make-actions
                   "Create task"
                   'sacha/helm-org-create-task)))))

    (defun sacha/org-refile-get-location (&optional prompt default-buffer new-nodes no-exclude)
      "Prompt the user for a refile location, using PROMPT.
PROMPT should not be suffixed with a colon and a space, because
this function appends the default value from
`org-refile-history' automatically, if that is not empty.
When NO-EXCLUDE is set, do not exclude headlines in the current subtree,
this is used for the GOTO interface."
      (let ((org-refile-targets org-refile-targets)
            (org-refile-use-outline-path org-refile-use-outline-path)
            excluded-entries)
        (when (and (derived-mode-p 'org-mode)
                   (not org-refile-use-cache)
                   (not no-exclude))
          (org-map-tree
           (lambda()
             (setq excluded-entries
                   (append excluded-entries (list (org-get-heading t t)))))))
        (setq org-refile-target-table
              (org-refile-get-targets default-buffer excluded-entries)))
      (unless org-refile-target-table
        (user-error "No refile targets"))
      (let* ((cbuf (current-buffer))
             (partial-completion-mode nil)
             (cfn (buffer-file-name (buffer-base-buffer cbuf)))
             (cfunc (if (and org-refile-use-outline-path
                             org-outline-path-complete-in-steps)
                        'org-olpath-completing-read
                      'org-icompleting-read))
             (extra (if org-refile-use-outline-path "/" ""))
             (cbnex (concat (buffer-name) extra))
             (filename (and cfn (expand-file-name cfn)))
             (tbl (mapcar
                   (lambda (x)
                     (if (and (not (member org-refile-use-outline-path
                                           '(file full-file-path)))
                              (not (equal filename (nth 1 x))))
                         (cons (concat (car x) extra " ("
                                       (file-name-nondirectory (nth 1 x)) ")")
                               (cdr x))
                       (cons (concat (car x) extra) (cdr x))))
                   org-refile-target-table))
             (completion-ignore-case t)
             cdef
             (prompt (concat prompt
                             (or (and (car org-refile-history)
                                      (concat " (default " (car org-refile-history) ")"))
                                 (and (assoc cbnex tbl) (setq cdef cbnex)
                                      (concat " (default " cbnex ")"))) ": "))
             pa answ parent-target child parent old-hist)
        (setq old-hist org-refile-history)
        ;; Use Helm's sources instead
        (setq answ (sacha/helm-org-refile-read-location tbl))
        (if (and (stringp answ)
                 (setq pa (org-refile--get-location answ tbl)))
            (progn
              (org-refile-check-position pa)
              (when (or (not org-refile-history)
                        (not (eq old-hist org-refile-history))
                        (not (equal (car pa) (car org-refile-history))))
                (setq org-refile-history
                      (cons (car pa) (if (assoc (car org-refile-history) tbl)
                                         org-refile-history
                                       (cdr org-refile-history))))
                (if (equal (car org-refile-history) (nth 1 org-refile-history))
                    (pop org-refile-history)))
              pa)
          (if (and (stringp answ) (string-match "\\`\\(.*\\)/\\([^/]+\\)\\'" answ))
              (progn
                (setq parent (match-string 1 answ)
                      child (match-string 2 answ))
                (setq parent-target (org-refile--get-location parent tbl))
                (when (and parent-target
                           (or (eq new-nodes t)
                               (and (eq new-nodes 'confirm)
                                    (y-or-n-p (format "Create new node \"%s\"? "
                                                      child)))))
                  (org-refile-new-child parent-target child)))
            (if (not (equal answ t)) (user-error "Invalid target location"))))))

    (fset 'org-refile-get-location 'sacha/org-refile-get-location)
    ))



;;; occ-scratch-space.el ends here
