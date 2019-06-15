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


;; check about function (org-refile-new-child)

;; org-capture-templates


(when nil
  (progn
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgd593b27
    ;; Self-tracking, statistics, and other data transformations
    ;; Quantified Awesome

    (defmacro my/org-with-current-task (&rest body)
      "Execute BODY with the point at the subtree of the current task."
      `(if (derived-mode-p 'org-agenda-mode)
           (save-window-excursion
             (org-agenda-switch-to)
             ,@body)
         ,@body))

    (defun my/org-clock-in-and-track ()
      "Start the clock running. Clock into Quantified Awesome."
      (interactive)
      (my/org-with-current-task
       (org-clock-in)
       (call-interactively 'my/org-quantified-track)
       (when (org-entry-get (point) "AUTO")
         (org-open-link-from-string (org-entry-get (point) "AUTO")))))
    (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)

    (defmacro my/with-org-task (&rest body)
      "Run BODY within the current agenda task, clocked task, or cursor task."
      `(cond
        ((derived-mode-p 'org-agenda-mode)
         (let* ((marker (org-get-at-bol 'org-marker))
                (buffer (marker-buffer marker))
                (pos (marker-position marker)))
           (with-current-buffer buffer
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char pos)
                 ,@body)))))
        ((and (derived-mode-p 'org-mode) (org-at-heading-p)) (save-excursion ,@body))
        ((org-clocking-p) (save-excursion (org-clock-goto) ,@body))
        ((derived-mode-p 'org-mode) ,@body)))

    (defun my/org-quantified-track (&optional category note)
      "Create a tracking record using CATEGORY and NOTE.
Default to the current task in the agenda, the currently-clocked
entry, or the current subtree in Org."
      (interactive (list nil nil))
      (unless (and category note)
        (my/with-org-task
         (setq category (or category
                            (org-entry-get-with-inheritance "QUANTIFIED")))
         (cond
          ((null category)
           (setq category (read-string "Category: "))
           (org-set-property "QUANTIFIED" category))
          ((string= category "ask")
           (setq category (read-string "Category: "))))
         (setq note
               (concat
                (if (string= (or (org-entry-get-with-inheritance "QUANTIFIEDQUIET") "") "t")
                    "!private "
                  "")
                (or note (elt (org-heading-components) 4) (read-string "Note: "))))))
      (quantified-track (concat category " | " note)))

    (defun my/org-quick-clock-in-task (location jump)
      "Track and clock in on the specified task.
If JUMP is non-nil or the function is called with the prefix argument, jump to that location afterwards."
      (interactive (list (save-excursion (my/org-refile-get-location "Location")) current-prefix-arg))
      (when location
        (if jump
            (progn (org-refile 4 nil location) (my/org-clock-in-and-track))
          (save-window-excursion
            (org-refile 4 nil location)
            (my/org-clock-in-and-track)))))
    (bind-key "C-c q" 'my/org-quick-clock-in-task)

    (require 'quantified nil t)



    )


  (progn

    ;;http://pages.sachachua.com/.emacs.d/Sacha.html
    ;;http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

    (defun my/org-contacts-template-email (&optional return-value)
      "Try to return the contact email for a template.
  If not found return RETURN-VALUE or something that would ask the user."
      (or (cadr (if (gnus-alive-p)
                    (gnus-with-article-headers
                     (mail-extract-address-components
                      (or (mail-fetch-field "Reply-To") (mail-fetch-field "From") "")))))
          return-value
          (concat "%^{" org-contacts-email-property "}p")))


    (defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")

    (setq org-capture-templates
          `(("t" "Tasks" entry
             (file+headline "~/personal/organizer.org" "Inbox")
             ,my/org-basic-task-template)
            ("T" "Quick task" entry
             (file+headline "~/personal/organizer.org" "Inbox")
             "* TODO %^{Task}\nSCHEDULED: %t\n"
             :immediate-finish t)
            ("i" "Interrupting task" entry
             (file+headline "~/personal/organizer.org" "Inbox")
             "* STARTED %^{Task}"
             :clock-in :clock-resume)
            ("e" "Emacs idea" entry
             (file+headline "~/code/emacs-notes/tasks.org" "Emacs")
             "* TODO %^{Task}"
             :immediate-finish t)
            ("E" "Energy" table-line
             (file+headline "~/personal/organizer.org" "Track energy")
             "| %U | %^{Energy 5-awesome 3-fuzzy 1-zzz} | %^{Note} |"
             :immediate-finish t
             )
            ("b" "Business task" entry
             (file+headline "~/personal/business.org" "Tasks")
             ,my/org-basic-task-template)
            ("p" "People task" entry
             (file+headline "~/personal/people.org" "Tasks")
             ,my/org-basic-task-template)
            ("j" "Journal entry" plain
             (file+datetree "~/personal/journal.org")
             "%K - %a\n%i\n%?\n"
             :unnarrowed t)
            ("J" "Journal entry with date" plain
             (file+datetree+prompt "~/personal/journal.org")
             "%K - %a\n%i\n%?\n"
             :unnarrowed t)
            ("s" "Journal entry with date, scheduled" entry
             (file+datetree+prompt "~/personal/journal.org")
             "* \n%K - %a\n%t\t%i\n%?\n"
             :unnarrowed t)
            ("c" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox")
             "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")
            ("db" "Done - Business" entry
             (file+headline "~/personal/business.org" "Tasks")
             "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
            ("dp" "Done - People" entry
             (file+headline "~/personal/people.org" "Tasks")
             "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
            ("dt" "Done - Task" entry
             (file+headline "~/personal/organizer.org" "Inbox")
             "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
            ("q" "Quick note" item
             (file+headline "~/personal/organizer.org" "Quick notes"))
            ("l" "Ledger entries")
            ("lm" "MBNA" plain
             (file "~/personal/ledger")
             "%(org-read-date) %^{Payee}
    Liabilities:MBNA
    Expenses:%^{Account}  $%^{Amount}
  " :immediate-finish t)
            ("ln" "No Frills" plain
             (file "~/personal/ledger")
             "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
    Liabilities:MBNA
    Assets:Wayne:Groceries  $%^{Amount}
  " :immediate-finish t)
            ("lc" "Cash" plain
             (file "~/personal/ledger")
             "%(org-read-date) * %^{Payee}
    Expenses:Cash
    Expenses:%^{Account}  %^{Amount}
  ")
            ("B" "Book" entry
             (file+datetree "~/personal/books.org" "Inbox")
             "* %^{Title}  %^g
  %i
  *Author(s):* %^{Author} \\\\
  *ISBN:* %^{ISBN}

  %?

  *Review on:* %^t \\
  %a
  %U"
             :clock-in :clock-resume)
            ("C" "Contact" entry (file "~/personal/contacts.org")
             "* %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(my/org-contacts-template-email)
  :END:")
            ("n" "Daily note" table-line (file+olp "~/personal/organizer.org" "Inbox")
             "| %u | %^{Note} |"
             :immediate-finish t)
            ("r" "Notes" entry
             (file+datetree "~/personal/organizer.org")
             "* %?\n\n%i\n%U\n"
             )))
    (bind-key "C-M-r" 'org-capture)

    ;; Allow refiling in the middle(ish) of a capture

    ;; This lets me use C-c C-r to refile a capture and then jump to the new
    ;; location. I wanted to be able to file tasks under projects so that they
    ;; could inherit the QUANTIFIED property that I use to track time (and any
    ;; Beeminder-related properties too), but I also wanted to be able to clock in
    ;; on them.

    (defun my/org-refile-and-jump ()
      (interactive)
      (if (derived-mode-p 'org-capture-mode)
          (org-capture-refile)
        (call-interactively 'org-refile))
      (org-refile-goto-last-stored))
    (eval-after-load 'org-capture
      '(bind-key "C-c C-r" 'my/org-refile-and-jump org-capture-mode-map))


    )

  (progn
    ;;http://pages.sachachua.com/.emacs.d/Sacha.html
    ;;http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/
    (progn



      (ert-deftest my/org-capture-prefill-template ()
        (should
         ;; It should fill things in one field at ia time
         (string=
          (my/org-capture-prefill-template
           "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
           "Hello World")
          "* TODO Hello World\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
          ))
        (should
         (string=
          (my/org-capture-prefill-template
           "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
           "Hello World" "<2015-01-01>")
          "* TODO Hello World\nSCHEDULED: <2015-01-01>\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"))
        (should
         (string=
          (my/org-capture-prefill-template
           "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
           "Hello World" "<2015-01-01>" "0:05")
          "* TODO Hello World\nSCHEDULED: <2015-01-01>\n:PROPERTIES:\n:Effort: 0:05\n:END:\n%?\n")))

      (defun my/org-capture-prefill-template (template &rest values)
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

      (defun my/org-get-current-refile-location ()
        "Return the current entry as a location understood by org-refile."
        (interactive)
        (list (elt (org-heading-components) 4)
              (or buffer-file-name
                  (with-current-buffer (buffer-base-buffer (current-buffer))
                    buffer-file-name))
              nil
              (point)))

      (defun my/helm-org-create-task (candidate)
        "Creates the task and returns the location."
        (let ((entry (org-capture-select-template "T")))
          (org-capture-set-plist entry)
          (org-capture-get-template)
          (org-capture-set-target-location)
          (condition-case error
              (progn
                (org-capture-put
                 :template
                 (org-capture-fill-template
                  (my/org-capture-prefill-template (org-capture-get :template)
                                                   candidate)))
                (org-capture-place-template
                 (equal (car (org-capture-get :target)) 'function))
                (setq org-refile-target-table (org-refile-get-targets))
                ;; Return the new location
                (my/org-get-current-refile-location))
            ((error quit)
             (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
             (error "Capture abort: %s" error)))))

      ;; (my/org-refile-get-location-by-substring "Try again")

      ;; Next, I want to add this to the way that Helm prompts me to refile. That
      ;; means that my creation task should return something ready for org-refile.
      ;; Actually, maybe I don't have to do that if I know I'm always going to
      ;; call it when I want to jump to something. I might as well add that bit of
      ;; code that sets up clocking in, too.

      (defvar my/helm-org-refile-locations nil)
      (defvar my/org-refile-last-location nil)

      (defun my/helm-org-clock-in-and-track-from-refile (candidate)
        (let ((location (org-refile--get-location candidate my/helm-org-refile-locations)))
          (save-window-excursion
            (org-refile 4 nil location)
            (my/org-clock-in-and-track)
            t)))

      (defun my/org-get-todays-items-as-refile-candidates ()
        "Return items scheduled for today, ready for choosing during refiling."
        (delq
         nil
         (mapcar
          (lambda (s)
            (if (get-text-property 0 'org-marker s)
                (list
                 s
                 (buffer-file-name (marker-buffer (get-text-property 0 'org-marker s)))
                 nil
                 (marker-position (get-text-property 0 'org-marker s)))))
          (save-window-excursion (my/org-get-entries-fn (calendar-current-date) (calendar-current-date))))))

      ;; Based on http://emacs.stackexchange.com/questions/4063/how-to-get-the-raw-data-for-an-org-mode-agenda-without-an-agenda-view
      (defun my/org-get-entries-fn (begin end)
        "Return org schedule items between BEGIN and END.
USAGE:  (org-get-entries-fn '(6 1 2015) '(6 30 2015))"
        (require 'calendar)
        (require 'org)
        (require 'org-agenda)
        (require 'cl)
        (unless
            (and
             (calendar-date-is-valid-p begin)
             (calendar-date-is-valid-p end))
          (let ((debug-on-quit nil))
            (signal 'quit `("One or both of your gregorian dates are invalid."))))
        (let* ((result nil)
               (org-agenda-prefix-format "  • ")
               (org-agenda-entry-types '(:scheduled))
               (date-after
                (lambda (date num)
                  "Return the date after NUM days from DATE."
                  (calendar-gregorian-from-absolute
                   (+ (calendar-absolute-from-gregorian date) num))))
               (enumerate-days
                (lambda (begin end)
                  "Enumerate date objects between BEGIN and END."
                  (when (> (calendar-absolute-from-gregorian begin)
                           (calendar-absolute-from-gregorian end))
                    (error "Invalid period : %S - %S" begin end))
                  (let ((d begin) ret (cont t))
                    (while cont
                      (push (copy-sequence d) ret)
                      (setq cont (not (equal d end)))
                      (setq d (funcall date-after d 1)))
                    (nreverse ret)))))
          (org-agenda-reset-markers)
          (setq org-agenda-buffer
                (when (buffer-live-p org-agenda-buffer)
                  org-agenda-buffer))
          (org-compile-prefix-format nil)
          (setq result
                (loop for date in (funcall enumerate-days begin end) append
                      (loop for file in (org-agenda-files nil 'ifmode)
                            append
                            (progn
                              (org-check-agenda-file file)
                              (apply 'org-agenda-get-day-entries file date org-agenda-entry-types)))))
          (unless (buffer-live-p (get-buffer org-agenda-buffer-name))
            (get-buffer-create org-agenda-buffer-name))
          (with-current-buffer (get-buffer org-agenda-buffer-name)
            (org-agenda-mode)
            (setq buffer-read-only t)
            (let ((inhibit-read-only t))
              (erase-buffer))
            (mapcar
             (lambda (x)
               (let ((inhibit-read-only t))
                 (insert (format "%s" x) "\n")))
             result))
          ;;    (display-buffer org-agenda-buffer-name t)
          result))

      (defun my/helm-org-refile-read-location (tbl)
        (setq my/helm-org-refile-locations tbl)
        (helm
         (list
          ;; (helm-build-sync-source "Today's tasks"
          ;;   :candidates (mapcar (lambda (a) (cons (car a) a))
          ;;                       (my/org-get-todays-items-as-refile-candidates))
          ;;   :action '(("Select" . identity)
          ;;             ("Clock in and track" . my/helm-org-clock-in-and-track-from-refile)
          ;;             ("Draw index card" . my/helm-org-prepare-index-card-for-subtree))
          ;;   :history 'org-refile-history)
          (helm-build-sync-source "Refile targets"
            :candidates (mapcar (lambda (a) (cons (car a) a)) tbl)
            :action '(("Select" . identity)
                      ("Clock in and track" . my/helm-org-clock-in-and-track-from-refile)
                      ("Draw index card" . my/helm-org-prepare-index-card-for-subtree))
            :history 'org-refile-history)
          (helm-build-dummy-source "Create task"
            :action (helm-make-actions
                     "Create task"
                     'my/helm-org-create-task)))))

      (defun my/org-refile-get-location (&optional prompt default-buffer new-nodes no-exclude)
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
                ;; (org-refile-get-targets default-buffer excluded-entries)
                (org-refile-get-targets default-buffer)))
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
          (setq answ (my/helm-org-refile-read-location tbl))
          (cond
           ((and (stringp answ)
                 (setq pa (org-refile--get-location answ tbl)))
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
            (setq my/org-refile-last-location pa)
            pa)
           ((and (stringp answ) (string-match "\\`\\(.*\\)/\\([^/]+\\)\\'" answ))
            (setq parent (match-string 1 answ)
                  child (match-string 2 answ))
            (setq parent-target (org-refile--get-location parent tbl))
            (when (and parent-target
                       (or (eq new-nodes t)
                           (and (eq new-nodes 'confirm)
                                (y-or-n-p (format "Create new node \"%s\"? "
                                                  child)))))
              (org-refile-new-child parent-target child)))
           ((listp answ) answ) ;; Sacha: Helm returned a refile location
           ((not (equal answ t))
            (user-error "Invalid target location")))))

      (fset 'org-refile-get-location-bkp 'org-refile-get-location)

      (fset 'org-refile-get-location 'my/org-refile-get-location)



      )
    )


  (fset 'org-refile-get-location 'org-refile-get-location-bkp))



(when nil
  (cl-defgeneric display-number (i)
    "display-number")

  (cl-defmethod display-number ((i number))
    ;; (cl-call-next-method)
    (message "plain i=%d" i))

  (cl-defmethod display-number :extra "test0" ((i number))
                (cl-call-next-method)
                (message "test0 i=%d" i))

  (cl-defmethod display-number :extra "test1" ((i number))
                (cl-call-next-method)
                (message "test1 i=%d" i))

  (cl-defmethod display-number :extra "test2" ((i number))
                (cl-call-next-method)
                (message "test2 i=%d" i))

  (display-number 1))

;;; occ-scratch-space.el ends here
