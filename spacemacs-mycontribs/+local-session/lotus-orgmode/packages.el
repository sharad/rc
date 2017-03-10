;;; packages.el --- lotus-orgmode layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-orgmode-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-orgmode/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-orgmode/pre-init-PACKAGE' and/or
;;   `lotus-orgmode/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-orgmodeS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-orgmode-packages
  '(
    ;; (PACKAGE :location local)
    org
    org-agenda
    org-timer
    org-clock
    org2rem
    org-feed
    )
  "The list of Lisp packages required by the lotus-orgmode layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-orgmode/post-init-org ()
  (use-package org
      :defer t
      :config
      (progn
        (progn
          (with-eval-after-load "org-publishing"
            (setq
             org-directory          (org-publish-get-attribute "notes" "org" :base-directory)
             org-default-notes-file (expand-file-name "notes.org" org-directory))))
        (progn
          (setq org-log-into-drawer "LOGBOOK"))
        (progn
          ;; http://orgmode.org/worg/org-gtd-etc.html
          (add-to-list 'org-modules 'org-timer)
          (add-to-list 'org-modules 'org-clock)
          (add-to-list 'org-modules 'org-protocol))

        (progn
          (setq org-todo-keywords ;; http://doc.norang.ca/org-mode.html#sec-5
                (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

          (setq org-todo-keyword-faces ;; http://doc.norang.ca/org-mode.html#sec-5
                (quote (("TODO" :foreground "red" :weight bold)
                        ("NEXT" :foreground "blue" :weight bold)
                        ("DONE" :foreground "forest green" :weight bold)
                        ("WAITING" :foreground "orange" :weight bold)
                        ("HOLD" :foreground "magenta" :weight bold)
                        ("CANCELLED" :foreground "forest green" :weight bold)
                        ("MEETING" :foreground "forest green" :weight bold)
                        ("PHONE" :foreground "forest green" :weight bold)))))

        (progn ;; "url"
          ;; http://orgmode.org/worg/org-hacks.html

          (require 'mm-url) ; to include mm-url-decode-entities-string

          (defun my-org-insert-url ()
            "Insert org link where default description is set to html title."
            (interactive)
            (let* ((url (read-string "URL: "))
                   (title (get-html-title-from-url url)))
              (org-insert-link nil url title)))

          (defun get-html-title-from-url (url)
            "Return content in <title> tag."
            (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
              (save-excursion
                (set-buffer download-buffer)
                (beginning-of-buffer)
                (setq x1 (search-forward "<title>"))
                (search-forward "</title>")
                (setq x2 (search-backward "<"))
                (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2))))))

        (progn ;; "beautification"
          (deh-require-maybe (and
                              org-bullets
                              ;; org-beautify-theme
                              )
            )

          (ignore-errors
            (require 'org-beautify-theme nil)))

        (progn ;; "babel"
          ;; http://draketo.de/book/export/html/41
                                        ; And add babel inline code execution
                                        ; babel, for executing code in org-mode.
          (org-babel-do-load-languages
           'org-babel-load-languages
                                        ; load all language marked with (lang . t).
           '((C . t)
             ;; (R . t)
             (asymptote)
             (awk)
             (calc)
             (clojure)
             (comint)
             (css)
             (ditaa . t)
             (dot . t)
             (emacs-lisp . t)
             (fortran)
             (gnuplot . t)
             (haskell)
             (io)
             (java)
             (js)
             (latex)
             (ledger)
             (lilypond)
             (lisp)
             (matlab)
             (maxima)
             (mscgen)
             (ocaml)
             (octave)
             (org . t)
             (perl)
             (picolisp)
             (plantuml)
             (python . t)
             (ref)
             (ruby)
             (sass)
             (scala)
             (scheme)
             (screen)
             (sh . t)
             (shen)
             (sql)
             (sqlite))))


        (progn
          (require 'org-export-freemind-install nil t))

        (progn ;; "org misc"

          (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
          ;; see key binding in binding.el

          (with-eval-after-load "org-publishing"
            (defun gtd ()
              (interactive)
              (find-file
               (expand-file-name
                "office"
                (org-publish-get-attribute "notes" "org" :base-directory)))
              ;;(org-show-todo-tree 4)
              )))

        (progn
          (with-eval-after-load "appt"
            (defadvice org-agenda-to-appt (before wickedcool activate)
              "Clear the appt-time-msg-list."
              (setq appt-time-msg-list nil))

            (add-hook 'org-mode-hook
                      (lambda()
                        (add-hook 'after-save-hook 'org-agenda-to-appt t t)))))

        (progn ;; template
          ;; If you are, like me, missing the function org-remember-insinuate, try
          ;; the following
          ;; start
          ;; from: http://members.optusnet.com.au/~charles57/GTD/remember.html

          (defvar org-template-files-revert nil "")

          (add-hook 'ad-remember-mode-after-hook
                    (lambda ()
                      ;;(dolist (f org-template-files-revert)
                      (while org-template-files-revert
                        (let ((f (pop org-template-files-revert)))
                          (if (find-buffer-visiting f)
                              (with-current-buffer (find-buffer-visiting f)
                                (setq buffer-read-only t
                                      view-read-only t
                                      view-mode t)))))))

          (defun org-template-set-file-writable (xfile)
            (if (consp xfile)
                (error "xfile %s not file" xfile))
            (let* ((buf
                    (or (find-buffer-visiting xfile)
                        (find-file-noselect xfile))))
              (with-current-buffer buf
                (when buffer-read-only
                  (setq
                   buffer-read-only nil
                   view-read-only   nil
                   view-mode        nil)
                  (add-to-list 'org-template-files-revert xfile)))
              xfile))

          (defvar org-remember-template-alist nil "org-remember-template-alist")

          (defun org-template-push (template &rest keys)
            (pushnew (cons keys template)
                     org-remember-template-alist
                     :key 'car)))


        (progn ;; "org-protocol-open-source"
          ;; [[http://orgmode.org/worg/org-contrib/org-protocol.html#sec-7][Edit published content: org-protocol-open-source]]
          (setq org-protocol-project-alist
                '(("Worg"
                   :base-url "http://orgmode.org/worg/"
                   :working-directory "/home/user/worg/"
                   :online-suffix ".html"
                   :working-suffix ".org")
                  ("My local Org-notes"
                   :base-url "http://localhost/org/"
                   :working-directory "/home/user/org/"
                   :online-suffix ".php"
                   :working-suffix ".org"))))


        (when nil
          (progn
            (deh-require-maybe (and
                                ob-exp
                                ox-html5presentation)
              )

            (deh-require-maybe (and remember
                                    org
                                    ;; org-html
                                    ob-exp
                                    ob-ditaa
                                    org-element
                                    org-list
                                    planner
                                    remember-planner
                                    read-file-name
                                    ;; remember-blosxom
                                    ;; remember-experimental ;; will start mail at daemon startup time.
                                    remember-autoloads
                                    remember-diary
                                    remember-planner
                                    remember-bbdb
                                    remember
                                    ;; remember-bibl
                                    ;; macs-wiki-journal
                                    ))
            )))
      ))

(defun lotus-orgmode/post-init-org-agenda ()
  (use-package org-agenda
      :defer t
      :config
      (progn
        (progn

          (require 'file-utils)

          (setq org-agenda-custom-commands nil)

          (defun add-to-org-agenda-custom-commands (spec)
            (add-to-list
             'org-agenda-custom-commands
             spec))

          ;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
          (add-to-org-agenda-custom-commands
           '("P" "Project List"
             ((tags "PROJECT"))))

          (add-to-org-agenda-custom-commands
           '("O" "Office"
             ((agenda)
              (tags-todo "OFFICE"))))

          (add-to-org-agenda-custom-commands
           '("W" "Weekly Plan"
             ((agenda)
              (todo "TODO")
              (tags "PROJECT"))))

          (add-to-org-agenda-custom-commands
           '("H" "Home NA Lists"
             ((agenda)
              (tags-todo "HOME")
              (tags-todo "COMPUTER"))))

          (require 'org-publishing)

          (when nil
          (use-package org-publishing
              :defer t
              :config
              (progn
                (add-to-org-agenda-custom-commands
                 `("Z" ;; "Meru Today" ;; tags-todo "computer" ;; (1) (2) (3) (4)
                   "Meru Today" ;;  search ""
                   ((agenda ""
                            ((org-agenda-span 'day)
                             (org-agenda-prefix-format  "%e")))
                    (org-agenda-files
                     ',(directory-files-recursive
                        (expand-file-name "meru" (org-publish-get-attribute "tasks" "org" :base-directory))
                        "\\.org$" 2 "\\(rip\\|stage\\)"))

                    ;; (org-agenda-sorting-strategy '(priority-up effort-down))
                    )
                   ;; ("~/computer.html")
                   ))))
          )

          (progn ;; "Review Aganda" ;;http://stackoverflow.com/a/22440571
            ;; define "R" as the prefix key for reviewing what happened in various
            ;; time periods
            (add-to-org-agenda-custom-commands
             '("R" . "Review" ))

            (when nil
            (use-package org-publishing
              :defer t
              :config
              (progn
                (progn ;; "org-publishing"
                  ;; COMMON settings for all reviews
                  (setq efs/org-agenda-review-settings
                        `((org-agenda-files
                           ',(directory-files-recursive
                              (expand-file-name "meru" (org-publish-get-attribute "tasks" "org" :base-directory))
                              "\\.org$" 2 "\\(rip\\|stage\\)"))
                          (org-agenda-show-all-dates t)
                          (org-agenda-start-with-log-mode t)
                          (org-agenda-start-with-clockreport-mode t)
                          (org-agenda-archives-mode t)
                          ;; I don't care if an entry was archived
                          (org-agenda-hide-tags-regexp
                           (concat org-agenda-hide-tags-regexp
                                   "\\|ARCHIVE"))
                          ))


                  ;; Show the agenda with the log turn on, the clock table show and
                  ;; archived entries shown.  These commands are all the same exept for
                  ;; the time period.
                  (add-to-org-agenda-custom-commands
                   `("Rw" "Week in review"
                     agenda ""
                     ;; agenda settings
                     ,(append
                       efs/org-agenda-review-settings
                       '((org-agenda-span 'week)
                         (org-agenda-start-on-weekday 0)
                         (org-agenda-overriding-header "Week in Review"))
                       )
                     ("~/org/review/week.html")))

                  (add-to-org-agenda-custom-commands
                   `("Rd" "Day in review"
                     agenda ""
                     ;; agenda settings
                     ,(append
                       efs/org-agenda-review-settings
                       '((org-agenda-span 'day)
                         (org-agenda-overriding-header "Week in Review"))
                       )
                     ("~/org/review/day.html")))

                  (add-to-org-agenda-custom-commands
                   `("Rm" "Month in review"
                     agenda ""
                     ;; agenda settings
                     ,(append
                       efs/org-agenda-review-settings
                       '((org-agenda-span 'month)
                         (org-agenda-start-day "01")
                         (org-read-date-prefer-future nil)
                         (org-agenda-overriding-header "Month in Review"))
                       )
                     ("~/org/review/month.html")))))))

            )

          (setq
           org-columns-default-format-org "%25ITEM %TODO %3PRIORITY %TAGS"
           org-columns-default-format     "%TODO %70ITEM(Task) %8Effort(Effort){:} %8CLOCKSUM{:} %8CLOCKSUM_T(Today){:} %CLOSED")

          (defun org-reset-agenda-files ()
            (interactive)
            (setq
             org-agenda-files (directory-files-recursive
                               (expand-file-name
                                "~/Documents/CreatedContent/contents/org")
                               "\\.org$"
                               2
                               "\\(rip\\|stage\\)")))

          (org-reset-agenda-files)


          (setq
           ;; http://orgmode.org/worg/agenda-optimization.html
           ;; org-agenda-inhibit-startup t
           )))))

(defun lotus-orgmode/init-org-timer ()
  (use-package org-timer
      :defer t
      :config
      (progn
        (progn
          (setq
           org-timer-default-timer 25))

        (progn
          (defun org-timer-update-mode-line ()
            "Update the timer time in the mode line."
            (if org-timer-pause-time
                nil
                (setq org-timer-mode-line-string
                      (org-propertize
                       (concat " <" (substring (org-timer-value-string) 0 -1) ">")
                       'mouse-face (if (featurep 'xemacs) 'highlight 'mode-line-highlight)))
                (force-mode-line-update)))

          ;; (let ((range (org-clock-special-range 'today))
          ;;       (headline-filter))
          ;;   (org-clock-sum (car range) (cadr range)
          ;;                  headline-filter :org-clock-minutes-today))
          ;;
          ;;
          ;; (let ((range (org-clock-special-range 'today)))
          ;;   (org-clock-get-table-data
          ;;    "today.org"
          ;;    '(
          ;;      :tstart (car range)
          ;;      :tend   (cadr range)
          ;;      )))

          ))))

(defun lotus-orgmode/init-org-clock ()
  (use-package org-clock
      :defer t
      :config
      (progn
        (eval-when-compile
          (require 'org-misc-utils))

        (progn
          (setq
           ;; org-timer-default-timer 25
           org-clock-persist-file  (auto-config-file "org/clock/org-clock-save.el")
           org-log-note-clock-out t           ;excellent, great
           org-clock-clocked-in-display 'both ;; ('mode-line 'frame-title 'both)
           org-clock-idle-time 5 ;; minutes
           org-clock-resolve-expert nil ;; good
           org-clock-sound t ;; could be file name
           ;; org-clock-current-task
           ;; org-clock-heading
           org-clock-history-length 100
           ;; org-clock-marker
           ;; org-clock-history
           org-clock-persist t
           ;; org-clock-out-switch-to-state ;; good
           ;; org-clock-in-switch-to-state
           org-clock-out-remove-zero-time-clocks t))

        (progn
          (when nil
            (defvar org-clock-display-timer-delay 2 "Org clock display timer delay")

            (defun org-clock-display-with-timer (start end old-len)
              (when (buffer-modified-p)
                ;; (when org-clock-display-timer
                ;;   (cancel-timer org-clock-display-timer)
                ;;   (setq org-clock-display-timer nil))
                ;; (setq
                ;;  org-clock-display-timer
                ;;  (run-with-timer org-clock-display-timer-delay nil 'org-clock-display))
                (org-clock-display)))

            (defun org-mode-setup-clock-display ()
              (make-variable-buffer-local 'org-clock-display-timer)
              (add-hook 'after-change-functions
                        'org-clock-display-with-timer))

            (add-hook 'org-mode-hook 'org-mode-setup-clock-display)))

        (progn
          ;; (find
          ;;   org-clock-leftover-time
          ;;   org-clock-default-task ;; M-x org-clock-mark-default-task
          ;;   M-x org-clock-select-task
          ;; (org-clocking-buffer)
          ;; (org-clock-sum-today)
          ;; (org-clock-sum-custom nil 'today)
          ;; (org-clock-is-active)
          ;; )

          (add-hook 'org-clock-in-hook
                    '(lambda ()
                      ;; ;; if effort is not present than add it.
                      ;; (unless (org-entry-get nil "Effort")
                      ;;   (save-excursion
                      ;;    (org-set-effort)))
                      ;; set timer
                      (when (not
                             (and
                              (boundp' org-timer-countdown-timer)
                              org-timer-countdown-timer))
                        (if (org-entry-get nil "Effort")
                            (save-excursion
                              (forward-line -2)
                              (org-timer-set-timer nil))
                            (call-interactively 'org-timer-set-timer)))
                      (save-buffer)
                      (org-save-all-org-buffers)))

          (defvar org-clock-default-effort "1:00")
          (defun my-org-mode-add-default-effort ()
            "Add a default effort estimation."
            (unless (org-entry-get (point) "Effort")
              (org-set-property "Effort" org-clock-default-effort)))
          (add-hook 'org-clock-in-prepare-hook
                    'my-org-mode-ask-effort)
          (defun my-org-mode-ask-effort ()
            "Ask for an effort estimate when clocking in."
            (unless (org-entry-get (point) "Effort")
              (let ((effort
                     (completing-read
                      "Effort: "
                      (org-entry-get-multivalued-property (point) "Effort"))))
                (unless (equal effort "")
                  (org-set-property "Effort" effort)))))


          (add-hook 'org-clock-out-hook
                    '(lambda ()
                      (if (and
                           (boundp' org-timer-countdown-timer)
                           org-timer-countdown-timer)
                          (org-timer-stop))
                      (org-clock-get-work-day-clock-string t)
                      (save-buffer)
                      (org-save-all-org-buffers)))

          (defun org-clock-in-refile (refile-targets)
            (org-with-refile (or refile-targets org-refile-targets)
                             (let ((buffer-read-only nil))
                               (org-clock-in))))

          (defvar org-donot-try-to-clock-in nil
            "Not try to clock-in, require for properly creating frame especially for frame-launcher function.")

          (defun org-clock-in-if-not ()
            (interactive)
            (unless (or
                     org-donot-try-to-clock-in
                     (org-clock-is-active))
              ;; (org-clock-goto t)
              (if org-clock-history
                  (let (buffer-read-only)
                    (org-clock-in '(4)))
                  (org-clock-in-refile nil))))

          (defun org-clock-out-with-note (note &optional switch-to-state fail-quietly at-time)
            (interactive
             (let ((note (read-from-minibuffer "Closing notes: "))
                   (switch-to-state current-prefix-arg))
               (list note switch-to-state)))
            (let ((org-log-note-clock-out t))
              (move-marker org-log-note-return-to nil)
              (move-marker org-log-note-marker nil)
              (org-clock-out switch-to-state fail-quietly at-time)
              (remove-hook 'post-command-hook 'org-add-log-note)
              (org-insert-log-note note)))

          (with-eval-after-load "startup-hooks"
            (add-hook 'sharad/enable-startup-interrupting-feature-hook
                      '(lambda ()
                        (when nil
                          (add-hook 'after-make-frame-functions
                                    '(lambda (nframe)
                                      (run-at-time-or-now 100
                                       '(lambda ()
                                         (if (any-frame-opened-p)
                                             (org-clock-in-if-not)))))
                                    t))
                        (add-hook 'delete-frame-functions
                         '(lambda (nframe)
                           (if (and
                                (org-clock-is-active)
                                (y-or-n-p-with-timeout (format "Do you want to clock out current task %s: " org-clock-heading) 7 nil))
                               (org-with-clock-writeable-buffer
                                (let (org-log-note-clock-out)
                                  (if (org-clock-is-active)
                                      (org-clock-out))))))))
                      t)

            (add-hook 'sharad/enable-desktop-restore-interrupting-feature
                      '(lambda ()
                        (if (fboundp 'org-clock-persistence-insinuate)
                            (org-clock-persistence-insinuate)
                            (message "Error: Org Clock function org-clock-persistence-insinuate not available."))
                        (if (fboundp 'org-clock-start-check-timer)
                            (org-clock-start-check-timer)))))


          (add-hook
           'kill-emacs-hook
           (lambda ()
             (if (and
                  (org-clock-is-active)
                  ;; (y-or-n-p-with-timeout (format "Do you want to clock out current task %s: " org-clock-heading) 7 nil)
                  )
                 (org-with-clock-writeable-buffer
                  (let (org-log-note-clock-out)
                    (if (org-clock-is-active)
                        (org-clock-out)))))))
          ))))

(defun lotus-orgmode/init-org-feed ()
  (use-package org-feed
      :defer t
      :config
      (progn
        (progn
          (with-eval-after-load "org-publishing"
            (setq org-feed-alist
                  `(("mybugs"
                     "https://bugzilla.merunetworks.com/buglist.cgi?bug_status=NEEDINFO&bug_status=NEW&bug_status=ASSIGNED&bug_status=REOPENED&email1=spratap%40merunetworks.com&emailassigned_to1=1&emailreporter1=1&emailtype1=exact&list_id=169890&query_format=advanced&title=Bug%20List&ctype=atom"
                     ,(expand-file-name "meru/mybugs.org" (org-publish-get-attribute "tasks" "org" :base-directory))
                     "My Bugs")
                    ("OSNews"
                     "http://www.osnews.com/feeds"
                     ,(expand-file-name "../rss/osnews.org" (org-publish-get-attribute "tasks" "org" :base-directory))
                     "OSNews Entries"))))))))

(defun lotus-orgmode/init-org2rem ()
  (use-package org2rem
      :defer t
      :config
      (progn
        (progn ;; org2rem
          ;; (add-hook 'org-mode-hook
          ;;           (lambda()
          ;;             (add-hook 'after-save-hook 'org2rem-all-agenda-files t t)))
          ;; The following lines are always needed.  Choose your own keys.
          (defvar org2rem-create-remind-file nil "Create remind file on when saving org file.")
          (setq org-remind-escape-percentage nil
                org-remind-include-todo t
                org-log-done '(stat)        ;use for prgress logging.
                org-remind-suppress-last-newline nil)

          ;; #+SEQ_TODO: TODO ORDERED INVOICE PAYED RECEIVED SENT
          ;; #+STARTUP: lognotestate
          ;; from http://www.djcbsoftware.nl/dot-emacs.html
          ;; (add-hook 'org-mode-hook
          ;;           (lambda() (add-hook 'before-save-hook 'org-agenda-to-appt t t)))
          ;; from http://www.djcbsoftware.nl/dot-emacs.html
          (add-hook 'org-mode-hook
                    (lambda()
                      (if org2rem-create-remind-file
                          (add-hook 'after-save-hook 'org2rem-this-file t t))))))))

(defun lotus-orgmode/init-org-notmuch ()
  (use-package org-notmuch
      ;; http://notmuchmail.org/emacstips/
      ;; (add-to-list 'load-path "/usr/share/org-mode/lisp")
      :defer t
      :config
      (progn
        )))

;;; packages.el ends here
