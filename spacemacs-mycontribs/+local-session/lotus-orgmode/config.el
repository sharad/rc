;;; config.el --- config                             -*- lexical-binding: t; -*-

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

(defun lotus-orgmode-config/post-init-org ()
  (progn

    (progn
      (add-hook 'message-mode-hook 'turn-on-orgstruct)
      (add-hook 'message-mode-hook 'turn-on-orgstruct++))

    (progn
      (when (fboundp 'spaceline-toggle-org-clock-on)
        (spaceline-toggle-org-clock-on)))

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
      (setq
       org-todo-keywords ;; http://doc.norang.ca/org-mode.html#sec-5
       '((sequence "TODO(t)" "STARTED" "NEXT(n)" "|" "DONE(d@/!)" "|" "CLOSED(c@/!)")
         (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(C@/!)" "PHONE" "MEETING"))

       org-todo-keyword-faces ;; http://doc.norang.ca/org-mode.html#sec-5
       '(("TODO" :foreground "red" :weight bold)
         ("STARTED" :foreground "red" :weight bold)
         ("NEXT" :foreground "blue" :weight bold)
         ("DONE" :foreground "forest green" :weight bold)
         ("CLOSED" :foreground "forest green" :weight bold)
         ("WAITING" :foreground "orange" :weight bold)
         ("HOLD" :foreground "magenta" :weight bold)
         ("CANCELLED" :foreground "forest green" :weight bold)
         ("MEETING" :foreground "forest green" :weight bold)
         ("PHONE" :foreground "forest green" :weight bold))))

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
         (shell . t) ;; https://emacs.stackexchange.com/questions/37692/how-to-fix-symbols-function-definition-is-void-org-babel-get-header?rq=1
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
        ))


    (progn
      ;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
      (setq
       org-refile-use-outline-path 'file ;; default nil
       org-outline-path-complete-in-steps nil ;; default t
       org-refile-allow-creating-parent-nodes 'confirm ;; default nil
       )
      )

    (progn
      ;; http://pages.sachachua.com/.emacs.d/Sacha.html#org0c5d380
      (setq
       org-expiry-inactive-timestamps t
       org-clock-idle-time nil
       org-log-done 'time
       org-clock-continuously nil
       org-clock-persist t
       org-clock-in-switch-to-state "STARTED"
       org-clock-in-resume nil
       org-show-notification-handler 'message
       org-clock-report-include-clocking-task t))))

(defun lotus-orgmode-config/post-init-org-task-manager-occ ()
  ;; add hook occ-global-tsk-collection-spec onchange
  ;; when it gets change run code at that time.
  (progn
    (let* ((party-base-dir (task-party-base-dir))
           (start-file (expand-file-name "start.org" party-base-dir)))
      (setq
       org-agenda-files (occ-included-files)))

    (progn
      (add-to-task-current-party-change-hook
       (occ-run-with-global-tsk-collection
        #'(lambda ()
            (let* ((party-base-dir (task-party-base-dir))
                   (start-file (expand-file-name "start.org" party-base-dir)))
              (setq
               org-agenda-files (occ-included-files)))))))))

(defun lotus-orgmode-config/post-init-org-agenda ()
  (progn

    (require 'file-utils)

    (setq org-agenda-custom-commands nil)

    (defun add-to-org-agenda-custom-commands (&rest specs)
      (dolist (spec specs)
        (add-to-list
         'org-agenda-custom-commands
         spec)))

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

    (add-to-org-agenda-custom-commands
     ;; https://emacs.stackexchange.com/questions/16551/how-do-i-view-all-org-mode-todos-that-are-not-recurring-or-not-scheduled
     '("c" . "My Custom Agendas")
     '("cu" "Unscheduled TODO"
       ((todo ""
         ((org-agenda-overriding-header "\nUnscheduled TODO")
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
       nil
       nil))

    ;; (require 'org-publishing)

    (when nil                     ;moved to lotus-publishing
      (use-package publishing
          :defer t
          :config
          (progn
            (let ((task-dir
                   (expand-file-name "meru" (org-publish-get-attribute "tasks" "org" :base-directory))))
              (when (file-directory-p task-dir)
                (add-to-org-agenda-custom-commands
                 `("Z" ;; "Meru Today" ;; tags-todo "computer" ;; (1) (2) (3) (4)
                   "Meru Today" ;;  search ""
                   ((agenda ""
                            ((org-agenda-span 'day)
                             (org-agenda-prefix-format  "%e")))
                    (org-agenda-files
                     ',(directory-files-recursive task-dir
                                                  "\\.org$" 2 "\\(rip\\|stage\\)"))
                    ;; (org-agenda-sorting-strategy '(priority-up effort-down))
                    )
                   ;; ("~/computer.html")
                   ))))))
      )

    (progn ;; "Review Aganda" ;;http://stackoverflow.com/a/22440571
      ;; define "R" as the prefix key for reviewing what happened in various
      ;; time periods
      (add-to-org-agenda-custom-commands
       '("R" . "Review" ))

      (when nil                     ;moved to lotus-publishing
        (use-package publishing
            :defer t
            :config
            (progn
              (progn ;; "org-publishing"

                ;; COMMON settings for all reviews
                (setq efs/org-agenda-review-settings
                      `((org-agenda-files
                         ',(let ((task-dir (expand-file-name "meru" (org-publish-get-attribute "tasks" "org" :base-directory))))
                             (if (file-directory-p task-dir)
                                 (directory-files-recursive
                                  task-dir
                                  "\\.org$" 2 "\\(rip\\|stage\\)"))))
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
                          "~/Documents/CreatedContent/contents/virtual/org/default")
                         "\\.org$"
                         2
                         "\\(rip\\|stage\\)")))

    (org-reset-agenda-files)


    (setq
     ;; http://orgmode.org/worg/agenda-optimization.html
     ;; org-agenda-inhibit-startup t
     )))

(defun lotus-orgmode-config/post-init-ob-tangle ()
  (progn
    ;; (set-face-attribute
    ;;    'org-block nil :foreground "#FFFFFF")

    (set-face-attribute 'org-block nil :inherit 'src-block)

    ;; https://emacs.stackexchange.com/questions/26603/how-to-run-the-tangled-file
    (unless (require 'ob-sh nil 'noerror)
      (require 'ob-shell))

    ;; http://explog.in/dot/emacs/config.html
    (setq
     org-src-fontify-natively t
     org-src-tab-acts-natively t
     org-edit-src-content-indentation 0)

    (spacemacs/add-to-hooks (if dotspacemacs-smartparens-strict-mode
                                'smartparens-strict-mode
                              'smartparens-mode)
                            '( org-mode-hook ))


    (dolist (f prog-mode-hook)
      (spacemacs/add-to-hooks
       f
       '(org-mode-hook))))

  (load "~/.xemacs/snippets/org-untangle-utils.el"))

(defun lotus-orgmode-config/init-org-timer ()
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

    ))

(defun lotus-orgmode-config/init-org-clock ()
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
      (defun org-idle-tracing-function (orig-fun &rest args)
        (message "org-resolve-clocks-if-idle called with args %S" args)
        (let ((res (apply orig-fun args)))
          (message "org-resolve-clocks-if-idle returned %S" res)
          res))

      (advice-add 'org-resolve-clocks-if-idle :around #'org-idle-tracing-function))

    ;; (advice-remove 'display-buffer #'org-idle-tracing-function)
    ))

(defun lotus-orgmode-config/init-org-feed ()
  (progn
    (with-eval-after-load "org-publishing"
      (setq org-feed-alist
            `(("mybugs"
               (concat
                "https://bugzilla.com/buglist.cgi?"
                (mapconcat
                 'identity
                 (list
                  "bug_status=NEEDINFO"
                  "bug_status=NEW"
                  "bug_status=ASSIGNED"
                  "bug_status=REOPENED"
                  "email1=spratap%40merunetworks.com"
                  "emailassigned_to1=1"
                  "emailreporter1=1"
                  "emailtype1=exact"
                  "list_id=169890"
                  "query_format=advanced"
                  "title=Bug%20List"
                  "ctype=atom")
                 "&"))
               ,(expand-file-name "meru/mybugs.org" (org-publish-get-attribute "tasks" "org" :base-directory))
               "My Bugs")
              ("OSNews"
               "http://www.osnews.com/feeds"
               ,(expand-file-name "../rss/osnews.org" (org-publish-get-attribute "tasks" "org" :base-directory))
               "OSNews Entries"))))))

(defun lotus-orgmode-config/init-org2rem ()
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
                    (add-hook 'after-save-hook 'org2rem-this-file t t))))))

(defun lotus-orgmode-config/init-orgnav ()
  (progn
    (setq orgnav-log 't)))

(defun lotus-orgmode-config/init-outshine ()
  ;; TODO https://orgmode.org/worg/org-tutorials/org-outside-org.html
  ;; http://www.modernemacs.com/post/outline-ivy/
  ;; https://gist.github.com/kidd/8a5209d0ca9885a6883fa4459f5420d6

  ;; https://www.emacswiki.org/emacs/OutlineMinorMode
  ;; https://gist.github.com/kidd/8a5209d0ca9885a6883fa4459f5420d6
  ;; http://www.modernemacs.com/post/outline-ivy/
  ;; https://orgmode.org/worg/org-tutorials/org-outside-org.html

  (progn
    ;; https://gist.github.com/kidd/8a5209d0ca9885a6883fa4459f5420d6
    ;; (defvar outline-minor-mode-prefix "\M-#")
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
    (setq outshine-use-speed-commands t)
    (add-hook 'prog-mode-hook 'outline-minor-mode)
    (add-hook 'ruby-mode-hook 'outline-minor-mode)
    (add-hook 'lua-mode-hook 'outline-minor-mode)
    (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
    (add-hook 'common-lisp-mode-hook 'outline-minor-mode)
    (add-hook 'lisp-mode-hook 'outline-minor-mode)))

;;; config.el ends here
