
(deh-require-maybe 'org
    ;; The following lines are always needed.  Choose your own keys.
      (setq org-remind-escape-percentage nil
            org-remind-include-todo t
            org-log-done '(stat)        ;use for prgress logging.
            org-remind-suppress-last-newline nil)

      ;; #+SEQ_TODO: TODO ORDERED INVOICE PAYED RECEIVED SENT
      ;; #+STARTUP: lognotestate
      ;;;; from http://www.djcbsoftware.nl/dot-emacs.html
      ;; (add-hook 'org-mode-hook
      ;;           (lambda() (add-hook 'before-save-hook 'org-agenda-to-appt t t)))
      ;;;; from http://www.djcbsoftware.nl/dot-emacs.html
      (deh-require-maybe 'org2rem
        (add-hook 'org-mode-hook
                  (lambda()
                    (add-hook 'after-save-hook 'org2rem-all-agenda-files t t))))

      (add-hook 'org-mode-hook
                (lambda()
                  (add-hook 'after-save-hook 'org-agenda-to-appt t t)))

      (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
      (global-set-key "\C-cl" 'org-store-link)
      (global-set-key "\C-ca" 'org-agenda)
      (global-set-key "\C-cb" 'org-iswitchb)
;;;       (define-key org-mode-map [S-right] 'forward-word)
;;;       (define-key org-mode-map [S-left] 'backward-word)
;;;       (define-key org-mode-map [S-down] 'scroll-one-line-up)
;;;       (define-key org-mode-map [S-up] 'scroll-one-line-down)

      (defun gtd ()
        (interactive)
        (find-file "~/.Organize/emacs/org/office/plan.org" )
        ;;(org-show-todo-tree 4)
        )

      (setq org-agenda-custom-commands
            '(("P" "Project List"
               ((tags "PROJECT")))
              ("O" "Office"
               ((agenda)
                 (tags-todo "OFFICE")))
              ("W" "Weekly Plan"
               ((agenda)
                 (todo "TODO")
                 (tags "PROJECT")))
              ("H" "Home NA Lists"
               ((agenda)
                (tags-todo "HOME")
                (tags-todo "COMPUTER")))))

      (xrequire 'org-export-freemind-install)
      (defadvice org-agenda-to-appt (before wickedcool activate)
        "Clear the appt-time-msg-list."
        (setq appt-time-msg-list nil)))

(user-provide 'orgmode)

