
;; (configuration-layer/declare-layer 'B)




;; (add-hook 'find-file-hook 'git-gutter+-turn-on)


;; (git-gutter+-turn-on mode-local-post-major-mode-change global-flycheck-mode-check-buffers show-smartparens-global-mode-check-buffers gdb-find-file-hook pbm-file-find-file-hook persp-add-or-not-on-find-file recentf-track-opened-file auto-insert yatemplate--find-file-hook
;;                      (lambda nil
;;                        (if recentf-mode nil
;;                          (recentf-mode)
;;                          (recentf-track-opened-file)))
;;                      projectile-rails-global-mode-check-buffers projectile-find-file-hook-function global-page-break-lines-mode-check-buffers yas-global-mode-check-buffers global-hl-todo-mode-check-buffers forge-bug-reference-setup fasd-add-file-to-db global-anzu-mode-check-buffers global-edit-server-edit-mode-check-buffers magit-branch-description-check-buffers global-magit-file-mode-check-buffers git-commit-setup-check-buffer evil-mode-check-buffers global-undo-tree-mode-check-buffers undo-tree-load-history-hook global-spacemacs-leader-override-mode-check-buffers annotate-todo spacemacs/check-large-file auto-revert-find-file-function url-handlers-set-buffer-mode global-eldoc-mode-check-buffers global-font-lock-mode-check-buffers epa-file-find-file-hook vc-refresh-state which-func-ff-hook save-place-find-file-hook ede-turn-on-hook)


;; (setq find-file-hook
;;       '(git-gutter+-turn-on
;;         mode-local-post-major-mode-change
;;         global-flycheck-mode-check-buffers
;;         show-smartparens-global-mode-check-buffers
;;         gdb-find-file-hook
;;         pbm-file-find-file-hook
;;         persp-add-or-not-on-find-file
;;         recentf-track-opened-file
;;         auto-insert
;;         yatemplate--find-file-hook
;;         (lambda nil
;;           (if recentf-mode nil
;;             (recentf-mode)
;;             (recentf-track-opened-file)))
;;         projectile-rails-global-mode-check-buffers
;;         projectile-find-file-hook-function
;;         global-page-break-lines-mode-check-buffers
;;         yas-global-mode-check-buffers
;;         global-hl-todo-mode-check-buffers
;;         forge-bug-reference-setup
;;         fasd-add-file-to-db
;;         global-anzu-mode-check-buffers
;;         global-edit-server-edit-mode-check-buffers
;;         magit-branch-description-check-buffers
;;         global-magit-file-mode-check-buffers
;;         git-commit-setup-check-buffer
;;         evil-mode-check-buffers
;;         global-undo-tree-mode-check-buffers
;;         undo-tree-load-history-hook
;;         global-spacemacs-leader-override-mode-check-buffers
;;         annotate-todo
;;         spacemacs/check-large-file
;;         auto-revert-find-file-function
;;         url-handlers-set-buffer-mode
;;         global-eldoc-mode-check-buffers
;;         global-font-lock-mode-check-buffers
;;         epa-file-find-file-hook
;;         vc-refresh-state
;;         which-func-ff-hook
;;         save-place-find-file-hook
;;         ede-turn-on-hook))



;; (setq-default find-file-hook
;;                '(git-gutter+-turn-on
;;                  mode-local-post-major-mode-change
;;                  global-flycheck-mode-check-buffers
;;                  show-smartparens-global-mode-check-buffers
;;                  gdb-find-file-hook
;;                  pbm-file-find-file-hook
;;                  persp-add-or-not-on-find-file

;;                  recentf-track-opened-file
;;                  auto-insert
;;                  yatemplate--find-file-hook
;;                  (lambda nil
;;                    (if recentf-mode nil
;;                      (recentf-mode)
;;                      (recentf-track-opened-file)))
;;                  projectile-rails-global-mode-check-buffers
;;                  projectile-find-file-hook-function

;;                  global-page-break-lines-mode-check-buffers
;;                  yas-global-mode-check-buffers
;;                  global-hl-todo-mode-check-buffers

;;                  forge-bug-reference-setup

;;                  fasd-add-file-to-db
;;                  global-anzu-mode-check-buffers
;;                  global-edit-server-edit-mode-check-buffers

;;                  magit-branch-description-check-buffers
;;                  global-magit-file-mode-check-buffers
;;                  git-commit-setup-check-buffer
;;                  evil-mode-check-buffers
;;                  global-undo-tree-mode-check-buffers
;;                  undo-tree-load-history-hook
;;                  global-spacemacs-leader-override-mode-check-buffers
;;                  annotate-todo

;;                  spacemacs/check-large-file
;;                  auto-revert-find-file-function
;;                  url-handlers-set-buffer-mode
;;                  global-eldoc-mode-check-buffers
;;                  global-font-lock-mode-check-buffers
;;                  epa-file-find-file-hook

;;                  vc-refresh-state
;;                  which-func-ff-hook
;;                  save-place-find-file-hook
;;                  ede-turn-on-hook))

;; (setq-default find-file-hook (default-value 'find-file-hook))


;; (defun forge-bug-reference-setup ()
;;   "Setup `bug-reference' in the current buffer.
;; If forge data has been fetched for the current repository, then
;; enable `bug-reference-mode' or `bug-reference-prog-mode' and set
;; some `bug-reference' variables to the appropriate values."
;;   (magit--with-safe-default-directory nil
;;     (when-let ((repo (forge-get-repository 'full))
;;                (format (oref repo issue-url-format)))
;;       (unless bug-reference-url-format
;;         (setq-local bug-reference-url-format
;;                     (if (forge--childp repo 'forge-gitlab-repository)
;;                         (lambda ()
;;                           ;; (message "TEST")
;;                           (forge--format repo
;;                                          (if (equal (match-string 3) "#")
;;                                              'issue-url-format
;;                                            'pullreq-url-format)
;;                                          `((?i . ,(match-string 2)))))
;;                       (forge--format repo 'issue-url-format '((?i . "%s")))))
;;         (setq-local bug-reference-bug-regexp
;;                     (if (forge--childp repo 'forge-gitlab-repository)
;;                         "\\(?3:[!#]\\)\\(?2:[0-9]+\\)"
;;                       "#\\(?2:[0-9]+\\)")))
;;       (if (derived-mode-p 'prog-mode)
;;           (bug-reference-prog-mode 1)
;;         (bug-reference-mode 1))
;;       (add-hook 'completion-at-point-functions
;;                 'forge-topic-completion-at-point nil t))))


;; (magit--with-safe-default-directory nil)

;; (forge-bug-reference-setup)


;; (magit--with-safe-default-directory nil
;;     (when-let ((repo (forge-get-repository 'full))
;;                (format (oref repo issue-url-format)))
;;       (unless bug-reference-url-format
;;         (setq-local bug-reference-url-format
;;                     (if (forge--childp repo 'forge-gitlab-repository)
;;                         (lambda ()
;;                           ;; (message "TEST")
;;                           (forge--format repo
;;                                          (if (equal (match-string 3) "#")
;;                                              'issue-url-format
;;                                            'pullreq-url-format)
;;                                          `((?i . ,(match-string 2)))))
;;                       (forge--format repo 'issue-url-format '((?i . "%s")))))
;;         (setq-local bug-reference-bug-regexp
;;                     (if (forge--childp repo 'forge-gitlab-repository)
;;                         "\\(?3:[!#]\\)\\(?2:[0-9]+\\)"
;;                       "#\\(?2:[0-9]+\\)")))
;;       (if (derived-mode-p 'prog-mode)
;;           (bug-reference-prog-mode 1)
;;         (bug-reference-mode 1))
;;       (add-hook 'completion-at-point-functions
;;                 'forge-topic-completion-at-point nil t)))




;; (cl-defmethod forge-get-repository ((topic forge-topic))
;;   (closql-get (forge-db)
;;               (oref topic repository)
;;               'forge-repository))



;; (defun forge-db ()
;;   (unless (and forge--db-connection (emacsql-live-p forge--db-connection))
;;     (make-directory (file-name-directory forge-database-file) t)
;;     (closql-db 'forge-database 'forge--db-connection
;;                forge-database-file t)
;;     (let* ((db forge--db-connection)
;;            (version (caar (emacsql db "PRAGMA user_version")))
;;            (version (forge--db-maybe-update forge--db-connection version)))
;;       (cond
;;        ((> version forge--db-version)
;;         (emacsql-close db)
;;         (user-error
;;          "The Forge database was created with a newer Forge version.  %s"
;;          "You need to update the Forge package."))
;;        ((< version forge--db-version)
;;         (emacsql-close db)
;;         (error "BUG: The Forge database scheme changed %s"
;;                "and there is no upgrade path")))))
;;   forge--db-connection)



;; (closql-db 'forge-database 'forge--db-connection
;;            forge-database-file t)






;; (cl-defmethod closql-db ((class (subclass closql-database))
;;                          &optional variable file debug)
;;   (or (let ((db (and variable (symbol-value variable))))
;;         (and db (emacsql-live-p db) db))
;;       (let ((db-init (not (and file (file-exists-p file))))
;;             (db (make-instance class :file file)))
;;         (set-process-query-on-exit-flag (oref db process) nil)
;;         (when debug
;;           (emacsql-enable-debugging db))
;;         (when db-init
;;           (closql--db-init db))
;;         (when variable
;;           (set variable db))
;;         db)))



;; (or (let ((db (and 'forge--db-connection (symbol-value 'forge--db-connection))))
;;       (and db (emacsql-live-p db) db))
;;     (let ((db-init (not (and forge-database-file (file-exists-p forge-database-file))))
;;           (db (make-instance 'forge-database :file forge-database-file)))
;;       (set-process-query-on-exit-flag (oref db process) nil)
;;       (when t
;;         (emacsql-enable-debugging db))
;;       (when db-init
;;         (closql--db-init db))
;;       (when 'forge--db-connection
;;         (set 'forge--db-connection db))
;;       db))




