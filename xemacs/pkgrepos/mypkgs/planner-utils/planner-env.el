
;; (require 'planner)

;;;_ + Sections

(defun planner-narrow-to-subsection (subsection section &optional create)
  "Widen to the whole page and narrow to the section labelled SECTION.
If CREATE is non-nil, create the section if it is not found.
Return non-nil if SECTION was found."
  (interactive "MSection: \nMSubsection: ")
  (widen)
  (when (planner-narrow-to-section section create)
    (goto-char (point-min))
    ;;(widen)
    (when (or
           (re-search-forward
            (concat "^\\*\\*\\s-+" (regexp-quote subsection) "\\s-*$") nil t)
           (and create
                ;;(funcall planner-create-subsection-function section)
                (funcall 'planner-create-subsection-at-top subsection section)
                (goto-char (point-min))
                (re-search-forward (concat "^\\*\\*\\s-+" (regexp-quote subsection)
                                           "\\s-*$") nil t)))
      (let ((beg (match-beginning 0))
            (end (if (re-search-forward "^\\*+\\s-+" nil t)
                     (match-beginning 0) (point-max))))
        (narrow-to-region beg end)
        t))))

(defun planner-narrow-between-section-subsection (section &optional create)
  "Widen to the whole page and narrow to the section labelled SECTION.
If CREATE is non-nil, create the section if it is not found.
Return non-nil if SECTION was found."
  (interactive "MSection: ")
  (unless (stringp section)
    (setq section (cdr (assoc section planner-sections))))
  (widen)
  (goto-char (point-min))
  (planner-narrow-to-section section create)
  (re-search-forward (concat "^\\*\\s-+" (regexp-quote section) "\\s-*$") nil t) ;skip section.
  (let ((beg (point-min))
        ;; (end (if (re-search-forward "^\\*+\\s-+[^\\s-]+\\s-*$" nil t)
        (end (if (re-search-forward "^\\*+" nil t)
                 (match-beginning 0) (point-max))))
    ;;(widen)                             ;check for multiple body.
    (narrow-to-region beg end))         ;will work
    t)

(defun planner-delete-section (section)
  "Delete the named SECTION."
  (unless (planner-derived-mode-p 'planner-mode)
    (error "This is not a planner buffer"))
  (unless (stringp section)
    (setq section (cdr (assoc section planner-sections))))
  (widen)
  (goto-char (point-min))
  (when (re-search-forward (concat "^\\*\\s-+" section "\\(\\s-*\\)$") nil t)
    (let ((beg (planner-line-beginning-position))
          (end (if (re-search-forward "^* " nil t)
                   (planner-line-beginning-position)
                 (point-max))))
      (delete-region beg end))))

(defun planner-delete-section-text (section)
  "Delete the text of the named SECTION."
  (unless (planner-derived-mode-p 'planner-mode)
    (error "This is not a planner buffer"))
  (unless (stringp section)
    (setq section (cdr (assoc section planner-sections))))
  (widen)
  (goto-char (point-min))
  (when (re-search-forward (concat "^\\*\\s-+" section "\\(\\s-*\\)$") nil t)
    (let ((beg (point))
          (end (if (re-search-forward "^* " nil t)
                   (planner-line-beginning-position)
                 (point-max))))
      (delete-region beg end)
      (goto-char (planner-line-beginning-position)))))

(defun planner-seek-to-first-subsection (&optional subsection section)
  "Positions the point at the specified SECTION, or Tasks if not specified."
  (interactive)
  (unless section
    (setq section planner-default-section))
  (unless (stringp section)
    (setq section (cdr (assoc section planner-sections))))

  (widen)
  (goto-char (point-min))
  (planner-seek-to-first section)

  (if (re-search-forward (concat "^\\*\\*\\s-+" subsection "\\(\\s-*?\\)$") nil t)
      (let ((old (point)) new)
        (forward-line 1)
        (if (re-search-forward "[^\\s-]" nil t) ;non blank line
            (progn
              (goto-char (planner-line-beginning-position))
              (unless (looking-at "^\\*\\+\\s-")
                (setq new (point)))))
        (goto-char (or new old))
        (unless new
          (forward-line 1)
          (when (or (looking-at "^\\*\\+\\s-+")
                    (> (forward-line 1) 0)) (insert "\n"))
          (when (or (looking-at "^\\*\\+\\s-+")
                    (> (forward-line 1) 0)) (insert "\n"))
          (when (looking-at "^\\*\\+\\s-+") (forward-line -1))))
    ;; Section not found, so create it. planner-create-subsection-at-to
    ;; (funcall planner-create-subsection-function subsection)))
    (funcall 'planner-create-subsection-at-top subsection section)))

(defun zzsfe ()
  (interactive)
  (planner-page-create-env-ele "raja" "GNUEmacs" "sharad"))

(defun zzsge ()
  (interactive)
  (planner-page-get-env "GNUEmacs" "sharadasdas"))

(defun zznbss ()
  (interactive)
  (planner-narrow-between-section-subsection 'env t))

(defun zzns ()
  (interactive)
  (planner-narrow-to-subsection "sharad" 'env t))

(defun planner-create-subsection-at-top (subsection section)
  "Create SECTION at top of file."
  (planner-narrow-between-section-subsection section t)
  (goto-char (point-max))
  (insert "\n\n")
  (let ((buffer-status (buffer-modified-p)))
    (insert "** " subsection "\n\n")
    (set-buffer-modified-p buffer-status))
  (widen)
  t)

(defun planner-create-subsection-at-bottom (subsection section)
  "Create SECTION at bottom of file."

  (planner-narrow-to-section section t)
  (goto-char (point-max))
  (insert "\n")
  (let ((buffer-status (buffer-modified-p)))
    (insert "** " subsection "\n")
    (set-buffer-modified-p buffer-status))
  (widen)
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Xplanner-page-create-env (&optional page task-desc)
     (if (and current-prefix-arg (planner-derived-mode-p 'planner-mode)
              (not (string-match planner-date-regexp (planner-page-name))))
         ;; Universal prefix means pick up from current page
         (planner-page-name)
       (let ((planner-default-page
              (if (and (planner-derived-mode-p 'planner-mode)
                       (planner-page-name)
                       (not (string-match planner-date-regexp
                                          (planner-page-name))))

                  (planner-page-name)
                planner-default-page)))
         (planner-read-non-date-page
          (planner-file-alist)))))


(defun planner-page-create-env-ele (ele page &optional task-desc)
  (unless (stringp ele) (error "%s" "ele should be string"))
  (if (some
       '(lambda (e)
          (apply #'string=
                 (mapcar '(lambda (s)
                            (replace-regexp-in-string "\\s-" "" s)) (list e ele))))
       (planner-page-get-env page task-desc))
      (message "%s already exist" ele)
    (save-window-excursion
      (save-excursion
        (planner-page-create-env page task-desc)
        (insert ele "\n\n")
        (save-buffer)))))
;; test
;; (planner-page-create-env-ele "sharad " "MyMIS" "XX")

(defun planner-page-create-env (page &optional task-desc)
  "Create a note to be remembered in PAGE (today if PAGE is nil).
If `planner-reverse-chronological-notes' is non-nil, create the
note at the beginning of the notes section; otherwise, add it to
the end.  Position point after the anchor."
  (interactive (list
                (and (planner-derived-mode-p 'planner-mode)
                     (planner-page-name))))
  (planner-goto (or page
                    (and (planner-derived-mode-p 'planner-mode)
                         (planner-page-name))))
  ;;(if task-desc
  ;;    (planner-seek-to-first-subsection task-desc 'env)
  (planner-seek-to-first 'env)
  (save-restriction
    (when (if task-desc
              (planner-narrow-to-subsection task-desc 'env t)
            (planner-narrow-between-section-subsection 'env t))
      (let ((total 0))
        (goto-char (point-min))
        (while (re-search-forward "^\.#\\([0-9]+\\)\\s-+" nil t)
          (setq total (max total (string-to-number (match-string 1)))))
        (if planner-reverse-chronological-notes
            (progn (goto-char (point-min))
                   (forward-line 1)
                   (skip-chars-forward "\n"))
          (goto-char (point-max))
          (skip-chars-backward "\n")
          (when (= (forward-line 1) 1) (insert "\n"))
          (when (= (forward-line 1) 1) (insert "\n")))
        (insert ".#" (number-to-string (1+ total)) " ")
        (unless (eobp) (save-excursion (insert "\n\n")))
        (1+ total)))))
;; improve it.

(defun planner-page-get-env (page &optional task-desc)
  "Create a note to be remembered in PAGE (today if PAGE is nil).
If `planner-reverse-chronological-notes' is non-nil, create the
note at the beginning of the notes section; otherwise, add it to
the end.  Position point after the anchor."
  (interactive (list (and (planner-derived-mode-p 'planner-mode)
                          (planner-page-name))))
  (save-window-excursion
    (save-excursion
      (save-restriction
        (planner-goto (or page
                          (and (planner-derived-mode-p 'planner-mode)
                               (planner-page-name))))

        ;; (if task-desc
        ;;     (planner-seek-to-first-subsection task-desc 'env)
        ;;   (planner-seek-to-first 'env))

        (let (;;(total 0)
              envlist)
          (when (if task-desc
                    (planner-narrow-to-subsection task-desc 'env)
                  (planner-narrow-between-section-subsection 'env))
            (goto-char (point-min))
            (while (re-search-forward "^\.#\\([0-9]+\\)\\s-+\\([^\n]+\\)" nil t)
              (add-to-list 'envlist (match-string-no-properties 2))))
          (widen)
          envlist)))))

;;todo:
;; 1. not to open page ?? find it.

(defun planner-create-env-from-task (&optional plan-page-p)
  "Create a note based on the current task.
Argument PLAN-PAGE-P is used to determine whether we put the new
note on the task's plan page or on the current page."
  (interactive "P")
  (let ((task-info (planner-current-task-info))
         env-num)
    (when task-info
      ;; If PLAN-PAGE-P and the task has a plan page, create a note on
      ;; the plan page. If not, create it on the current page.
      (when (and plan-page-p
                 (string= (planner-task-date task-info)
                          (planner-task-page task-info)))
        (planner-jump-to-linked-task task-info))
      (setq env-num (planner-create-env (planner-page-name)))
      (save-excursion
        (save-window-excursion
          (when (planner-find-task task-info)
            (planner-edit-task-description
             (concat (planner-task-description task-info) " "
                     (planner-make-link
                      (concat (planner-page-name) "#"
                              (number-to-string env-num))
                      (format "(%d)" env-num)))))))
      (insert " " (planner-task-description task-info) "\n\n"))))


;; (defun planner-create-apps-from-task (&optional plan-page-p)
;;   "Create a note based on the current task.
;; Argument PLAN-PAGE-P is used to determine whether we put the new
;; note on the task's plan page or on the current page."
;;   (interactive "P")
;;   (let ((task-info (planner-current-task-info))
;;          note-num)
;;     (when task-info
;;       ;; If PLAN-PAGE-P and the task has a plan page, create a note on
;;       ;; the plan page. If not, create it on the current page.
;;       (when (and plan-page-p
;;                  (string= (planner-task-date task-info)
;;                           (planner-task-page task-info)))
;;         (planner-jump-to-linked-task task-info))
;;       (setq note-num (planner-create-note (planner-page-name)))
;;       (save-excursion
;;         (save-window-excursion
;;           (when (planner-find-task task-info)
;;             (planner-edit-task-description
;;              (concat (planner-task-description task-info) " "
;;                      (planner-make-link
;;                       (concat (planner-page-name) "#"
;;                               (number-to-string note-num))
;;                       "(APP)")))))))
;;       (insert " " (planner-task-description task-info) "\n\n"))))

(provide 'planner-env)
