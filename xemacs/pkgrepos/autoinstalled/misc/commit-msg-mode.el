(defgroup commit-msg-faces nil
  "Commit message faces."
  :group 'font-lock)

(defface commit-msg-summary-face
  '((t (:foreground "yellow")))
  "Face for summary line of commit message"
  :group 'commit-msg-faces)

(defvar commit-msg-summary-face 'commit-msg-summary-face)

(defface commit-msg-ignore-face
  '((t (:foreground "cyan")))
  "Face for ignored commit message lines"
  :group 'commit-msg-faces)

(defvar commit-msg-ignore-face 'commit-msg-ignore-face)

(defvar commit-msg-keywords
  '(("\\(\\`\.\*\$\\)" . commit-msg-summary-face)
    ("^\\(#\.\*\$\\)" . commit-msg-ignore-face)
    ("^\\(HG:\.\*\$\\)" . commit-msg-ignore-face)))

(defun commit-msg-mode ()
  (interactive)
  (setq major-mode 'commit-msg-mode)
  (setq mode-name "Commit-Message")
  (setq font-lock-defaults '(commit-msg-keywords t))
  (font-lock-fontify-buffer)
  (run-hooks 'commit-msg-mode-hook))

(defvar commit-msg-mode-hook nil)

(provide 'commit-msg-mode)
