;; org-untangle-utils


;; [[file:~/.repos/git/user/rc/xemacs/snippets/org-untangle-utils.org::*org-untangle-utils][org-untangle-utils:1]]
(defmacro with-major-mode-buffer (major-mode &rest body)
  `(with-temp-buffer
     (funcall ,major-mode)
     (progn
       ,@body)
     (buffer-substring-no-properties
      (point-min)
      (point-max))))

(defun org-babel-untangle ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (let* ((file-name-san-ext (file-name-sans-extension (file-name-nondirectory file-name)))
               (org-file-name  (concat file-name-san-ext ".org"))
               (file-extention (file-name-extension file-name))
               (tangle-file (if file-extention "yes" file-name-san-ext))
               (file-major-mode (replace-regexp-in-string
                                 "-mode\$"
                                 ""
                                 (symbol-name major-mode)))
               (first-line (save-excursion
                             (goto-char (point-min))
                             (buffer-substring-no-properties (line-beginning-position)
                                                             (line-end-position))))
               (second-line (save-excursion
                              (goto-char (point-min))
                              (forward-line 1)
                              (buffer-substring-no-properties (line-beginning-position)
                                                              (line-end-position))))
               (shebang (when (string-match auto-mode-interpreter-regexp first-line)
                          first-line))
               (file-prop-line-regexp "[ \t]*\\([^ \t\n\r:;]+\\)\\([ \t]*-\\*-\\)")
               (prop-line (cond
                            ((string-match file-prop-line-regexp first-line)
                             first-line)
                            ((string-match file-prop-line-regexp second-line)
                             second-line))))
          (if (not
               (or
                (eq file-extention "org")
                (eq 'org-mode major-mode)))
              (with-current-buffer (find-file-noselect org-file-name)
                (save-excursion
                  (widen)
                  (erase-buffer)

                  (insert (format "#+TITLE %s\n" file-name-san-ext))
                  (insert (format
                           "#+PROPERTY: header-args :tangle %s :padline yes :comments both :noweb yes\n"
                           tangle-file))
                  (insert "\n\n")
                  (insert (format "* %s\n\n" file-name-san-ext))

                  (progn
                    (insert (format "** Preamble\n"))
                    (insert
                     (concat
                      (format "#+begin_src %s :padline no :comments no :noweb no"
                              file-major-mode)
                      (when (stringp shebang)
                        (format " :shebang %s" shebang))))
                    (insert "\n")

                    (insert
                     (with-major-mode-buffer major-mode
                       (if prop-line
                           (insert (format "%s" prop-line))
                           (progn
                             (insert (format " -*- major-mode: %s; -*-" file-major-mode))
                             (progn
                               (comment-region
                                (line-beginning-position)
                                (line-end-position)))))
                       (insert "\n")))

                    (insert (format "#+end_src\n")))

                  (insert (format "** Rest\n"))
                  (insert (format "#+begin_src %s\n" file-major-mode))
                  (insert
                   (with-major-mode-buffer major-mode
                     (let ((max-point (+
                                       (point)
                                       (nth 1 (insert-file-contents file-name)))))
                       (let ((first-insert-line
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
                             (second-insert-line
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line 1)
                                (buffer-substring-no-properties (line-beginning-position)
                                                                (line-end-position)))))
                         (if (or
                              (string-match auto-mode-interpreter-regexp first-insert-line)
                              (string-match file-prop-line-regexp second-insert-line))
                             (progn
                               (kill-whole-line)
                               (when (string-match file-prop-line-regexp second-insert-line)
                                 (kill-whole-line)))
                             (when (string-match file-prop-line-regexp second-insert-line)
                               (forward-line 1)
                               (kill-whole-line)))))
                     (goto-char (point-max))))

                  (insert "#+end_src\n")
                  (save-buffer)))
              (error "%s is already is org file." file-name)))
        (error "buffer not associated with any file."))))
;; org-untangle-utils:1 ends here

;; Face correction src block
;; Can notice 'org-block face is inherited from 'src-block face
;; if we examine it than it will inherit as shadow


;; [[file:~/.repos/git/user/rc/xemacs/snippets/org-untangle-utils.org::*Face%20correction%20src%20block][Face correction src block:1]]
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
                          '(org-mode-hook)))
;; Face correction src block:1 ends here
