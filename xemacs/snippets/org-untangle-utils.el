;; org-untangle-utils


;; [[file:~/.repos/git/user/rc/xemacs/snippets/org-untangle-utils.org::*org-untangle-utils][org-untangle-utils:1]]
(defun org-babel-untangle ()
  (interactive)
  (let ((file-name (buffer-file-name))
        (file-major-mode (replace-regexp-in-string
                          "-mode\$"
                          ""
                          (symbol-name major-mode))))
    (if file-name
        (let* ((file-name-san-ext (file-name-sans-extension (file-name-nondirectory file-name)))
               (org-file-name  (concat file-name-san-ext ".org"))
               (file-extention (file-name-extension file-name))
               (tangle-file (if file-extention "yes" file-name-san-ext)))
          (if (not
               (or
                (eq file-extention "org")
                (eq 'org-mode major-mode)))
              (with-current-buffer (find-file-noselect org-file-name)
                (save-excursion
                  (erase-buffer)

                  (insert (format "#+TITLE %s\n" file-name-san-ext))
                  (insert (format
                           "#+PROPERTY: header-args :tangle %s :padline yes :comments both :noweb yes\n"
                           tangle-file))
                  (insert "\n\n")
                  (insert (format "* %s\n\n" file-name-san-ext))

                  (progn
                    (insert (format "** Preamble\n"))
                    (let ((shebang (let ((first-line (save-excursion
                                                       (goto-char (point-min))
                                                       (buffer-substring-no-properties (line-beginning-position)
                                                                                       (line-end-position)))))
                                     (when (string-match auto-mode-interpreter-regexp first-line)
                                       first-line))))
                      (insert
                       (concat
                        (format "#+begin_src %s :padline no :comments no :noweb no"
                                file-major-mode)
                        (when (stringp shebang)
                          (format " :shebang %s" shebang))))
                      (insert "\n"))
                    (insert (format "# -*- major-mode: %s; -*-\n" file-major-mode))
                    (insert (format "#+end_src\n")))

                  (insert (format "** Rest\n"))
                  (insert (format "#+begin_src %s\n" file-major-mode))
                  (goto-char (+
                              (point)
                              (nth 1 (insert-file-contents file-name))))
                  (insert "#+end_src\n")
                  (save-buffer))
                (error "%s is already is org file." file-name))))
        (error "buffer not associated with any file."))))
;; org-untangle-utils:1 ends here

;; Face correction src block
;; Can notice 'org-block face is inherited from 'src-block face
;; if we examine it than it will inherit as shadow

;; [[file:~/.repos/git/user/rc/xemacs/snippets/org-untangle-utils.org::*Face%20correction%20src%20block][Face correction src block:1]]
;; (set-face-attribute
;;    'org-block nil :foreground "#FFFFFF")

(set-face-attribute 'org-block nil :inherit 'src-block)

;; https://emacs.stackexchange.com/questions/26603/how-to-run-the-tangled-file
(unless (require 'ob-sh nil 'noerror)
  (require 'ob-shell))
;; Face correction src block:1 ends here
