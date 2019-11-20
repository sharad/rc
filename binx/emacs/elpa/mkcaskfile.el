#!/usr/bin/emacs --script
;; -*- major-mode: emacs-lisp; -*-
(require 'package)
(require 'package-build "~/.emacs.d/core/libs/package-build.el")

;; (message "command-line-args-left: %s" command-line-args-left)

(let* (str
       (source   "(source gnu)\n(source melpa)\n(source marmalade)\n(source org)\n(source local \"~/.xemacs/elpa/upload\")")
       (init-str "(development\n (depends-on \"f\")\n (depends-on \"ecukes\")\n (depends-on \"ert-runner\")\n (depends-on \"el-mock\")")
       (file     (car command-line-args-left))
       (dir      (file-name-directory file))
       (files    (directory-files (or dir default-directory) nil ".+.el$"))
       (buff     (find-file-noselect file))
       (sexp     (with-current-buffer buff
                   (goto-char (point-min))
                   (read (current-buffer)))))


  (message "\n%s\n\n" source)
  (message "(package \"%s\" \"%s\" \"%s\")\n\n"
           (nth 1 sexp)
           ;; (nth 2 sexp)
           (package-version-join
            (package-build--valid-version
             (format-time-string "%Y%m%d.%H%M")))
           (nth 3 sexp))

  (setq str "(files")
  (dolist (f files)
    (setq str (concat str (format "\n \"%s\"" f))))
  (setq str (concat str ")\n\n"))

  (setq str (concat str init-str))

  (dolist (pkg (cadr (nth 4 sexp)))
    (setq str (concat str (format "\n (depends-on \"%s\")" (car pkg)))))
  (setq str (concat str ")"))
  (message "%s" str))







