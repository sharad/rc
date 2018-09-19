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

;; https://stackoverflow.com/a/13783907
;; etags
;; M-x tags-reset-tags-table

;;{{ tags file generation
;; ref: http://www.emacswiki.org/cgi-bin/wiki/TagFile
;; The following will automatically create a TAGS file from within Emacs
;; itself if none exists. Just hit `M-. and youre off.

(defvar *lotus-tags-warn-level* :debug)
(defvar *lotus-tags-config* nil "Tags system configurations")

(setq
 *lotus-tags-config*
 '((cmd
    (gtags  . "gtags -v 2>/dev/null")
    (etags  . "find %s  -path '*.svn*'  -prune -o -type f | etags --output=TAGS - 2>/dev/null")
    (cscope . "cscope -Rb 2>/dev/null"))
   (files
    (gtags "GTAGS" "GRTAGS" "GPATH")
    (etags "TAGS")
    (cscope "cscope.out"))
   (dirs-cache
    (gtags)
    (etags)
    (cscope))
   (setupfn
    (gtags)
    (etags)
    (cscope))
   (tag-resetfun
    (etags tags-reset-tags-tables))))

;; (setq *lotus-tags-warn-level* :warning)



(defun lotus-tags-config-files (tag-sys)
  (cdr (assoc tag-sys (cdr (assoc 'files *lotus-tags-config*)))))

(defun lotus-tags-config-cmd (tag-sys)
  (cdr (assoc tag-sys (cdr (assoc 'cmd *lotus-tags-config*)))))

(defun lotus-tags-config-setupfn (tag-sys)
  (cdr (assoc tag-sys (cdr (assoc 'setupfn *lotus-tags-config*)))))

(defmacro lotus-tags-config-dirs-cache (tag-sys)
  `(cdr (assoc ,tag-sys (cdr (assoc 'dirs-cache *lotus-tags-config*)))))

(defun lotus-tags-config-tag-resetfun (tag-sys)
  (cadr (assoc tag-sys (cdr (assoc 'tag-resetfun *lotus-tags-config*)))))

(defun lotus-search-upwards (files starting-path)
  ;; from: https://lists.ubuntu.com/archives/bazaar/2009q2/057669.html
  "Search for `filename' in every directory from `starting-path' up."
  (let ((path (file-name-as-directory starting-path)))
    (lwarn 'tag *lotus-tags-warn-level* "path %s files %s" path files)
    ;; (if (file-exists-p (concat path filename))

    (if (every #'(lambda (f)
                   (file-exists-p (expand-file-name f path)))
               files)
        path
      (let ((parent (file-name-directory (directory-file-name path))))
        (if (string= parent path)
            nil
          (lotus-search-upwards files parent))))))

(defun lotus-issubdirp (superdir subdir)
  ;; check if this is working proplerly.
  (lwarn 'tag *lotus-tags-warn-level* "lotus-issubdirp %s %s" superdir subdir)
  (let ((superdir (file-truename superdir))
        (subdir (file-truename subdir)))
    (string-prefix-p superdir subdir)))

(defun lotus-lotus-tag-file-existp-main (tag-sys dir)
  (if (lotus-search-upwards (lotus-tags-config-files tag-sys) dir)
      (pushnew dir (lotus-tags-config-dirs-cache tag-sys))))

(defun lotus-tag-file-existp (tag-sys dir)
  (lwarn 'tag *lotus-tags-warn-level* "lotus-tag-file-existp %s %s" tag-sys dir)
  (let* ((dirs (lotus-tags-config-dirs-cache tag-sys))
         (cached-valid-dirs (remove-if-not #'(lambda (d)
                                               (if (lotus-issubdirp d dir)
                                                   (every #'(lambda (f)
                                                              (file-exists-p (expand-file-name f d)))
                                                          (lotus-tags-config-files tag-sys))))
                                           dirs)))
    (lwarn 'tag *lotus-tags-warn-level* "lotus-tag-file-existp dirs %s" dirs)
    ;; (if (some '(lambda (d)
    ;;              (lotus-issubdirp d dir))
    ;;           dirs)
    (if cached-valid-dirs
        t
      (lotus-lotus-tag-file-existp-main tag-sys dir))))

          ;;;;;;;;;;;;;;;;;;
;; ref: http://www.emacswiki.org/cgi-bin/wiki/BuildTags
;; Or to build a tags file for a source tree (e.g. the linux kernel) you can use something like:
;;
;;     find . -type f -iname "*.[ch]" | etags -
;;
;; Avoid using xargs with etags (e.g. find . -type f -iname *.[ch] |
;; xargs etags). For source trees with many files, xargs will execute
;; etags multiple times, overwriting the previous TAGS file on each
;; execution.
;;
;; Or to build the tags file within emacs, put this in your .emacs file:

(defun lotus-create-tags-default (tag-sys dirs &optional force)
  (interactive)
  (dolist (d dirs)
    (let* ((fmt (lotus-tags-config-cmd tag-sys))
           (cmd (read-from-minibuffer
                 (format "%s cmd: " tag-sys)
                 (format fmt
                         (if (stringp d)
                             (file-name-localname d)
                           "")))))
      (let ((default-directory d))
        ;; (async-shell-command cmd)
        (shell-command-no-output cmd)
        (funcall (lotus-tags-config-tag-resetfun tag-sys))))))

(defun lotus-create-tags (tag-sys dir &optional force)
  (interactive)
  (message "(lotus-create-tags %s %s %s)" tag-sys dir force)
  (let* ((tag-dir (ido-read-directory-name (format "Directory to create %s files: " tag-sys) dir dir t))
         (dirs (list tag-dir ;; get other libdirs also like gtags libdir
                     ))
         (fun (lotus-tags-config-setupfn tag-sys)))
    (when
        (if fun
            (funcall fun dirs)
          (funcall 'lotus-create-tags-default tag-sys dirs force))
      ;; (push-dir-in-tag-sys-alist tag-sys dir)
      (pushnew dir (lotus-tags-config-dirs-cache tag-sys))
      (funcall (lotus-tags-config-tag-resetfun tag-sys)))))


;; (defun create-c-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (eshell-command
;;    (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; (defun create-perl-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (eshell-command
;;    (format "find %s -type f -regex '.*\\.p\\(\\(l\\|m\\)\\|cgi\\)' | etags -l perl -" dir-name)))

;; This package provides a function which rebuilds the TagFile being used
;; by the current buffer. You can pre-configure the shell command based
;; on the tag file being built.
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defadvice find-tag (before c-tag-file () preactivate)
;; (defadvice find-tag (before c-tag-file () preactivate)
;; (defadvice find-tag (before c-tag-file () activate)
;; (defadvice find-tag (before c-tag-file () disable)
;; (defadvice find-tag (before c-tag-file () preactivate)

;; (defadvice find-tag (before c-tag-file last () disable)
;;   "Automatically create tags file."
;;   (let ((tag-file (concat default-directory "TAGS")))
;;     (unless (lotus-tag-file-existp 'etags default-directory)
;;       (lotus-create-tags 'etags default-directory))
;;     (visit-tags-table tag-file)))

(defmacro lotus-create-tags-before (tag-sys find-fun)
  `(defadvice ,find-fun (before lotus-create-tags last () activate)
     "Automatically create tags file."
     (unless (lotus-tag-file-existp ',tag-sys default-directory)
       (lotus-create-tags ',tag-sys default-directory))))

(when nil
  (dolist (fun '(find-tag
                 find-tag-interactive
                 tags-apropos
                 gtags-find-tag
                 gtags-find-rtag
                 cscope-find-this-symbol
                 cscope-find-functions-calling-this-function))
    (ad-remove-advice fun 'before 'create-tags)))


;;; visit-tags-table-buffer implement here

;;}}





;;{{ gtags
  ;; select a tag line from lines
(when nil
  (defun gtags-select-it (delete &optional other-win)
    (let (line file)
      ;; get context from current tag line
      (beginning-of-line)
      (if (not (looking-at "[^ \t]+[ \t]+\\([0-9]+\\)[ \t]\\([^ \t]+\\)[ \t]"))
          (gtags-pop-context)
        (setq line (string-to-number (gtags-match-string 1)))
        (setq file (gtags-decode-pathname (gtags-match-string 2)))
        (message "File before: %s" file)
        (if (and (not (file-exists-p file)) (string-match "^../" file))
            (setq file (concat "../" file)))
        (message "File after: %s" file)
        ;;
        ;; Why should we load new file before killing current-buffer?
        ;;
        ;; If you kill current-buffer before loading new file, current directory
        ;; will be changed. This might cause loading error, if you use relative
        ;; path in [GTAGS SELECT MODE], because emacs's buffer has its own
        ;; current directory.
        ;;
        (let ((prev-buffer (current-buffer)))
          ;; move to the context
          (if gtags-read-only
              (if (null other-win) (find-file-read-only file)
                (find-file-read-only-other-window file))
            (if (null other-win) (find-file file)
              (find-file-other-window file)))
          (if delete (kill-buffer prev-buffer)))
        (setq gtags-current-buffer (current-buffer))
        (goto-line line)
        (gtags-mode 1)))))

;; http://www.emacswiki.org/emacs/GnuGlobal
;; (setq global-supported-pgm-langs '(c))
(when nil
(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  ;; ido-is-tramp-root
  ;; "\\`/[^/]+[@:][^:/]+:"
  (let* ((tramp-prefix "\\`/[^/]+[@:][^:/]+:")
         (prefix (if (string-match tramp-prefix default-directory)
                     (match-string 0 default-directory)))
         (dir (with-temp-buffer
                (if (zerop (process-file "global" nil t nil "-pr"))
                    (buffer-substring (point-min) (1- (point-max)))
                  nil))))
    (concat  prefix dir)))

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  ;; ido-is-tramp-root
  (let* ((prefix (tramp-file-prefix default-directory))
         (dir (with-temp-buffer
                (if (zerop (process-file "global" nil t nil "-pr"))
                    (buffer-substring (point-min) (1- (point-max)))
                  nil))))
    (concat  prefix dir)))

(defun gtags-update-synchronously ()
  "Make GTAGS incremental update, synchronously."
  (call-process "global" nil nil nil "-u"))

(defun gtags-update-asynchronously ()
  "Make GTAGS incremental update, asynchronously."
  (if (eq (process-status "global") 'nil)
      (start-process "global" "global-update" "global" "-u")))

;; test (start-process "global" "global-update" "sleep" "10")
;; default-directory

;; (defun gtags-global-update ()
;;   "If current directory is part of a GLOBAL database update it."
;;   (interactive)
;;   (when (gtags-global-dir)
;;     (if (equal (call-process "global" nil nil nil "-vu") 0)
;;         (setq gtags-global-complete-list-obsolete-flag t)
;;       (error "global database update failed"))))



(defun gtags-update ()
  (when (gtags-root-dir)
    (gtags-update-asynchronously)))

(setq gtags-mode-hook
      #'(lambda ()
          (setq gtags-path-style 'relative)))

;; (add-element-to-lists '(lambda ()
;;                         (gtags-mode 1))
;;                       global-supported-pgm-langs)

(add-hook 'after-save-hook #'gtags-update)

;; http://www.emacswiki.org/emacs/CyclingGTagsResult
(defun ww-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil  (mapcar #'(lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (forward-line)
           (gtags-select-it nil)))))

;; Here’s my key binding for using GNU Global.


;; http://lists.gnu.org/archive/html/help-gnu-emacs/2005-09/msg00157.html

;; when (executable-find "global")

;; TODO: check if required
(defadvice gtags-visit-rootdir (after make-complete-list activate)
  "Rebuilds completion list when changing GLOBAL database rootdir."
  (gtags-make-complete-list))


(defun gtags-global-dir-p (dir)
  "Return non-nil if directory DIR contains a GLOBAL database."
  (every #'(lambda (file)
             (file-exists-p (expand-file-name file dir)))
         (lotus-tags-config-files 'gtags)))

(defun gtags-global-dir (&optional dir)
  "Return the nearest super directory that contains a GLOBAL database."
  (interactive)
  (when (null dir)
    (setq dir default-directory))
  (cond ((gtags-global-dir-p dir) dir)
        ((equal (file-truename dir) (file-truename "/")) nil)
        (t (gtags-global-dir (file-name-as-directory
                              (expand-file-name ".." dir))))))

(defvar gtags-global-complete-list-obsolete-flag nil
  "When non-nil, the GLOBAL complete list should be rebuilt.")

(defun gtags-global-update ()
  "If current directory is part of a GLOBAL database update it."
  (interactive)
  (gtags-update-asynchronously))
)
;; (when (gtags-global-dir)
;;   (if (equal (call-process "global" nil nil nil "-vu") 0)
;;       (setq gtags-global-complete-list-obsolete-flag t)
;;     (error "global database update failed"))))


;; (when (executable-find "global") ...)

;;}}


;;{{ gtags other directories inclusion

;; Use gtags in all modes for now.
;; (gtags-mode 1)
; (when (locate-library "gtags") ...)


;; "GTAGSLIBDIR"
;; (defvar gtags-libdirs 'empty "extra lib dirs")
;; (make-local-variable 'gtags-libdirs)
(defvar tag-dir-config-file ".tag-dir-local.el" "extra lib dirs")
(defvar tag-dir-config nil "tags dir config")
(make-local-variable 'tag-dir-config)

(defun tags-dir-store-config ()
  (let* ((readfile (expand-file-name tag-dir-config-file (gtags-root-dir))))
    (lotus-write-file readfile (prin1-to-string tag-dir-config))
    tag-dir-config))

(defun tags-dir-restore-config ()
  (let* ((readfile (expand-file-name tag-dir-config-file (gtags-root-dir))))
    (setq tag-dir-config (lotus-read-sexp readfile))))

(defun tags-dir-get-config (variable)
  (interactive
   (let ((variable (intern
                    (ido-completing-read "variable: " '("gtags-libdirs") nil t))))
     (list variable)))
  (if (or tag-dir-config (tags-dir-restore-config))
      (or (cdr (assoc variable tag-dir-config))
          (tags-dir-set-config variable))
    (tags-dir-set-config variable)))

(defun tags-dir-set-config (variable)
  (interactive
   (let ((variable (intern
                    (ido-completing-read "variable: " '("gtags-libdirs") nil t))))
     (list variable)))
  (pushnew (list variable) tag-dir-config :key 'car)
  (push (ido-read-directory-name "gtags dir: ")
        (cdr (assoc variable tag-dir-config)))
  (tags-dir-store-config)
  (cdr (assoc variable tag-dir-config)))

(defun gtags-set-env (envar)
  (let* ((dirs (tags-dir-get-config envar)))
    (when dirs
      (let ((gtagslibpath-env (mapconcat 'identity dirs ":")))
        (push (concat "GTAGSLIBPATH=" gtagslibpath-env) process-environment)
        ;; (setenv "GTAGSLIBPATH" gtagslibpath-env)
        (message "gtags-libdirs %s" dirs)))))

(defun gtags-reset-env ()
  (pop process-environment))

(defadvice gtags-find-tag (around set-gtags-libdirs last () activate)
  (gtags-set-env 'gtags-libdirs)
  ad-do-it
  (gtags-reset-env))

;; (ad-disable-advice 'gtags-find-tag 'before 'set-gtags-libdirs)
;; (ad-enable-advice 'gtags-find-tag 'before 'set-gtags-libdirs)
;; (ad-update 'gtags-find-tag)
;; (ad-activate 'gtags-find-tag)

;; make dir-local variable. -- will not work
;; keep a seperate file .el in same dir where GTAGS files are present.

;; defadvice set GTAGSLIBPATH before global query

;;}}








;; (let ((str "/scp:spratap@susengg-01:/home/spratap/releases/5.1/src/wnc/coord/")
;;       (regexs (list
;;                tramp-file-name-regexp
;;                tramp-file-name-regexp-unified
;;                tramp-file-name-regexp-url
;;                tramp-root-regexp
;;                tramp-domain-regexp
;;                tramp-user-regexp
;;                tramp-prefix-domain-regexp
;;                "\\`/[^:/][^:/]+:\\'"
;;                "\\`/[^/]+[@:][^:/]+:/")))
;;   (message "start")
;;   (dolist (r regexs)
;;     (string-match r str)
;;     (message "aa: %s %s" r (match-string 0 str))))
;; (ido-is-tramp-root "/scp:spratap@susengg-01:")
;; (ido-is-root-directory "/")



;;; config.el ends here
