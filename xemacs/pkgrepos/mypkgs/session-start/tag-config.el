;; ref: http://www.emacswiki.org/cgi-bin/wiki/TagFile
;; The following will automatically create a TAGS file from within Emacs
;; itself if none exists. Just hit `M-. and youre off.


(eval-when-compile
  '(require 'cl))

(require 'cl)
(require 'tree)


(defvar *tags-config* nil "Tags system configurations")

;; (setf (tree-node *tags-config* setupfn cscope) 'create-cscope)
;; (setf (tree-node *tags-config* setupfn etags)  'create-etags)
;; (setf (tree-node *tags-config* setupfn gtags)  'create-gtags)

(setf (tree-node *tags-config* files cscope) '("cscope.out"))
(setf (tree-node *tags-config* files etags)  '("TAGS"))
(setf (tree-node *tags-config* files gtags)  '("GTAGS" "GRTAGS" "GPATH"
                                               ;; "GSYMS"
                                               ))

(setf (tree-node *tags-config* cmd cscope) "cscope -Rb - 2>/dev/null")
(setf (tree-node *tags-config* cmd etags)  "find %s  -path '*.svn*'  -prune -o -type f | etags --output=TAGS -- 2>/dev/null")
(setf (tree-node *tags-config* cmd gtags)  "gtags -v 2>/dev/null")

;; (defun pushnew-alist (key value list)
;;   (unless (assoc key list)
;;     (pushnew (cons key nil) list :key #'car))
;;   (pushnew value (cdr (assoc key list)) :test #'string-equal))

;; (defun push-dir-in-tag-sys-alist (tag-sys dir)
;;   (pushnew-alist tag-sys dir *dirs-having-tag-files-alist*))

(defun search-upwards (files starting-path)
  ;; from: https://lists.ubuntu.com/archives/bazaar/2009q2/057669.html
  "Search for `filename' in every directory from `starting-path' up."
  (let ((path (file-name-as-directory starting-path)))
    (message "path %s" (concat path filename))
    ;; (if (file-exists-p (concat path filename))

    (if (every '(lambda (f)
                  (file-exists-p (expand-file-name f path)))
               files)
        path
        (let ((parent (file-name-directory (directory-file-name path))))
          (if (string= parent path)
              nil
              (search-upwards files parent))))))

(defun issubdirp (superdir subdir)
  ;; check if this is working proplerly.
  (message "issubdirp %s %s" superdir subdir)
  (let ((superdir (file-truename superdir))
        (subdir (file-truename subdir)))
    (string-prefix-p superdir subdir)))

(defun tag-file-existp-main (tag-sys dir)
  (if (search-upwards (tree-node *tags-config* files tag-sys) dir)
      (pushnew dir (tree-node *tags-config* dirs-cache tag-sys))))

(defun tag-file-existp (tag-sys dir)
  (message "tag-file-existp %s %s" tag-sys dir)
  (let ((dirs (tree-node *tags-config* dirs-cache tag-sys)))
    (message "tag-file-existp dirs %s" dirs)
    (if (some '(lambda (d)
                 (issubdirp d dir))
              dirs)
        t
      (tag-file-existp-main tag-sys dir))))

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

(defun create-tags-default (tag-sys dirs &optional force)
  (interactive)
  (dolist (d dirs)
    (let* ((fmt (tree-node *tags-config* cmd tag-sys))
           (cmd (read-from-minibuffer (format "%s cmd: " tag-sys) (format fmt d))))
      (let ((default-directory d))
        ;; (async-shell-command cmd)
        (shell-command-no-output cmd)))))

(defun create-tags (tag-sys dir &optional force)
  (interactive)
  (let* ((tag-dir (ido-read-directory-name (format "Directory to create %s files: " tag-sys) dir dir t))
         (dirs (list tag-dir ;; get other libdirs also like gtags libdir
                    ))
         (fun (tree-node *tags-config* setupfn cscope)))
    (when
        (if fun
            (funcall fun dirs)
          (funcall create-tags-default tag-sys dirs force))
      ;; (push-dir-in-tag-sys-alist tag-sys dir)
      (pushnew dir (tree-node *tags-config* dirs-cache tag-sys)))))

(defun create-etags (dir &optional force)
  "Create etags file."
  (let* ((fmt "find %s  -path '*.svn*'  -prune -o -type f | etags --output=TAGS -- 2>/dev/null")
         (cmd (read-from-minibuffer "etag cmd: " (format fmt dir))))
    ;; (eshell-command cmd)
    (shell-command-no-output cmd)))

(defun create-gtags (dir &optional force)
  "Create etags file."
  (let* ((fmt "gtags -v 2>/dev/null")
         (cmd (read-from-minibuffer "gtag cmd: " (format fmt dir))))
    (let ((default-directory dir))
      ;; (async-shell-command cmd)
      (shell-command-no-output cmd))))

(defun create-cscope (dir &optional force)
  "Create etags file."
  (let* ((fmt "cscope -Rb - 2>/dev/null")
         (cmd (read-from-minibuffer "cscope cmd: " (format fmt dir))))
    (let ((default-directory dir))
      ;; (async-shell-command cmd)
      (shell-command-no-output cmd))))

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
;;     (unless (tag-file-existp 'etags default-directory)
;;       (create-tags 'etags default-directory))
;;     (visit-tags-table tag-file)))

(defmacro create-tags-before (tag-sys find-fun)
  `(defadvice ,find-fun (before create-tags last () activate)
     "Automatically create tags file."
     (unless (tag-file-existp ',tag-sys default-directory)
       (create-tags ',tag-sys default-directory))))

(create-tags-before etags find-tag)
(create-tags-before gtags gtags-find-tag)
(create-tags-before cscope cscope-find-this-symbol)


;; (defadvice find-tag (before create-tags last () activate)
;;   "Automatically create tags file."
;;   (unless (tag-file-existp 'etags default-directory)
;;     (create-tags 'etags default-directory)))


;; ref: http://www.emacswiki.org/cgi-bin/wiki/EmacsTags
;; Completion
;; You can use M-x complete-tag to get simple (ie context
;; free) symbol name Completion. This works like other types of
;; completion in emacs, if there are multiple possibilities a window will
;; be opened showing them all. This used to be bound to M-TAB by default
;; but as many window managers use this to switch between windows, I tend
;; to use M-RET instead.
(global-set-key (kbd "M-<return>") 'complete-tag)



(deh-require-maybe gtags
  ;; http://www.emacswiki.org/emacs/GnuGlobal
  (setq global-supported-pgm-langs
      '(c))

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
        '(lambda ()
          (setq gtags-path-style 'relative)))

  (add-element-to-lists '(lambda ()
                          (gtags-mode 1)
                          ) global-supported-pgm-langs)

  (add-hook 'after-save-hook #'gtags-update)

  ;; http://www.emacswiki.org/emacs/CyclingGTagsResult
  (defun ww-next-gtag ()
    "Find next matching tag, for GTAGS."
    (interactive)
    (let ((latest-gtags-buffer
           (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
      (cond (latest-gtags-buffer
             (switch-to-buffer latest-gtags-buffer)
             (forward-line)
             (gtags-select-it nil)))))

  ;; Here’s my key binding for using GNU Global.


;;; http://lists.gnu.org/archive/html/help-gnu-emacs/2005-09/msg00157.html


  (autoload 'gtags-mode "gtags" nil t)

  (when (executable-find "global")

    (defadvice gtags-visit-rootdir (after make-complete-list activate)
      "Rebuilds completion list when changing GLOBAL database rootdir."
      (gtags-make-complete-list))


    (defun gtags-global-dir-p (dir)
      "Return non-nil if directory DIR contains a GLOBAL database."
      (every '(lambda (file)
                (file-exists-p (expand-file-name file dir)))
             (tree-node *tags-config* files 'gtags)))

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

      ;; (when (gtags-global-dir)
      ;;   (if (equal (call-process "global" nil nil nil "-vu") 0)
      ;;       (setq gtags-global-complete-list-obsolete-flag t)
      ;;     (error "global database update failed"))))


    )                            ; (when (executable-find "global") ...)

  ;; Use gtags in all modes for now.
  ;; (gtags-mode 1)
                                  ; (when (locate-library "gtags") ...)


  (deh-section "GTAGSLIBDIR"
    ;; (defvar gtags-libdirs 'empty "extra lib dirs")
    ;; (make-local-variable 'gtags-libdirs)
    (defvar tag-dir-config-file ".tag-dir-local.el" "extra lib dirs")
    (defvar tag-dir-config nil "tags dir config")
    (make-local-variable 'tag-dir-config)

    (defun tags-dir-store-config ()
      (let* ((readfile (expand-file-name tag-dir-config-file (gtags-root-dir))))
        (sharad/write-file readfile (prin1-to-string tag-dir-config))
        tag-dir-config))

    (defun tags-dir-restore-config ()
      (let* ((readfile (expand-file-name tag-dir-config-file (gtags-root-dir))))
        (setq tag-dir-config (sharad/read-file readfile))))

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

    ;; (when nil
    ;;   (defadvice gtags-find-tag (before set-gtags-libdirs last () activate)
    ;;     (gtags-set-env 'gtags-libdirs))

    ;;   (defadvice gtags-find-tag (after reset-gtags-libdirs last () activate)
    ;;     (gtags-reset-env)
    ;;     ad-return-value))

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
    )

  (deh-section "Combnining all tag search"

    (defun combine-find-tag ()
      (o
       (gtags-find-tag)
       (find-tag)
       (cscope-find-this-symbol)))
    )

  )


(deh-require-maybe xcscope
  ;; http://emacswiki.org/emacs/CScopeAndEmacs
  (setq cscope-do-not-update-database t)

;; 5. If you intend to use xcscope.el often you can optionally edit your
;;    ~/.emacs file to add keybindings that reduce the number of keystrokes
;;    required.  For example, the following will add "C-f#" keybindings, which
;;    are easier to type than the usual "C-c s" prefixed keybindings.  Note
;;    that specifying "global-map" instead of "cscope:map" makes the
;;    keybindings available in all buffers:
;;
;;	(define-key global-map [(control f3)]  'cscope-set-initial-directory)
;;	(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
;;	(define-key global-map [(control f5)]  'cscope-find-this-symbol)
;;	(define-key global-map [(control f6)]  'cscope-find-global-definition)
;;	(define-key global-map [(control f7)]
;;	  'cscope-find-global-definition-no-prompting)
;;	(define-key global-map [(control f8)]  'cscope-pop-mark)
;;	(define-key global-map [(control f9)]  'cscope-next-symbol)
;;	(define-key global-map [(control f10)] 'cscope-next-file)
;;	(define-key global-map [(control f11)] 'cscope-prev-symbol)
;;	(define-key global-map [(control f12)] 'cscope-prev-file)
;;      (define-key global-map [(meta f9)]  'cscope-display-buffer)
;;      (define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)
;;
;; 6. Restart (X)Emacs.  That's it.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ***** USING THIS MODULE *****
;;
;; * Basic usage:
;;
;; If all of your C/C++/lex/yacc source files are in the same
;; directory, you can just start using this module.  If your files are
;; spread out over multiple directories, see "Advanced usage", below.
;;
;; Just edit a source file, and use the pull-down or pop-up (button 3)
;; menus to select one of:
;;
;;         Find symbol
;;         Find global definition
;;         Find called functions
;;         Find functions calling a function
;;         Find text string
;;         Find egrep pattern
;;         Find a file
;;         Find files #including a file
;;
;; The cscope database will be automatically created in the same
;; directory as the source files (assuming that you've never used
;; cscope before), and a buffer will pop-up displaying the results.
;; You can then use button 2 (the middle button) on the mouse to edit
;; the selected file, or you can move the text cursor over a selection
;; and press [Enter].
;;
;; Hopefully, the interface should be fairly intuitive.
;;
;;
;; * Locating the cscope databases:
;;
;; This module will first use the variable, `cscope-database-regexps',
;; to search for a suitable database directory.  If a database location
;; cannot be found using this variable then a search is begun at the
;; variable, `cscope-initial-directory', if set, or the current
;; directory otherwise.  If the directory is not a cscope database
;; directory then the directory's parent, parent's parent, etc. is
;; searched until a cscope database directory is found, or the root
;; directory is reached.  If the root directory is reached, the current
;; directory will be used.
;;
;; A cscope database directory is one in which EITHER a cscope database
;; file (e.g., "cscope.out") OR a cscope file list (e.g.,
;; "cscope.files") exists.  If only "cscope.files" exists, the
;; corresponding "cscope.out" will be automatically created by cscope
;; when a search is done.  By default, the cscope database file is called
;; "cscope.out", but this can be changed (on a global basis) via the
;; variable, `cscope-database-file'.  There is limited support for cscope
;; databases that are named differently than that given by
;; `cscope-database-file', using the variable, `cscope-database-regexps'.
;;
;; Note that the variable, `cscope-database-regexps', is generally not
;; needed, as the normal hierarchical database search is sufficient
;; for placing and/or locating the cscope databases.  However, there
;; may be cases where it makes sense to place the cscope databases
;; away from where the source files are kept; in this case, this
;; variable is used to determine the mapping.  One use for this
;; variable is when you want to share the database file with other
;; users; in this case, the database may be located in a directory
;; separate from the source files.
;;
;; Setting the variable, `cscope-initial-directory', is useful when a
;; search is to be expanded by specifying a cscope database directory
;; that is a parent of the directory that this module would otherwise
;; use.  For example, consider a project that contains the following
;; cscope database directories:
;;
;;     /users/jdoe/sources
;;     /users/jdoe/sources/proj1
;;     /users/jdoe/sources/proj2
;;
;; If a search is initiated from a .c file in /users/jdoe/sources/proj1
;; then (assuming the variable, `cscope-database-regexps', is not set)
;; /users/jdoe/sources/proj1 will be used as the cscope data base directory.
;; Only matches in files in /users/jdoe/sources/proj1 will be found.  This
;; can be remedied by typing "C-c s a" and then "M-del" to remove single
;; path element in order to use a cscope database directory of
;; /users/jdoe/sources.  Normal searching can be restored by typing "C-c s A".
;;
;;
;; * Keybindings:
;;
;; All keybindings use the "C-c s" prefix, but are usable only while
;; editing a source file, or in the cscope results buffer:
;;
;;      C-c s s         Find symbol.
;;      C-c s d         Find global definition.
;;      C-c s g         Find global definition (alternate binding).
;;      C-c s G         Find global definition without prompting.
;;      C-c s c         Find functions calling a function.
;;      C-c s C         Find called functions (list functions called
;;                      from a function).
;;      C-c s t         Find text string.
;;      C-c s e         Find egrep pattern.
;;      C-c s f         Find a file.
;;      C-c s i         Find files #including a file.
;;
;; These pertain to navigation through the search results:
;;
;;      C-c s b         Display *cscope* buffer.
;;      C-c s B         Auto display *cscope* buffer toggle.
;;      C-c s n         Next symbol.
;;      C-c s N         Next file.
;;      C-c s p         Previous symbol.
;;      C-c s P         Previous file.
;;      C-c s u         Pop mark.
;;
;; These pertain to setting and unsetting the variable,
;; `cscope-initial-directory', (location searched for the cscope database
;;  directory):
;;
;;      C-c s a         Set initial directory.
;;      C-c s A         Unset initial directory.
;;
;; These pertain to cscope database maintenance:
;;
;;      C-c s L         Create list of files to index.
;;      C-c s I         Create list and index.
;;      C-c s E         Edit list of files to index.
;;      C-c s W         Locate this buffer's cscope directory
;;                      ("W" --> "where").
;;      C-c s S         Locate this buffer's cscope directory.
;;                      (alternate binding: "S" --> "show").
;;      C-c s T         Locate this buffer's cscope directory.
;;                      (alternate binding: "T" --> "tell").
;;      C-c s D         Dired this buffer's directory.
;;
;;
;; * Advanced usage:
;;
;; If the source files are spread out over multiple directories,
;; you've got a few choices:
;;
;; [ NOTE: you will need to have the script, "cscope-indexer",
;;   properly installed in order for the following to work.  ]
;;
;; 1. If all of the directories exist below a common directory
;;    (without any extraneous, unrelated subdirectories), you can tell
;;    this module to place the cscope database into the top-level,
;;    common directory.  This assumes that you do not have any cscope
;;    databases in any of the subdirectories.  If you do, you should
;;    delete them; otherwise, they will take precedence over the
;;    top-level database.
;;
;;    If you do have cscope databases in any subdirectory, the
;;    following instructions may not work right.
;;
;;    It's pretty easy to tell this module to use a top-level, common
;;    directory:
;;
;;    a. Make sure that the menu pick, "Cscope/Index recursively", is
;;       checked (the default value).
;;
;;    b. Select the menu pick, "Cscope/Create list and index", and
;;       specify the top-level directory.  This will run the script,
;;       "cscope-indexer", in the background, so you can do other
;;       things if indexing takes a long time.  A list of files to
;;       index will be created in "cscope.files", and the cscope
;;       database will be created in "cscope.out".
;;
;;    Once this has been done, you can then use the menu picks
;;    (described in "Basic usage", above) to search for symbols.
;;
;;    Note, however, that, if you add or delete source files, you'll
;;    have to either rebuild the database using the above procedure,
;;    or edit the file, "cscope.files" to add/delete the names of the
;;    source files.  To edit this file, you can use the menu pick,
;;    "Cscope/Edit list of files to index".
;;
;;
;; 2. If most of the files exist below a common directory, but a few
;;    are outside, you can use the menu pick, "Cscope/Create list of
;;    files to index", and specify the top-level directory.  Make sure
;;    that "Cscope/Index recursively", is checked before you do so,
;;    though.  You can then edit the list of files to index using the
;;    menu pick, "Cscope/Edit list of files to index".  Just edit the
;;    list to include any additional source files not already listed.
;;
;;    Once you've created, edited, and saved the list, you can then
;;    use the menu picks described under "Basic usage", above, to
;;    search for symbols.  The first time you search, you will have to
;;    wait a while for cscope to fully index the source files, though.
;;    If you have a lot of source files, you may want to manually run
;;    cscope to build the database:
;;
;;            cd top-level-directory    # or wherever
;;            rm -f cscope.out          # not always necessary
;;            cscope -b
;;
;;
;; 3. If the source files are scattered in many different, unrelated
;;    places, you'll have to manually create cscope.files and put a
;;    list of all pathnames into it.  Then build the database using:
;;
;;            cd some-directory         # wherever cscope.files exists
;;            rm -f cscope.out          # not always necessary
;;            cscope -b
;;
;;    Next, read the documentation for the variable,
;;    "cscope-database-regexps", and set it appropriately, such that
;;    the above-created cscope database will be referenced when you
;;    edit a related source file.
;;
;;    Once this has been done, you can then use the menu picks
;;    described under "Basic usage", above, to search for symbols.
;;
;;
;; * Interesting configuration variables:
;;
;; "cscope-truncate-lines"
;;      This is the value of `truncate-lines' to use in cscope
;;      buffers; the default is the current setting of
;;      `truncate-lines'.  This variable exists because it can be
;;      easier to read cscope buffers with truncated lines, while
;;      other buffers do not have truncated lines.
;;
;; "cscope-use-relative-paths"
;;      If non-nil, use relative paths when creating the list of files
;;      to index.  The path is relative to the directory in which the
;;      cscope database will be created.  If nil, absolute paths will
;;      be used.  Absolute paths are good if you plan on moving the
;;      database to some other directory (if you do so, you'll
;;      probably also have to modify `cscope-database-regexps').
;;      Absolute paths may also be good if you share the database file
;;      with other users (you'll probably want to specify some
;;      automounted network path for this).
;;
;; "cscope-index-recursively"
;;      If non-nil, index files in the current directory and all
;;      subdirectories.  If nil, only files in the current directory
;;      are indexed.  This variable is only used when creating the
;;      list of files to index, or when creating the list of files and
;;      the corresponding cscope database.
;;
;; "cscope-name-line-width"
;;      The width of the combined "function name:line number" field in
;;      the cscope results buffer.  If negative, the field is
;;      left-justified.
;;
;; "cscope-do-not-update-database"
;;      If non-nil, never check and/or update the cscope database when
;;      searching.  Beware of setting this to non-nil, as this will
;;      disable automatic database creation, updating, and
;;      maintenance.
;;
;; "cscope-display-cscope-buffer"
;;      If non-nil, display the *cscope* buffer after each search
;;      (default).  This variable can be set in order to reduce the
;;      number of keystrokes required to navigate through the matches.
;;
;; "cscope-database-regexps"
;; 	List to force directory-to-cscope-database mappings.
;; 	This is a list of `(REGEXP DBLIST [ DBLIST ... ])', where:
;;
;; 	REGEXP is a regular expression matched against the current buffer's
;; 	current directory.  The current buffer is typically some source file,
;; 	and you're probably searching for some symbol in or related to this
;; 	file.  Basically, this regexp is used to relate the current directory
;; 	to a cscope database.  You need to start REGEXP with "^" if you want
;; 	to match from the beginning of the current directory.
;;
;; 	DBLIST is a list that contains one or more of:
;;
;; 	    ( DBDIR )
;; 	    ( DBDIR ( OPTIONS ) )
;; 	    ( t )
;; 	    t
;;
;; 	Here, DBDIR is a directory (or a file) that contains a cscope
;; 	database.  If DBDIR is a directory, then it is expected that the
;; 	cscope database, if present, has the filename given by the variable,
;; 	`cscope-database-file'; if DBDIR is a file, then DBDIR is the path
;; 	name to a cscope database file (which does not have to be the same as
;; 	that given by `cscope-database-file').  If only DBDIR is specified,
;; 	then that cscope database will be searched without any additional
;; 	cscope command-line options.  If OPTIONS is given, then OPTIONS is a
;; 	list of strings, where each string is a separate cscope command-line
;; 	option.
;;
;; 	In the case of "( t )", this specifies that the search is to use the
;; 	normal hierarchical database search.  This option is used to
;; 	explicitly search using the hierarchical database search either before
;; 	or after other cscope database directories.
;;
;; 	If "t" is specified (not inside a list), this tells the searching
;; 	mechanism to stop searching if a match has been found (at the point
;; 	where "t" is encountered).  This is useful for those projects that
;; 	consist of many subprojects.  You can specify the most-used
;; 	subprojects first, followed by a "t", and then followed by a master
;; 	cscope database directory that covers all subprojects.  This will
;; 	cause the most-used subprojects to be searched first (hopefully
;; 	quickly), and the search will then stop if a match was found.  If not,
;; 	the search will continue using the master cscope database directory.
;;
;; 	Here, `cscope-database-regexps' is generally not used, as the normal
;; 	hierarchical database search is sufficient for placing and/or locating
;; 	the cscope databases.  However, there may be cases where it makes
;; 	sense to place the cscope databases away from where the source files
;; 	are kept; in this case, this variable is used to determine the
;; 	mapping.
;;
;; 	This module searches for the cscope databases by first using this
;; 	variable; if a database location cannot be found using this variable,
;; 	then the current directory is searched, then the parent, then the
;; 	parent's parent, until a cscope database directory is found, or the
;; 	root directory is reached.  If the root directory is reached, the
;; 	current directory will be used.
;;
;; 	A cscope database directory is one in which EITHER a cscope database
;; 	file (e.g., "cscope.out") OR a cscope file list (e.g.,
;; 	"cscope.files") exists.  If only "cscope.files" exists, the
;; 	corresponding "cscope.out" will be automatically created by cscope
;; 	when a search is done.  By default, the cscope database file is called
;; 	"cscope.out", but this can be changed (on a global basis) via the
;; 	variable, `cscope-database-file'.  There is limited support for cscope
;; 	databases that are named differently than that given by
;; 	`cscope-database-file', using the variable, `cscope-database-regexps'.
;;
;; 	Here is an example of `cscope-database-regexps':
;;
;;		(setq cscope-database-regexps
;;		      '(
;;			( "^/users/jdoe/sources/proj1"
;;			  ( t )
;;			  ( "/users/jdoe/sources/proj2")
;;			  ( "/users/jdoe/sources/proj3/mycscope.out")
;;			  ( "/users/jdoe/sources/proj4")
;;			  t
;;			  ( "/some/master/directory" ("-d" "-I/usr/local/include") )
;;			  )
;;			( "^/users/jdoe/sources/gnome/"
;;			  ( "/master/gnome/database" ("-d") )
;;			  )
;;			))
;;
;; 	If the current buffer's directory matches the regexp,
;; 	"^/users/jdoe/sources/proj1", then the following search will be
;; 	done:
;;
;; 	    1. First, the normal hierarchical database search will be used to
;;	       locate a cscope database.
;;
;; 	    2. Next, searches will be done using the cscope database
;;	       directories, "/users/jdoe/sources/proj2",
;;	       "/users/jdoe/sources/proj3/mycscope.out", and
;;	       "/users/jdoe/sources/proj4".  Note that, instead of the file,
;;	       "cscope.out", the file, "mycscope.out", will be used in the
;;	       directory "/users/jdoe/sources/proj3".
;;
;; 	    3. If a match was found, searching will stop.
;;
;; 	    4. If a match was not found, searching will be done using
;;	       "/some/master/directory", and the command-line options "-d"
;;	       and "-I/usr/local/include" will be passed to cscope.
;;
;; 	If the current buffer's directory matches the regexp,
;; 	"^/users/jdoe/sources/gnome", then the following search will be
;; 	done:
;;
;; 	    The search will be done only using the directory,
;; 	    "/master/gnome/database".  The "-d" option will be passed to
;; 	    cscope.
;;
;; 	If the current buffer's directory does not match any of the above
;; 	regexps, then only the normal hierarchical database search will be
;; 	done.
;;
;;
;; * Other notes:
;;
;; 1. The script, "cscope-indexer", uses a sed command to determine
;;    what is and is not a C/C++/lex/yacc source file.  It's idea of a
;;    source file may not correspond to yours.
;;
;; 2. This module is called, "xcscope", because someone else has
;;    already written a "cscope.el" (although it's quite old).
;;
;;
;; * KNOWN BUGS:
;;
;; 1. Cannot handle whitespace in directory or file names.
;;
;; 2. By default, colored faces are used to display results.  If you happen
;;    to use a black background, part of the results may be invisible
;;    (because the foreground color may be black, too).  There are at least
;;    two solutions for this:
;;
;;    2a. Turn off colored faces, by setting `cscope-use-face' to `nil',
;;        e.g.:
;;
;;            (setq cscope-use-face nil)
;;
;;    2b. Explicitly set colors for the faces used by cscope.  The faces
;;        are:
;;
;;            cscope-file-face
;;            cscope-function-face
;;            cscope-line-number-face
;;            cscope-line-face
;;            cscope-mouse-face
;;
;;        The face most likely to cause problems (e.g., black-on-black
;;        color) is `cscope-line-face'.

 )


;; (let ((str "/scpc:spratap@susengg-01:/home/spratap/releases/5.1/src/wnc/coord/")
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
;; (ido-is-tramp-root "/scpc:spratap@susengg-01:")
;; (ido-is-root-directory "/")


(provide 'tag-config)
