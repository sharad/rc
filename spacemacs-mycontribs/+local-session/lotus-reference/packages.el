;;; packages.el --- lotus-reference layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-reference-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-reference/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-reference/pre-init-PACKAGE' and/or
;;   `lotus-reference/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-reference.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

;; TODO: https://tuhdo.github.io/c-ide.html

(defconst lotus-reference-packages
  '(
    xref
    ivy-xref
    gxref
    (ag    :location local)
    (tags  :location local)
    (gtags :location local)
    ggtags
    imenu
    function-args
    counsel-etags
    counsel-gtags
    xcscope
    elisp-slime-nav
    )
  "The list of Lisp packages required by the lotus-reference layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-reference/init-xref ()
  (use-package xref
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-reference/init-ivy-xref ()
  (use-package ivy-xref
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-reference/init-gxref ()
  (use-package gxref
      ;; https://github.com/dedi/gxref
      :defer t
      :config
      (progn
        (progn
          (use-package xref
              :defer t
              :config
              (progn
                (progn
                  (add-to-list
                   'c-mode-common-hook
                   #'(lambda ()
                       (add-to-list 'xref-backend-functions 'gxref-xref-backend t))))))))))

(defun lotus-reference/init-ag ()
  (use-package ag
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-reference/init-tags ()
  (use-package tags
      :defer t
      :config
      (progn

        (progn
          ;; https://stackoverflow.com/a/13783907
          ;; etags
          ;; M-x tags-reset-tags-table
          )

        (progn
          (setq
           tags-revert-without-query t  ;https://stackoverflow.com/questions/4096580/how-to-make-emacs-reload-the-tags-file-automatically
           ))

        (progn
          (setq
           tags-table-list
           ;; '("../../TAGS" "../TAGS" "./TAGS")
           '("../TAGS" "./TAGS")))

        (progn
          (use-package xref
              :defer t
              :config
              (progn
                (progn
                  (add-to-list
                   'c-mode-common-hook
                   #'(lambda ()
                       (add-to-list 'xref-backend-functions 'etags--xref-backend)))))))

        (progn
          (lotus-create-tags-before etags visit-tags-table-buffer)))))

(defun lotus-reference/init-gtags ()
  (use-package gtags
      :defer t
      :config
      (progn

        (progn
          (use-package gxref
              :defer t
              :config
              (progn
                (progn
                  (add-to-list
                   'c-mode-common-hook
                   #'(lambda ()
                       (add-to-list 'xref-backend-functions 'gxref-xref-backend t)))))))

        (progn
          (lotus-create-tags-before gtags gtags-find-tag)
          (lotus-create-tags-before gtags gtags-find-rtag))

        (progn
          (add-hook 'gtags-mode-hook
                    '(lambda ()
                      ; Local customization (overwrite key mapping)
                      (define-key gtags-mode-map "\C-f" 'scroll-up)
                      (define-key gtags-mode-map "\C-b" 'scroll-down)
                      ))
          (add-hook 'gtags-select-mode-hook
                    '(lambda ()
                      (setq hl-line-face 'underline)
                      (hl-line-mode 1)
                      ))
          (add-hook 'c-mode-common-hook
                    '(lambda ()
                      (gtags-mode 1)))
          ; Customization
          (setq gtags-suggested-key-mapping t)
          (setq gtags-auto-update t))))

  ;; (add-hook 'c-mode-common-hook
  ;;           '(lambda ()
  ;;             (gtags-mode 1)))
  )

(defun lotus-reference/init-ggtags ()
  (use-package ggtags
      :defer t
      :config
      (progn
        (progn
          )
        (progn
          ;; https://tuhdo.github.io/c-ide.html#orgheadline2
          (add-hook 'c-mode-common-hook
                    (lambda ()
                      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                        (ggtags-mode 1))))

          (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
          (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
          (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
          (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
          (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
          (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

          (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
          ))))

(defun lotus-reference/init-helm-gtags ()
  (use-package helm-gtags
      :defer t
      :config
      (progn
        (progn

          )

        (progn

          ;; for helm global

          ;; (setq
          ;;  helm-gtags-ignore-case t
          ;;  helm-gtags-auto-update t
          ;;  helm-gtags-use-input-at-cursor t
          ;;  helm-gtags-pulse-at-cursor t
          ;;  helm-gtags-prefix-key "\C-cg"
          ;;  helm-gtags-suggested-key-mapping t
          ;;  )
          )

        (progn

          ;; https://tuhdo.github.io/c-ide.html#orgheadline2

          ;; (require 'helm-gtags)
          ;; Enable helm-gtags-mode
          (add-hook 'dired-mode-hook 'helm-gtags-mode)
          (add-hook 'eshell-mode-hook 'helm-gtags-mode)
          (add-hook 'c-mode-hook 'helm-gtags-mode)
          (add-hook 'c++-mode-hook 'helm-gtags-mode)
          (add-hook 'asm-mode-hook 'helm-gtags-mode)

          (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
          (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
          (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
          (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
          (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
          (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)          )

        )))

(defun lotus-reference/init-imenu ()
  (use-package imenu
      :defer t
      :config
      (progn
        (progn
          )
        (progn
          ;; https://tuhdo.github.io/c-ide.html#orgheadline5
          (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
          ))))

(defun lotus-reference/init-function-args ()
  https://tuhdo.github.io/c-ide.html#orgheadline5
  (use-package function-args
      :defer t
      :config
      (progn
        (progn
          )
        (progn

          ))))

(defun lotus-reference/init-counsel-etags ()
  (use-packcounsel-etagse counsel-etags
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-reference/init-counsel-gtags ()
  (use-package counsel-gtags
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-reference/init-xcscope ()
  (use-package xcscope
      :defer t
      :config
      (progn
        (progn
          (lotus-create-tags-before cscope cscope-find-this-symbol)
          (lotus-create-tags-before cscope cscope-find-functions-calling-this-function))
        (progn
          ;; http://emacswiki.org/emacs/CScopeAndEmacs
          (setq cscope-do-not-update-database t))
              (progn
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
                ))))

(defun lotus-reference/post-init-elisp-slime-nav () ;; optional if installed via package.el
  (use-package elisp-slime-nav
      :defer t
      :config
      (progn
        (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
          (add-hook hook 'elisp-slime-nav-mode)))))



;;; packages.el ends here
