;;; packages.el --- lotus-dev layer packages file for Spacemacs.
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
;; added to `lotus-dev-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-dev/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-dev/pre-init-PACKAGE' and/or
;;   `lotus-dev/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-devS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-dev-packages
  '(
    ;; (PACKAGE :location local)
    which-func
    (member-functions :location local)
    (devel-notes :location local)
    sidebrain
    subword
    (simple-call-tree+ :location local)
    fm
    outline
    outline-magic
    (develock :location local)
    (cclookup :location local)
    (ya-cppref :location local)
    (devhelp-index :location local)
    semantic
    (semantic/find :location local)
    (semantic/ia :location local)
    (semantic/bovine/gcc :location local)
    (eassist :location local)
    cedet
    (cedet-global :location local)
    gud
    (gdb-ui :location local)
    gdb-mi
    (guess-offset :location local)
    (guess-style :location local)
    minimap
    (doxymacs :location local)
    (cc-cmds :location local)
    c-eldoc
    tramp
    compile
    paths-mapper
    (cc-vars :location local)
    disaster
    cc-mode
    session
    desktop
    uncrustify-mode
    srefactor
    (srefactor-lisp :location local)
    python)
  "The list of Lisp packages required by the lotus-dev layer.

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


;; TODO: [[http://tuhdo.github.io/c-ide.html][C/C++ Development Environment for Emacs]]

(defun lotus-dev/init-which-func ()
  (use-package which-func
      ;; :defer t
      :ensure t
      :config
      (progn
        (which-function-mode 1)
        (defface which-func
            '((((class color) (min-colors 88) (background dark)) (:foreground "Green")))
          "which-face")
        (defun copy-current-function ()
          (interactive)
          (let ((fun-name (which-function)))
            (if fun-name
                (kill-new fun-name)
                (message "Not able to get function.")))))))

(defun lotus-dev/init-member-functions ()
  (use-package member-functions
      :defer t
      :config
      (progn
        ;; for C++ mode
        )))

(defun lotus-dev/init-devel-notes ()
  (use-package devel-notes
      :defer t
      :config
      (progn
        ;; http://www.emacswiki.org/emacs/DevelNotes
        )))

(defun lotus-dev/post-init-sidebrain ()
  (use-package sidebrain
      :defer t
      :config
      (progn
        ;; (when nil
        ;;http://www.emacswiki.org/emacs/SideBrain
        ;;http://sidebrain.sourceforge.net/manual/index.html
        ;; (add-hook 'find-file-hook
        ;;           #'sidebrain-read-todo-from-comments)

        ;; (add-hook 'lotus-enable-login-session-interrupting-feature-hook
        ;;           #'(lambda ()
        ;;               (add-hook 'find-file-hook
        ;;                         #'sidebrain-read-todo-from-comments)) t)

        ;; (add-hook 'lotus-disable-login-session-interrupting-feature-hook
        ;;           #'(lambda ()
        ;;               (remove-hook 'find-file-hook
        ;;                            #'sidebrain-read-todo-from-comments)) t)


        ;; (xml-parse-file sidebrain-file-name)
        ;; (defun filter-ws (tree)
        ;;   (if (listp tree)
        ;;       (mapcar 'tree tree)
        ;;       tree))

        ;; xmllint --noblanks ~/.sidebrain.xml | tr -d '\n' |  sed 's/>[[:space:]]\+</></g'
        ;; (xml-parse-file "~/x.xml" )
        ;; (defun filter-ws (tree)
        ;;   (if (listp tree)
        ;;       (mapcar 'tree tree)
        ;;       tree))
        )))

(defun lotus-dev/post-init-subword ()
  (use-package subword
      :defer t
      :config
      (progn
        ;; (global-subword-mode 1)
        (with-eval-after-load "basic-utils"
          (add-element-to-lists '(lambda nil (subword-mode 1)) pgm-langs)))))

(defun lotus-dev/init-simple-call-tree ()
  (use-package simple-call-tree+
      :defer t
      :config
      (progn
        (progn
          (defun left-word (&optional arg)
            (interactive)
            (backward-word arg))

          (defun right-word (&optional arg)
            (interactive)
            (forward-word arg))))))

(defun lotus-dev/init-simple-call-tree+ ()
  (use-package simple-call-tree+
      :defer t
      :config
      (progn
        (progn
          (defun left-word (&optional arg)
            (interactive)
            (backward-word arg))

          (defun right-word (&optional arg)
            (interactive)
            (forward-word arg))))))

(defun lotus-dev/init-fm ()
  (use-package fm
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-outline ()
  (use-package outline
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-outline-magic ()
  (use-package outline-magic
      :defer t
      :config
      (progn
        (progn
          (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))))

(defun lotus-dev/init-develock ()
  (use-package develock
      ;; :defer t
      :demand t
      :config
      (progn
        ;; remove-useless-whitespace
        ;; develock-mode
        ;for now
        (when (ad-find-advice 'indent-region 'around 'remove-useless-whitespace)
          (ad-remove-advice 'indent-region 'around 'remove-useless-whitespace)
          (ad-activate 'indent-region)
          (ad-update  'indent-region)))))

(defun lotus-dev/init-gprof ()
  (use-package gprof
      :mode ("\\.g?prof\\'" . gprof-mode)
      :defer t
      :config
      (progn
        (autoload 'gprof-mode "gprof" "Switch to the gprof major mode" t)
        (add-to-list 'auto-mode-alist '("\\.g?prof\\'" . gprof-mode)))))

(defun lotus-dev/init-cclookup ()
  (use-package cclookup
      :commands (cclookup-lookup cclookup-update)
      :defer t
      :config
      (progn
        ;; this problem I were facing http://www.gnu.org/software/emacs/manual/html_node/elisp/Init-File.html
        ;; taesoo.org/proj/cclookup.html
        ;; http://www.cppreference.com/wiki
        ;; http://github.com/tsgates/cclookup

        ;; add cclookup to your loadpath, ex) "~/.lisp/addons/cclookup"
        (setq ccreference-dir "~/.doc/html/cppreference/"
              cclookup-dir (find-library-directory "cclookup")
              ;; set executable file and db file
              cclookup-program (concat cclookup-dir "/cclookup.py")
              cclookup-db-file (concat ccreference-dir "/cclookup.db"))

        ;; (push
        ;;  (list "text/html" (concat "file://" (expand-file-name ccreference-dir)) nil nil)
        ;;  w3m-content-type-alist)

        ;; (push
        ;;  (list "text/html" (concat "file:/" (expand-file-name ccreference-dir)) nil nil)
        ;;  w3m-content-type-alist)

        ;; to speedup, just load it on demand
        (autoload 'cclookup-lookup "cclookup"
          "Lookup SEARCH-TERM in the C++ Reference indexes." t)
        (autoload 'cclookup-update "cclookup"
          "Run cclookup-update and create the database at `cclookup-db-file'." t))))

(defun lotus-dev/init-ya-cppref ()
  (use-package ya-cppref
      :defer t
      :config
      (progn
        ;; git@github.com:whitypig/ya-cppref.git
        (setq ya-cppref-path-to-doc-root "/usr/share/doc/c-cpp-reference/"
              ;; "/usr/local/doc/cpp/reference/"
              ))))

(defun lotus-dev/init-devhelp-index ()
  (use-package devhelp-index
      :defer t
      :config
      (progn
        (progn
          ;; https://github.com/martine/devhelp-index
          ;; TODO: Add completion
          ;; http://askubuntu.com/questions/287526/how-to-add-more-languages-docs-to-devhelp-java-etc
          ;; refrese devhelp
          (defun devhelp-make-index ()
            (interactive)
            "Read a search query from the minibuffer."
            (let ((dir (find-library-directory "devhelp-index")))
              (shell-command (concat dir "/devhelp-index.py"))))))))

(defun lotus-dev/post-init-semantic ()
  (use-package semantic
    :defer t
    :config
    (progn
      (progn
        (setq
         semantic-symref-tool "global"
         ;;https://emacs.stackexchange.com/questions/5886/setting-minimum-time-between-consecutive-reparses-by-semantic
         ;;https://emacs.stackexchange.com/a/5896
         semantic-idle-scheduler-idle-time 10 ;1 is too less

         ))

      (progn
        (use-package cedet
          :defer t
          :config
          (progn
            ;; if you want to enable support for gnu global
            (if (and
                 (fboundp 'cedet-gnu-global-version-check)
                 (cedet-gnu-global-version-check t))
                (progn
                  (semanticdb-enable-gnu-global-databases 'c-mode)
                  (semanticdb-enable-gnu-global-databases 'c++-mode))
              (message "function cedet-gnu-global-version-check is not available, not doing semanticdb-enable-gnu-global-databases"))

            ;; enable ctags for some languages:
            ;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
            (when (and
                   (fboundp 'cedet-ectag-version-check)
                   (cedet-ectag-version-check t))
              (semantic-load-enable-primary-exuberent-ctags-support)))))

      (progn
        ;; http://www.emacswiki.org/emacs/eassist.el
        ;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
        ;; (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)

        (use-package imenu
          :defer t
          :config
          (progn
            (progn
              ;; (defun semantic-init-add-tags ()
              ;;   (imenu-add-to-menubar "TAGS"))
              (add-hook 'semantic-init-hooks
                        '(lambda ()
                           (imenu-add-to-menubar "TAGS"))))))))))

(defun lotus-dev/init-semantic/find ()
  (use-package semantic/find
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-semantic/ia ()
  (use-package semantic/ia
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-semantic/bovine/gcc ()
  (use-package semantic/bovine/gcc
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-eassist ()
  (use-package eassist
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-cedet ()
  (use-package cedet
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-cedet-global ()
  (use-package cedet-global
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-gud ()
  (use-package gud
      :defer t
      :config
      (progn
        (progn
          ;; make command for asking remotehost:remoteport for gdbserver
          ;; *** On Embedded Device
          ;; gdbserver localhost:PORT program arg
          ;; *** On build machine
          ;; gdb
          ;; (gdb) target remote EmbeddedDeviceIP:PORT
          ;; (gdb)
          ;; or
          ;; gdb -s /path/to/symbolfile.dbg
          ;; or
          ;; gdb -s /path/to/excutable-with-symbol
          (defadvice gdb-send-item (before gdb-save-history first nil activate)
            "write input ring on quit"

            (if (equal (type-of item) 'string) ; avoid problems with some horrible, seemingly unprintable structures sent to this function..

                (if (string-match "^q\\(u\\|ui\\|uit\\)?$" item)
                    (progn (comint-write-input-ring)
                           (message "history file '%s' written" comint-input-ring-file-name)))))

          (defun gdb-remote (command location)
            ;; not working.
            (interactive
             (let ((command (gud-query-cmdline 'gdb (format "--annotate=3 -n -s %s" default-directory)))
                   (location (or
                              (read-from-minibuffer "location: "
                                                    (concat "remote "
                                                            (or (tramp-file-name-host (tramp-file-connection default-directory)) "localhost")
                                                            ":1717"))
                              "remote localhost:1717")))
               (list command location)))
            (let ()
              (gdb command)
              ;; (setq gdb-output-sink 'user)
              (gud-refresh)
              (let ((process (get-buffer-process gud-comint-buffer)))
                (if process
                    (gdb-send process (concat "target " location))
                    (message "no process")))
              (gud-refresh)))

          ;; (gud-query-cmdline 'gdb (format " --annotate=3 -n -s %s" default-directory))
          ;; (gud-val 'command-name 'gdb)
          ;; (gud-symbol 'command-name t 'gdb)
          ;; (gud-symbol 'history nil 'gdb)
          ;; (setq gud-gdb-history-old gud-gdb-history
          ;;       gud-gdb-history nil)
          ;; (setq gdb-output-sink 'user)
          )

        (progn ;; "correction"
          (setq
           ;; gud-gdb-command-name (concat gud-gdb-command-name " -n -s SYMBOLFILE")
           gud-gdb-command-name "gdb --annotate=3 -n"
           gdb-many-windows t)

          (add-hook 'gdb-mode-hook
                    '(lambda()
                      (comint-read-input-ring t)
                      (setq
                       comint-input-ignoredups t
                       comint-input-ring-size 1000
                       comint-input-ring-file-name "~/.gdbhist")
                      (add-hook
                       'kill-buffer-hook
                       'comint-write-input-ring nil t)))

          ;; find-file-noselect
          ;; find-file
          ;; call fails over tramp

          (defalias 'gud-find-file-noselect 'gud-find-file)

          (defun gdb-get-location (bptno line flag)
            "Find the directory containing the relevant source file.
Put in buffer and place breakpoint icon."
            (goto-char (point-min))
            (catch 'file-not-found
              (if (search-forward "Located in " nil t)
                  (when (looking-at "\\S-+")
                    (delete (cons bptno "File not found") gdb-location-alist)
                    (push (cons bptno (match-string 0)) gdb-location-alist))
                  (gdb-resync)
                  (unless (assoc bptno gdb-location-alist)
                    (push (cons bptno "File not found") gdb-location-alist)
                    (message-box "Cannot find source file for breakpoint location.\n\
Add directory to search path for source files using the GDB command, dir."))
                  (throw 'file-not-found nil))
              (with-current-buffer
                  (gud-find-file-noselect (match-string 0))
                (gdb-init-buffer)
                ;; only want one breakpoint icon at each location
                (save-excursion
                  (goto-char (point-min))
                  (forward-line (1- (string-to-number line)))
                  (gdb-put-breakpoint-icon (eq flag ?y) bptno))))))

        (progn
          ))))

(defun lotus-dev/init-gdb-ui ()
  (use-package gdb-ui
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/post-init-gdb-mi ()
  (use-package gdb-mi
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-guess-offset ()
  (use-package guess-offset
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-guess-style ()
  (use-package guess-style
      :defer t
      :config
      (progn
        (progn
          ;; http://nschum.de/src/emacs/guess-style/
          ;; To guess variables when a major mode is loaded, add
          ;; guess-style-guess-all to that mode's hook like this: (add-hook
          ;; 'c-mode-common-hook 'guess-style-guess-all)

          ;; To (permanently) override values use guess-style-set-variable.
          ;; To change what variables are guessed, customize
          ;; guess-style-guesser-alist.

          ;; To show some of the guessed variables in the mode-line, enable
          ;; guess-style-info-mode.  You can do this by adding this to your
          ;; .emacs:

          (global-guess-style-info-mode 1)

          (add-hook
           'c-mode-common-hook
           'guess-style-guess-all)))))

(defun lotus-dev/init-minimap ()
  (use-package minimap
      :commands (minimap-create)
      :defer t
      :config
      (progn
        (setq minimap-window-location 'right))))

(defun lotus-dev/init-doxymacs ()
  (use-package doxymacs
      :defer t
      :config
      (progn
        ;; check out http://www.emacswiki.org/emacs/IndentingC
        (progn
          ;; http://doxymacs.sourceforge.net/
          ;; Default key bindings are:
          ;; C-c d ? will look up documentation for the symbol under the point.
          ;; C-c d r will rescan your Doxygen tags file.
          ;; C-c d f will insert a Doxygen comment for the next function.
          ;; C-c d i will insert a Doxygen comment for the current file.
          ;; C-c d ; will insert a Doxygen comment for a member variable on the current line (like M-;).
          ;; C-c d m will insert a blank multi-line Doxygen comment.
          ;; C-c d s will insert a blank single-line Doxygen comment.
          ;; C-c d @ will insert grouping comments around the current region.

          (add-hook 'c-mode-common-hook
                    (lambda ()
                      ;; (require 'doxymacs)
                      (doxymacs-mode t)
                      (doxymacs-font-lock)))))))

(defun lotus-dev/init-cc-cmds ()
  (use-package cc-cmds
      :defer t
      :config
      (progn
        (use-package cc-cmds-modified
            :defer t
            :config
            (progn
              )))))

(defun lotus-dev/init-c-eldoc ()
  (use-package c-eldoc
      :defer t
      :config
      (progn
        (progn
          ;; http://tenkan.org/~tim/c-function-signature.html
          ;; make 'function-synopsis a new thing for thing-at-point

          (deh-section "correction"
            (defun c-eldoc-get-buffer (function-name)
              "Call the preprocessor on the current file"
              ;; run the first time for macros
              (let ((output-buffer (cache-gethash (current-buffer) c-eldoc-buffers)))
                (if output-buffer output-buffer
                    (let* ((this-name (concat "*" buffer-file-name "-preprocessed*"))
                           (preprocessor-command (concat c-eldoc-cpp-command " "
                                                         c-eldoc-cpp-macro-arguments " "
                                                         c-eldoc-includes " "
                                                         (file-name-localname buffer-file-name)))
                           (cur-buffer (current-buffer))
                           (output-buffer (generate-new-buffer this-name)))
                      (bury-buffer output-buffer)
                      (process-file-shell-command preprocessor-command nil output-buffer nil)
                      ;; run the second time for normal functions
                      (setq preprocessor-command (concat c-eldoc-cpp-command " "
                                                         c-eldoc-cpp-normal-arguments " "
                                                         c-eldoc-includes " "
                                                         (file-name-localname buffer-file-name)))
                      (process-file-shell-command preprocessor-command nil output-buffer nil)
                      (cache-puthash cur-buffer output-buffer c-eldoc-buffers)
                      output-buffer)))))

          (put 'function-synopsis 'beginning-op
               (lambda ()
                 (if (bolp) (forward-line -1) (beginning-of-line))
                 (skip-chars-forward "^{")
                 (dotimes (i 3) (backward-sexp))))

          (put 'function-synopsis 'end-op
               (lambda () (skip-chars-forward "^{")))

          (setq c-eldoc-cpp-command "/usr/bin/cpp")

          ;; override eldoc-mode's doc printer thingy
          (defadvice eldoc-print-current-symbol-info
              (around eldoc-show-c-tag activate)
            (if (or (eq major-mode 'c-mode)
                    (eq major-mode 'c++-mode))
                (unless (show-tag-in-minibuffer)
                  ad-do-it)
                ad-do-it))

          (defun cleanup-function-synopsis (f)
            ;; nuke newlines
            (setq f (replace-regexp-in-string "\n" " " f))
            ;; nuke comments (note non-greedy *? instead of *)
            (setq f (replace-regexp-in-string "/\\*.*?\\*/" " " f))
            ;; (just-one-space)
            (setq f (replace-regexp-in-string "[ \t]+" " " f))
            f)

          ;; fetch a tag, jump to it, grab what looks like a function synopsis,
          ;; and output it in the minibuffer.
          (defun show-tag-in-minibuffer ()
            (when tags-table-list
              (let ((idle-time (or (current-idle-time) '(0 0 0))))
                (if (> (float-time idle-time) 2)
                    (save-excursion
                      ;; shadow some etags globals so they won't be modified
                      (let ((tags-location-ring (make-ring find-tag-marker-ring-length))
                            (find-tag-marker-ring (make-ring find-tag-marker-ring-length))
                            (last-tag nil))
                        (let* ((tag (funcall
                                     (or find-tag-default-function
                                         (get major-mode 'find-tag-default-function)
                                         'find-tag-default)))
                               ;; we try to keep M-. from matching any old tag all the
                               ;; time
                               (tag-regex (format "\\(^\\|[ \t\n*]\\)%s\\($\\|(\\)"
                                                  (regexp-quote tag))))
                          ;; (set-buffer (find-tag-noselect tag-regex nil t))
                          (set-buffer (find-tag-noselect tag nil t))
                          (let ((synopsis (or (thing-at-point 'function-synopsis)
                                              (thing-at-point 'line))))
                            (message "synopsis: %s %s" synopsis (find-tag-noselect tag-regex nil t))
                            (when synopsis
                              (eldoc-message "%s"
                                             (cleanup-function-synopsis synopsis))
                              t)))))))))

          ;; (find-tag-noselect (format "\\(^\\|[ \t\n*]\\)%s\\($\\|(\\)" (regexp-quote (find-tag-default))) nil t)
          ;; (find-tag-noselect (regexp-quote (find-tag-default)) nil t)

          (when nil
            (ad-disable-advice 'eldoc-print-current-symbol-info 'around 'eldoc-show-c-tag)
            (ad-remove-advice 'eldoc-print-current-symbol-info 'around 'eldoc-show-c-tag)
            (ad-update 'eldoc-print-current-symbol-info))

          ;; turn it on
          (use-package cc-vars
              :defer t
              :config
              (progn
               (add-hook 'c-mode-common-hook 'turn-on-eldoc-mode)))

          (deh-section "celdoc"
            (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ -I/usr/include/")
            (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode))))))

(defun lotus-dev/post-init-tramp ()
  (use-package tramp
      :defer t
      :config
      (progn

        (progn
          ;; Utility functions.
          (defun tramp-compile (command)
            "Compile on remote host."
            (interactive
             (if (or compilation-read-command current-prefix-arg)
                 (list (read-from-minibuffer "Compile command: "
                                             compile-command nil nil
                                             '(compile-history . 1)))
                 (list compile-command)))
            (setq compile-command command)
            (save-some-buffers (not compilation-ask-about-save) nil)
            (let ((d default-directory))
              (save-excursion
                (pop-to-buffer (get-buffer-create "*Compilation*") t)
                (erase-buffer)
                (setq default-directory d)))
            (tramp-handle-shell-command command (get-buffer "*Compilation*"))
            (pop-to-buffer (get-buffer "*Compilation*"))
            (tramp-minor-mode 1)
            (compilation-minor-mode 1))

          (defun tramp-recompile ()
            "Re-compile on remote host."
            (interactive)
            (save-some-buffers (not compilation-ask-about-save) nil)
            (tramp-handle-shell-command compile-command (get-buffer "*Compilation*"))
            (pop-to-buffer (get-buffer "*Compilation*"))
            (tramp-minor-mode 1)
            (compilation-minor-mode 1))

          (remove-hook 'find-file-hooks 'tramp-minor-mode)
          (remove-hook 'dired-mode-hook 'tramp-minor-mode)))))

(defun lotus-dev/post-init-compile () ;core-mod
  (use-package compile
    :defer t
    :config
    (progn
      (setq
       compilation-auto-jump-to-first-error t
       compilation-skip-threshold 2))))  ;; 2 - errors, 1 - warnings

;; TODO: solve double dependency issue once for all.
(defun lotus-dev/init-paths-mapper () ;core-mod
  (use-package paths-mapper
    :defer t
    :config
    (progn
      ))

  (use-package compile
    :defer t
    :config
    (progn
      (progn
        (use-package compile-paths-mapper
          :defer t
          :config
          (progn
            (progn
              (compile-paths-mapper-insinuate)))))
      (progn
        (when (fboundp 'compile-paths-mapper-insinuate)
          (compile-paths-mapper-insinuate))))))

(defun lotus-dev/init-cc-vars ()
  (use-package cc-vars
    :defer t
    :config
    (progn
      ;; https://www.emacswiki.org/emacs/IndentingC
      (progn

        (add-hook
         'c-mode-common-hook
         '(lambda ()
            (setq
             c-default-style "stroustrup"
             c-basic-offset 2)))

        ;; Automatic Indentation

        ;; Add the following to your ~/.emacs file. Whenever you type certain
        ;; characters, a newline will be inserted automatically. Some like it,
        ;; some hate it.

        (use-package cc-cmds
          :defer t
          :config
          (progn
            (add-hook
             'c-mode-common-hook
             #'(lambda ()
                 ;; ref: auto-newline
                 ;; ref: c-toggle-auto-newline
                 ;; ref: c-toggle-auto-state
                 ;; (c-toggle-auto-newline 1)
                 (c-toggle-auto-newline -1)))))

        ;; If you like this you might also be interested in
        ;; ‘c-toggle-hungry-state’, which will delete all characters until
        ;; next non-whitespace when you delete whitespace. Another form of
        ;; Automatic Indentation

        ;; For people who don’t like automatic indentation, but don’t want to
        ;; hit tab on every line, here’s another method:
        (use-package cc-mode
          :defer t
          :config
          (progn
            (add-hook
             'c-mode-common-hook
             '(lambda ()
                (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))))))

      ;; This maps newline-and-indent (normally C-j) to the return key. It’s
      ;; exactly equivalent to hitting tab after every time you hit return.

      ;; Note: In order to add this to your .emacs you must add `(require
      ;; ‘cc-mode)’ if you don’t have it already.




      (progn ;; "if0"
        ;; http://stackoverflow.com/questions/4549015/in-c-c-mode-in-emacs-change-face-of-code-in-if-0-endif-block-to-comment-f

        (defun my-c-mode-font-lock-if0 (limit)
          (save-restriction
            (widen)
            (save-excursion
              (goto-char (point-min))
              (let ((depth 0) str start start-depth)
                (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
                  (setq str (match-string 1))
                  (if (string= str "if")
                      (progn
                        (setq depth (1+ depth))
                        (when (and (null start) (looking-at "\\s-+0"))
                          (setq start (match-end 0)
                                start-depth depth)))
                    (when (and start (= depth start-depth))
                      (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
                      (setq start nil))
                    (when (string= str "endif")
                      (setq depth (1- depth)))))
                (when (and start (> depth 0))
                  (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
          nil)

        (defun my-c-mode-common-hook ()
          (font-lock-add-keywords
           nil
           '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

        (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))

      (progn ;; "if0"
        ;; http://alex-epico.blogspot.in/2011/09/how-to-high-light-if-0-in-emacs.html
        (defun my-cpp-highlight ()
          "highlight c/c++ #if 0 #endif macros"
          ;; (interactive)
          (setq cpp-known-face 'default)
          (setq cpp-unknown-face 'default)
          (setq cpp-known-writable 't)
          (setq cpp-unknown-writable 't)
          (setq cpp-edit-list '(("0" font-lock-comment-face default both)
                                ("1" default font-lock-comment-face both)))
          (cpp-highlight-buffer t))

        (add-hook 'c-mode-common-hook 'my-cpp-highlight))

      (progn
        (with-eval-after-load "session"
          (add-to-list 'session-locals-include
                       'c-indentation-style))
        (with-eval-after-load "desktop"
          (add-to-list 'desktop-locals-to-save
                       'c-indentation-style))))))

(defun lotus-dev/post-init-disaster ()
  ;; ![Screenshot](http://i.imgur.com/kMoN1m6.png)
  ;;
  ;; Disaster lets you press `C-c C-d` to see the compiled assembly code for the
  ;; C/C++ file you're currently editing. It even jumps to and highlights the
  ;; line of assembly corresponding to the line beneath your cursor.
  ;;
  ;; It works by creating a `.o` file using make (if you have a Makefile) or the
  ;; default system compiler. It then runs that file through objdump to generate
  ;; the human-readable assembly.

  ;;; Installation:

  ;; Make sure to place `disaster.el` somewhere in the load-path and add the
  ;; following lines to your `.emacs` file to enable the `C-c C-d` shortcut to
  ;; invoke `disaster':
  ;;
  (use-package disaster
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/post-init-cc-mode ()
  (use-package cc-mode
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-dev/init-session ()
  (use-package session
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/post-init-desktop ()
  (use-package desktop
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-erefactor ()
  (use-package erefactor
    :defer t
    :config
    (progn
      ;; Put this file into load-path'ed directory,
      ;; and byte compile its if desired.
      ;; And put the following expression into your ~/.emacs.
      ;;
      ;;     (eval-after-load 'lisp-mode
      ;;       '(progn
      ;;          (require 'erefactor)
      ;;          (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map)))
      ;;
      ;; And set these variables correctly.
      ;;  `erefactor-lint-path-alist', `erefactor-lint-by-emacsen'

      ;; Put the following in your .emacs, if you desire highlighting local variable.
      ;;
      ;;     (add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
      ;;     (add-hook 'lisp-interaction-mode-hook 'erefactor-lazy-highlight-turn-on)

      ;; Usage:

      ;; C-c C-v l : elint current buffer in clean environment.
      ;; C-c C-v L : elint current buffer by multiple emacs binaries.
      ;;             See `erefactor-lint-emacsen'
      ;; C-c C-v r : Rename symbol in current buffer.
      ;;             Resolve `let' binding as long as i can.
      ;; C-c C-v R : Rename symbol in requiring modules and current buffer.
      ;; C-c C-v h : Highlight current symbol in this buffer
      ;;             and suppress `erefacthr-highlight-mode'.
      ;; C-c C-v d : Dehighlight all by above command.
      ;; C-c C-v c : Switch prefix bunch of symbols.
      ;;             ex: '(hoge-var hoge-func) -> '(foo-var foo-func)
      ;; C-c C-v ? : Display flymake elint warnings/errors

      ;; * To show compilation warnings when evaluate `defun' form.
      ;;
      ;;   M-x erefactor-check-eval-mode


      (when nil
        ;; code run by elint
        (progn
          (setq load-path
                (append load-path 'nil))
          (find-file "/home/s/hell/.xemacs/pkgrepos/mypkgs/session-start/office-config.el")
          (goto-char
           (point-min))
          (condition-case err
              (let
                  (sexp)
                (while t
                  (setq sexp
                        (read
                         (current-buffer)))
                  (cond
                   ((memq
                     (car-safe sexp)
                     'require)
                    (princ
                     (format "Evaluating %s... " sexp))
                    (eval sexp))
                   ((eq
                     (car-safe sexp)
                     'eval-when-compile)
                    (princ
                     (format "Evaluating %s... "
                             `(progn ,@(cdr-safe sexp))))
                    (eval
                     `(progn ,@(cdr-safe sexp)))))))
            (error nil))
          (macroexpand
           '(labels nil))
          (elint-initialize)
          (elint-current-buffer)
          (with-current-buffer "*Elint*"
            (princ
             (buffer-string))))))))

(defun lotus-dev/init-uncrustify-mode ()
  (use-package uncrustify-mode
    :defer t
    :config
    (progn
      (add-hook 'c-mode-common-hook
                '(lambda ()
                   (uncrustify-mode -1))))))


(defun lotus-dev/post-init-srefactor ()
  (use-package srefactor
      :defer t
      :config
      (progn
        )))

(defun lotus-dev/init-srefactor-lisp ()
  (use-package srefactor-lisp
    :defer t
    :config
    (progn
      ;; (setq srefactor--getter-prefix "get_"
      ;;       srefactor--setter-prefix "set_"
      ;;       srefactor--getter-setter-capitalize-p t)
      ;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++.
      (semantic-mode 1) ;; -> this is optional for Lisp
      )))

(defun lotus-dev/init-PACKAGE ()
  (use-package PACKAGE
      :defer t
      :config
      (progn
        (progn
          (setq
           srefactor--getter-prefix "get"
           srefactor--setter-prefix "set"
           srefactor--getter-setter-capitalize-p t)

          ;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++.
          (semantic-mode 1) ;; -> this is optional for Lisp
          ))))

(defun lotus-dev/post-init-python ()
  (use-package python
    :defer t
    :config
    (progn
      (unless (fboundp 'python-shell-virtualenv-root)
        (defvar python-shell-virtualenv-root nil)))))



;;; packages.el ends here
