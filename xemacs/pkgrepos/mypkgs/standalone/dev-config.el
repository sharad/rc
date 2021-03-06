;;; dev-config.el --- dev config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad>
;; Keywords: internal, internal

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

(require 'init-config "~/.xemacs/init-config.el")


;; TODO: [[http://tuhdo.github.io/c-ide.html][C/C++ Development Environment for Emacs]]

(deh-require-maybe which-func

  (which-function-mode 1)

  (defface which-func
      '((((class color) (min-colors 88) (background dark)) (:foreground "Green")))
    "which-face")

  (defun copy-current-function ()
    (interactive)
    (let ((fun-name (which-function)))
      (if fun-name
          (kill-new fun-name)
          (message "Not able to get function.")))))

(deh-require-maybe member-functions
  ;; for C++ mode
  )

(deh-require-maybe devel-notes
  ;; http://www.emacswiki.org/emacs/DevelNotes
  (global-set-key "\C-cza" 'develnotes-add-annotation)
  (global-set-key "\C-czv" 'develnotes-visit-file)
  (global-set-key "\C-czt" 'develnotes-add-TODO)
  (global-set-key "\C-czf" 'develnotes-add-FIXME))

(deh-require-maybe sidebrain
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
  )


(deh-require-maybe subword
  ;; (global-subword-mode 1)
  (add-element-to-lists '(lambda nil (subword-mode 1)) pgm-langs))




(eval-after-load "simple-call-tree+"
  '(progn
    (defun left-word (&optional arg)
      (interactive)
      (backward-word arg))

    (defun right-word (&optional arg)
      (interactive)
      (forward-word arg))))

(eval-after-load "simple-call-tree"
    '(progn
      (defun left-word (&optional arg)
        (interactive)
        (backward-word arg))

      (defun right-word (&optional arg)
        (interactive)
        (forward-word arg))))


  (eval-after-load 'outline
    '(progn
      (require 'outline-magic)
      (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))


(deh-require-maybe (and simple-call-tree+ fm outline outline-magic)
  )


;; remove-useless-whitespace
(deh-require-maybe develock

  ;; develock-mode
                  ;for now
  (when (ad-find-advice 'indent-region 'around 'remove-useless-whitespace)
    (ad-remove-advice 'indent-region 'around 'remove-useless-whitespace)
    (ad-activate 'indent-region)
    (ad-update  'indent-region)))


(deh-section "gprof"
 (autoload 'gprof-mode "gprof" "Switch to the gprof major mode" t)
 (add-to-list 'auto-mode-alist '("\\.g?prof\\'" . gprof-mode)))

(require 'utils-config)

;; load cclookup when compile time
(eval-when-compile (deh-require-maybe 'cclookup))
(deh-require-maybe cclookup
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
    "Run cclookup-update and create the database at `cclookup-db-file'." t))

(deh-require-maybe ya-cppref
  ;; git@github.com:whitypig/ya-cppref.git
  (setq ya-cppref-path-to-doc-root "/usr/share/doc/c-cpp-reference/"
        ;; "/usr/local/doc/cpp/reference/"
        ))


(deh-require-maybe devhelp-index
  ;; https://github.com/martine/devhelp-index
  ;; TODO: Add completion
  ;; http://askubuntu.com/questions/287526/how-to-add-more-languages-docs-to-devhelp-java-etc
  ;; refrese devhelp
  (defun devhelp-make-index ()
    (interactive)
    "Read a search query from the minibuffer."
    (let ((dir (find-library-directory "devhelp-index")))
      (shell-command (concat dir "/devhelp-index.py")))))

(deh-require-maybe (and
                    semantic
                    semantic/find
                    semantic/ia
                    semantic/bovine/gcc
                    eassist
                    cedet-global
                    cedet)
  ;; http://www.emacswiki.org/emacs/eassist.el
  ;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
  ;; (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)
  (defun custom-semantic-hook ()
    (imenu-add-to-menubar "TAGS"))
  (add-hook 'semantic-init-hooks 'custom-semantic-hook)

  (setq semantic-symref-tool "global")

  ;; if you want to enable support for gnu global
  (when (cedet-gnu-global-version-check t)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode))

  ;; enable ctags for some languages:
  ;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
  (when (and
         (fboundp 'cedet-ectag-version-check)
         (cedet-ectag-version-check t))
    (semantic-load-enable-primary-exuberent-ctags-support))
  )




(deh-require-maybe (or gdb-ui gdb-mi)

  (deh-section "correction"


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

  (setq ;; gud-gdb-command-name (concat gud-gdb-command-name " -n -s SYMBOLFILE")
   gud-gdb-command-name "gdb --annotate=3 -n"
   gdb-many-windows t)


  (add-hook 'gdb-mode-hook '(lambda()
                             (setq comint-input-ring-file-name "~/.gdbhist")
                             (comint-read-input-ring t)
                             (setq comint-input-ring-size 1000)
                             (setq comint-input-ignoredups t)
                             (add-hook 'kill-buffer-hook 'comint-write-input-ring nil t)))



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
      (gud-refresh))))


;; (gud-query-cmdline 'gdb (format " --annotate=3 -n -s %s" default-directory))
;; (gud-val 'command-name 'gdb)
;; (gud-symbol 'command-name t 'gdb)
;; (gud-symbol 'history nil 'gdb)
;; (setq gud-gdb-history-old gud-gdb-history
;;       gud-gdb-history nil)
;; (setq gdb-output-sink 'user)


(deh-require-maybe (progn
                     guess-offset
                     guess-style)
  )


(deh-require-maybe guess-style
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
   'c-mode-common-hook 'guess-style-guess-all))


(deh-section "minimap"
  (autoload 'minimap-create "minimap")
  (setq minimap-window-location 'right))


;; enscript  -f Courier7  -E $f -p${dst}/${f}.ps
;; ps2pdf ${dst}/${f}.ps ${dst}/${f}.pdf

(defun find-alt-pdf-file (file)
  (interactive "ffile: ")
  (let ((filename (or file (buffer-file-name (current-buffer)))))
    (if filename
        (let ((ofile (expand-file-name (file-name-nondirectory filename) "/tmp/"))
              ofilemode
              (psfile (expand-file-name (concat (file-name-nondirectory filename) ".ps") "/tmp/"))
              (pdffile (expand-file-name (concat (file-name-nondirectory filename) ".pdf") "/tmp/")))
          (when (and (file-exists-p ofile)
                   (not (file-writable-p ofile)))
            (setq ofilemode (file-modes ofile))
            (set-file-modes ofile 666))
          (copy-file filename ofile 1)
          (if ofilemode
              (set-file-modes ofile ofilemode))
          (when (file-exists-p ofile)
            (shell-command-local-no-output (concat "enscript --color -f Courier7  -E " ofile " -p" psfile))
            (message (concat "enscript --color -f Courier7  -E " ofile " -p" psfile))
            (if (file-exists-p psfile)
                (progn
                  (shell-command-local-no-output (concat "ps2pdf " psfile " " pdffile))
                  (message (concat "ps2pdf " psfile " " pdffile))
                  (if (file-exists-p pdffile)
                      (find-file pdffile)
                      (message "Not able to create %s file." pdffile)))
                (message "Not able to create %s file." psfile)))
          (message "File %s done not exists" ofile)))))


(deh-section "C C++"
  ;; check out http://www.emacswiki.org/emacs/IndentingC
  (deh-require-maybe doxymacs
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
                (doxymacs-font-lock)))))


(deh-section "if0"
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


(deh-section "if0"
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


(deh-section "disassemble C/C++"
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
  (eval-after-load 'cc-mode
    '(progn
      (require 'disaster)
      (defun my-c-mode-common-hook ()
        (define-key c-mode-base-map (kbd "C-c C-d") 'disaster)
        (define-key c-mode-base-map (kbd "C-c C-c") 'compile))
      (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))))

(deh-require-maybe cc-cmds
  (require 'cc-cmds-modified))

(deh-require-maybe c-eldoc
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
  (add-hook 'c-mode-common-hook 'turn-on-eldoc-mode)

  (deh-section "celdoc"
    (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ -I/usr/include/")
    (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)))



(deh-require-maybe tramp
(when nil
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
    (compilation-minor-mode 1)))



  (remove-hook 'find-file-hooks 'tramp-minor-mode)
  (remove-hook 'dired-mode-hook 'tramp-minor-mode))


(deh-require-maybe compile
  (setq
   compilation-auto-jump-to-first-error t
   compilation-skip-threshold 2  ;; 2 - errors, 1 - warnings
   ))

(deh-featurep erefactor
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

  ;;; Usage:

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
         (buffer-string)))))

  )


(deh-section "editing case"
  ;; http://stackoverflow.com/questions/9288181/converting-from-camelcase-to-in-emacs

  (defun toggle-camelcase-underscores ()
    "Toggle between camelcase and underscore notation for the symbol at point."
    (interactive)
    (save-excursion
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds))
             (currently-using-underscores-p (progn (goto-char start)
                                                   (re-search-forward "_" end t))))
        (if currently-using-underscores-p
            (progn
              (upcase-initials-region start end)
              (replace-string "_" "" nil start end)
              (downcase-region start (1+ start)))
            (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
            (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

  (defun toggle-camelcase-underscores ()
    "Toggle between camelcase and underscore notation for the symbol at point."
    (interactive)
    (save-excursion
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds))
             (currently-using-underscores-p (progn (goto-char start)
                                                   (re-search-forward "_" end t))))
        (if currently-using-underscores-p
            (progn
              (upcase-initials-region start end)
              (replace-string "_" "" nil start end)
              (downcase-region start (1+ start)))
            (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
            (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))
  ;; (local-set-key "\M-\C-C"  'un-camelcase-word-at-point)
  )


(deh-require-maybe cc-vars
  (deh-require-maybe session
   (add-to-list 'session-locals-include
                'c-indentation-style))
  (deh-require-maybe desktop
   (add-to-list 'desktop-locals-to-save
                'c-indentation-style)))



(use-package uncrustify-mode
    :ensure t
    :config
    (when nil
     (add-hook 'c-mode-common-hook
              '(lambda ()
                (uncrustify-mode 1)))))


;; (defun generate-accessor ()
;;   (interactive)
;;   (let ((line ()))))

(deh-section "c/c++"
  (require 'srefactor)
  (require 'srefactor-lisp)

  (setq
   srefactor--getter-prefix "get"
   srefactor--setter-prefix "set"
   srefactor--getter-setter-capitalize-p t)

  ;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++.
  (semantic-mode 1) ;; -> this is optional for Lisp

  ;; Very helpful mode
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
  (global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
  (global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
  (global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer))

(use-package srefactor
    :ensure t
    :config
    (setq
     srefactor--getter-prefix "get"
     srefactor--setter-prefix "set"
     srefactor--getter-setter-capitalize-p t)

    ;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++.
    (semantic-mode 1) ;; -> this is optional for Lisp
    (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
    (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)

    (progn

      (global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
      (global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
      (global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
      (global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer))
  )

(provide 'dev-config)
;;; dev-config.el ends here
