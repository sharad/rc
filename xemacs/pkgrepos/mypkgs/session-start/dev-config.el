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

(deh-require-maybe 'member-functions
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

  ;; (add-hook 'sharad/enable-login-session-inperrupting-feature-hook
  ;;           #'(lambda ()
  ;;               (add-hook 'find-file-hook
  ;;                         #'sidebrain-read-todo-from-comments)) t)

  ;; (add-hook 'sharad/disable-login-session-inperrupting-feature-hook
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

(deh-require-maybe eassist
  ;; http://www.emacswiki.org/emacs/eassist.el
  )




(deh-require-maybe gdb-ui
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


(deh-section "cinfo"
  ;; http://tenkan.org/~tim/c-function-signature.html
  ;; make 'function-synopsis a new thing for thing-at-point
  (put 'function-synopsis 'beginning-op
       (lambda ()
         (if (bolp) (forward-line -1) (beginning-of-line))
         (skip-chars-forward "^{")
         (dotimes (i 3) (backward-sexp))))

  (put 'function-synopsis 'end-op
       (lambda () (skip-chars-forward "^{")))

  ;; override eldoc-mode's doc printer thingy
  (defadvice eldoc-print-current-symbol-info
      (around eldoc-show-c-tag activate)
    (if (eq major-mode 'c-mode)
        (show-tag-in-minibuffer)
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
                  (set-buffer (find-tag-noselect tag-regex nil t))
                  (let ((synopsis (or (thing-at-point 'function-synopsis)
                                      (thing-at-point 'line))))
                    (when synopsis
                      (eldoc-message "%s"
                                     (cleanup-function-synopsis synopsis)))))))))))

  (when nil
    (ad-disable-advice 'eldoc-print-current-symbol-info 'around 'eldoc-show-c-tag)
    (ad-remove-advice 'eldoc-print-current-symbol-info 'around 'eldoc-show-c-tag)
    (ad-update 'eldoc-print-current-symbol-info))

  ;; turn it on
  (add-hook 'c-mode-common-hook 'turn-on-eldoc-mode)

  (deh-section "celdoc"
    (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ -I/usr/include/")
    (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)))

(provide 'dev-config)
;;; dev-config.el ends here
