;; ref: http://search.cpan.org/src/YEWENBIN/PDE-v0.2.13/lisp/doc/QuickStartEn.html
;; Configuration in .emacs2

;; Editing
;; cperl-mode provides an excellent environment for perl
;; programming. With a good style, the indentation is fine. I recommend
;; the setting in PBP.

;; If you want reindent the code, using M-x indent-region when region is
;; selected. I recommend using pde-indent-dwim, it saves you time to
;; markup region.

;; Comment or uncomment code using M-x comment-dwim. When region
;; selected, if there are line uncommented, then comment the region,
;; otherwise uncomment the region. If no region selected and have
;; non-whitspace characters in current line, it will add comment at the
;; end of the line. If in empty line or only whitespace in current line,
;; it will add new comment. If there is comment already, jump to the
;; position where comment starts. With C-u prefix, it can delete comment
;; in current line. So with this command, all jobs about comment are
;; done.

;; Most editor provide feature about code-snippet. In emacs, you can use
;; abbrev. First you should turn on abbrev-mode. cperl-mode provides some
;; useful abbrevs, for example after keywords if, else, elsif, while,
;; for, foreach, unless and until. Also for pod items =head11, =over,
;; =head2, =pod.

;; Emacs code completion is not as gaudy as other editors, but it
;; provides more ways and high customization for completion. The most
;; useful commands for completion are hippie-expand and
;; dabbrev-expand. They can do almost all kinds of completion. In my
;; experience, I use hippie-expand to complete whole line and
;; dabbrev-expand to complete words. Emacs doesn't provide semantic
;; analysis for perl, so there is no exists way for code intelligent
;; completion. Although hippie-expand provide completion for file name,
;; its priority is too low to reach. So you may need addtional command
;; for file name completion when editing. I recommend use
;; comint-dynamic-complete.

;; M-SPC not available, window manager take it away
;;(global-set-key (kbd "M-'") 'just-one-space)
(global-set-key-if-unbind (kbd "C-M-=") 'pde-indent-dwim)
;; nearest key to dabbrev-expand
;;(global-set-key-if-unbind (kbd "M-;") 'hippie-expand)
;;(global-set-key-if-unbind (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-c f") 'comint-dynamic-complete)

(setq hippie-expand-try-functions-list
          '(try-expand-line
            try-expand-dabbrev
            try-expand-line-all-buffers
            try-expand-list
            try-expand-list-all-buffers
            try-expand-dabbrev-visible
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name
            try-complete-file-name-partially
            try-complete-lisp-symbol
            try-complete-lisp-symbol-partially
            try-expand-whole-kill))
(autoload 'comint-dynamic-complete "comint" "Complete for file name" t)
(setq comint-completion-addsuffix '("/" . ""))
(setq-default indent-tabs-mode nil)

(defalias 'perl-mode 'cperl-mode)
(defun pde-perl-mode-hook ()
  (abbrev-mode t)
  (add-to-list 'cperl-style-alist
               '("PDE"
                 (cperl-auto-newline                         . t)
                 (cperl-brace-offset                         . 0)
                 (cperl-close-paren-offset                   . -4)
                 (cperl-continued-brace-offset               . 0)
                 (cperl-continued-statement-offset           . 4)
                 (cperl-extra-newline-before-brace           . nil)
                 (cperl-extra-newline-before-brace-multiline . nil)
                 (cperl-indent-level                         . 4)
                 (cperl-indent-parens-as-block               . t)
                 (cperl-label-offset                         . -4)
                 (cperl-merge-trailing-else                  . t)
                 (cperl-tab-always-indent                    . t)))
  (cperl-set-style "PDE"))


;; Command
;;     * C-M-\ indent-region
;;     * C-M-= pde-indent-dwim
;;     * C-; comment-dwim
;;     * M-/ dabbrev-expand
;;     * M-; hippie-expand
;;     * C-c f comint-dynamic-complete
;;     * M-' just-one-space
;;     * M-\ delete-horizontal-space
;;     * C-M-a beginning-of-defun
;;     * C-M-e end-of-defun
;;     * C-M-f forward-sexp
;;     * C-M-b backward-sexp
;;     * C-M-u backward-up-list
;;     * M-{ backward-paragraph
;;     * M-} forward-paragraph
;;     * M-h mark-paragraph
;;     * C-M-h mark-defun
;;     * C-x h mark-whole-buffer

;; END: Editing

;; Syntax Checking and Running
;; cperl-mode doesn't provide command for syntax checking and running. It
;; did put them on menu. Activating them requires mode-compile. But I
;; don't like mode-compile, it makes simple problem complicated. I like
;; smark-compile+. But customizing and extending smart-compile+ is not
;; convenient, so I rewrite the library to compile-dwim. You can use
;; compile-dwim-compile for syntax checking and compile-dwim-run to run
;; the script.

;; If you have experience of GUI programming, a problem may raise when
;; running several programs at the same time. compile asks you to kill
;; previous process before the new one starts. If you think this is not
;; convenient, you can set compilation-buffer-name-function to
;; pde-compilation-buffer-name.

;; Emacs provides flymake to do on-the-fly syntax checking. in my humble
;; opinion, it is not very useful. But you can turn it on if you want
;; emacs seems more like IDE.

;; For perl novice, it is common forgeting chmod before running perl
;; script. The library executable provide a solution for automatic chmod
;; when saving file.

(setq compilation-buffer-name-function 'pde-compilation-buffer-name)
(autoload 'compile-dwim-run "compile-dwim" "Build and run" t)
(autoload 'compile-dwim-compile "compile-dwim" "Compile or check syntax" t)
(autoload 'executable-chmod "executable"
          "Make sure the file is executable.")

(defun pde-perl-mode-hook ()
   ;; chmod when saving
  (when (and buffer-file-name
        (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
      (add-hook 'after-save-hook 'executable-chmod nil t))
  (set (make-local-variable 'compile-dwim-check-tools) nil))

;; Commands
;;     * C-c s compile-dwim-compile
;;     * C-c r compile-dwim-run
;;     * compile
;;     * flymake-mode
;;     * executable-set-magic


;; END: Syntax Checking and Running

;; Code Browsing
;; Emacs provides several tools for code browsing. Etags is first
;; choice. cperl-mode supports creating TAGS. M-x cperl-etags can create
;; TAGS for current file. More commands can be found in the menu <Perl>
;; <Tools> <Tags>. There is an additional command <Class Hierarchy from
;; TAGS> (M-x cperl-tags-hier-init). A menu will display when invoke it
;; (show as the figure). But it may complain for error if you set
;; tags-table-list. You can press "++UPDATE++" if you change TAGS files.

(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c v") 'imenu-tree)
(global-set-key (kbd "C-c j") 'ffap)
(setq tags-table-list '("./TAGS" "../TAGS" "../../TAGS"))
(autoload 'imenu-tree "imenu-tree" "Show imenu tree" t)
(setq imenu-tree-auto-update t)
(eval-after-load "imenu"
 '(defalias 'imenu--completion-buffer 'pde-ido-imenu-completion))
(autoload 'tags-tree "tags-tree" "Show TAGS tree" t)
;; A wonderful minibuffer completion mode

(if (functionp 'partial-completion-mode)
    (partial-completion-mode 1))
;; not available in this emacs

;; (setq PC-include-file-path
;;       (delete-dups (append PC-include-file-path pde-perl-inc)))
;; (setq ffap-url-regexp
;;       (concat
;;        "\\`\\("
;;        "news\\(post\\)?:\\|mailto:\\|file:" ; no host ok
;;        "\\|"
;;        "\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://" ; needs host
;;        "\\)[^:]" ; fix for perl module, require one more character that not ":"
;;        ))
;; (add-to-list 'ffap-alist  '(cperl-mode . pde-ffap-locate))

;; Rebinding keys for hideshow
( if (and
      (keymapp 'hs-minor-mode-map)
      (xrequire 'hideshow))
    (define-key hs-minor-mode-map "\C-c\C-o"
      (let ((map (lookup-key hs-minor-mode-map "\C-c@")))
        ;; C-h is help to remind me key binding
        (define-key map "\C-h" 'describe-prefix-bindings)
        (define-key map "\C-q" 'hs-toggle-hiding)
        ;; compatible with outline
        (define-key map "\C-c" 'hs-hide-block)
        (define-key map "\C-e" 'hs-show-block)
        map))
  )
;; Commands
;;     * C-c f ffap
;;     * C-c i imenu
;;     * C-c v imenu-tree
;;     * cperl-tags-hier-init
;;     * M-. find-tag
;;     * M-* pop-tag-mark

;; END: Code Browsing

;; Finding Documents

;; The unix way to view documents is to man. Emacs using woman (WithOut
;; MAN). You can use woman to view pod of module if there is a
;; manpage. The problem is no manpages available in Windows and it is too
;; long to wait woman cache updated. cperl-pod-to-manpage still uses
;; external man program to generate documents, and not so pretty as
;; woman. PDE provide a new command pde-pod-to-manpage, it using pod2man
;; to generate manpage and using woman to formated. With the same idea,
;; PDE provide perldoc to show pod using woman. Although you can invoke
;; perldoc directly, I recommend use it with help-dwim. help-dwim
;; integrates several document commands to one command, you can use it to
;; search elisp function, variable, manpage, perldoc, perlapi and so
;; on. perldoc-tree also provide tree view for modules. You can view
;; module pod or open the module file here.

(global-set-key (kbd "C-c h") 'help-dwim)
(setq cperl-lazy-help-time 2)
(defun pde-perl-mode-hook ()
   (cperl-lazy-install))

;; Commands
;;     * C-c h help-dwim
;;     * pde-pod-to-manpage
;;     * perldoc-tree

;; END: Finding Documents

;; Interactive shell

;; A striking feature of dynamic language is it provides eval
;; function. The eval function make it possible to build a huge project
;; like building blocks. `perl -de 1' is good candicate for interactive
;; shell. But PDE provides another interactive shell for easy
;; customization and extending, and echo result of eval.

(autoload 'run-perl "inf-perl" "Start perl interactive shell" t)

;; Commands
;;     * run-perl
;;     * inf-perl-send
;;     * inf-perl-send-line
;;     * inf-perl-send-region
;;     * inf-perl-load-file

;; END: Interactive shell

;; Debugger

;; gud of emacs supports perldb, but it is simple and crude compare with
;; gdb-ui. PDE provides a similar interface like gdb-ui. But it is known
;; not stable. You may have a try.

;; Perl Debugger

;; perl5db provides a convenient command line interface, and maybe it
;; more quick to use command than using emacs command. Remember perl5db
;; commands is worth if you want to debug perl script. Except n, s, c, b,
;; B, w, W, L, p, x these most used commands, these are also convenient:

;;     * f Switch to a loaded file
;;     * l Jump to line or function
;;     * . Back to current line
;;     * r Return from current subroutine
;;     * V Search for variable
;;     * S Search for function
;;     * y List lexcial variables

(autoload 'perldb-ui "perldb-ui" "perl debugger" t)

;; Commands
;;     * perldb-ui
;;     * perldb-many-windows
;;     * perldb-restore-windows



;; Another interesting major mode is cperl-mode. It's an alternative
;; to perl-mode and included in recent Emacs distributions. Here's how
;; to switch from perl-mode to cperl-mode:
;; Perl
(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;; {{ http://www.emacswiki.org/emacs/PerlDevelopEnvironment#toc3
(xrequire 'pde-load)
;; }}

(provide 'perl-config)
