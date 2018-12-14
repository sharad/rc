
(when (configuration-layer/package-usedp 'cperl-mode)
  (defun spacemacs/cperl-mode-enable ()
    (progn)) ;; "Keybinding: Elscreen"
      ;;{{ elscreen
      ;; (define-key evil-emacs-state-map (kbd "") nil)
      ;; (global-unset-key [C-z])
      ;; (global-set-key [] 'elscreen-create)


  (defun spacemacs/cperl-mode-disable ()
    (progn)) ;; "Keybinding: Elscreen"
      ;; (define-key evil-emacs-state-map nil)
      ;; (global-unset-key [])


  (spacemacs/cperl-mode-enable))



(progn
  '(


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

    (setq imenu-tree-auto-update t)
    (autoload 'tags-tree "tags-tree" "Show TAGS tree" t)

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

    (eval-after-load "ffap"
      '(add-to-list 'ffap-alist '(cperl-mode . pde-ffap-locate)))
    (add-hook 'cperl-mode-hook 'pde-perl-mode-hook)


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
            map)))

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
    ; manpage. The problem is no manpages available in Windows and it is too
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


    ;; Commands
    ;;     * C-c h help-dwim
    ;;     * pde-pod-to-manpage
    ;;     * perldoc-tree

    ;; END: Finding Documents






    ;; Another interesting major mode is cperl-mode. It's an alternative
    ;; to perl-mode and included in recent Emacs distributions. Here's how
    ;; to switch from perl-mode to cperl-mode:
    ;; Perl
    (add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
    (add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

    ;; {{ http://www.emacswiki.org/emacs/PerlDevelopEnvironment#toc3
    (deh-require-maybe pde-load
      ;; (global-set-key "\C-m" 'newline-and-indent)
      (global-set-key (kbd "RET") 'newline-and-indent)
      (global-set-key (kbd "C-j") 'newline)
      (global-set-key (kbd "M-'") 'just-one-space)
      (global-set-key (kbd "C-M-=") 'pde-indent-dwim)
      ;; nearest key to dabbrev-expand
      ;; (global-set-key (kbd "M-;") 'hippie-expand)
      ;; (global-set-key (kbd "C-;") 'comment-dwim)
      (global-set-key (kbd "C-cf") 'comint-dynamic-complete)
      (global-set-key (kbd "C-cs") 'compile-dwim-compile)
      (global-set-key (kbd "C-cr") 'compile-dwim-run)
      (global-set-key (kbd "C-ci") 'imenu)
      (global-set-key (kbd "C-cv") 'imenu-tree)
      (global-set-key (kbd "C-cj") 'ffap)
      (global-set-key (kbd "C-ch") 'help-dwim)
      ;; TODO
      ;; (global-set-key "\C-xan" 'tempo-forward-mark)
      ;; (global-set-key "\C-xap" 'tempo-backward-mark)
      ;; (global-set-key "\C-xam" 'tempo-complete-tag)
      (global-set-key " " 'tempo-x-space))))

    ;; }}
