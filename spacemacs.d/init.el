;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs-mycontribs/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     ;; auto-completion
     ;; better-defaults
     emacs-lisp
     ;; git
     ;; markdown
     neotree)
   ;; org
   ;; (shell :variables
   ;;        shell-default-height 30
   ;;        shell-default-position 'bottom)
   ;; spell-checking
   ;; syntax-checking
   ;; version-control

   dotspacemacs-configuration-layers (lotus-layers-list)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(lotus-utils elscreen)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(vi-tilde-fringe)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7)
                                (bookmarks . 10)
                                (projects . 3)
                                (agenda . 7)
                                (todos . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;; dotspacemacs-themes '(spacemacs-dark
   ;;                       spacemacs-light)
   dotspacemacs-themes '(
                         ujelly
                         reverse
                         twilight
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn
                         arjen-grey
                         ;; arjen
                         )


   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10
                               :weight normal
                               :width normal)
   ;; dotspacemacs-default-font '("Source Code Pro"
   ;;                             :size 13
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.1)


   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (progn
    (load-file "~/.spacemacs.d/user-init-utils.el")
    (lotus-emacs-user-init-begin)))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (progn
    (push "~/.spacemacs-mycontribs/local" load-path)
    (lotus-emacs-user-init-finish)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(muse-blosxom-base-directory
   "/home/s/hell/Documents/CreatedContent/gen/virtual/muse/default/web/site/blog")
 '(muse-colors-autogen-headings (quote outline))
 '(muse-colors-inline-image-method (quote muse-colors-use-publishing-directory))
 '(muse-completing-read-function (quote ido-completing-read))
 '(muse-html-charset-default "utf-8")
 '(muse-html-encoding-default (quote utf-8))
 '(muse-html-footer "<lisp>(muse-insert-meta-file \"footer.html\")</lisp>")
 '(muse-html-header "<lisp>(muse-insert-meta-file \"header.html\")</lisp>")
 '(muse-html-meta-content-encoding (quote utf-8))
 '(muse-html-style-sheet
   "<lisp>
       (concat
        (muse-insert-css-link \"all\" \"common.css\")
        (muse-insert-css-link \"screen\" \"screen.css\")
        (muse-insert-css-link \"print\" \"print.css\"))
       </lisp>")
 '(muse-latex-header "<lisp>(muse-insert-meta-file \"header.tex\")</lisp>")
 '(muse-latex-pdf-browser "evince %s &")
 '(muse-mode-hook (quote (flyspell-mode footnote-mode)))
 '(muse-project-alist nil)
 '(muse-publish-comments-p t)
 '(muse-publish-date-format "%b. %e, %Y")
 '(muse-publish-desc-transforms
   (quote
    (muse-wiki-publish-pretty-title muse-wiki-publish-pretty-interwiki muse-publish-strip-URL)))
 '(muse-wiki-publish-small-title-words
   (quote
    ("the" "and" "at" "on" "of" "for" "in" "an" "a" "page")))
 '(muse-xhtml-footer "<lisp>(muse-insert-meta-file \"footer.html\")</lisp>")
 '(muse-xhtml-header "<lisp>(muse-insert-meta-file \"header.html\")</lisp>")
 '(muse-xhtml-style-sheet
   "<lisp>
       (concat
        (muse-insert-css-link \"all\" \"common.css\")
        (muse-insert-css-link \"screen\" \"screen.css\")
        (muse-insert-css-link \"print\" \"print.css\"))
       </lisp>")
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (org-projectile projectile-rails geeknote feature-mode ahk-mode helm-gtags helm-cscope ivy-xref counsel-gtags counsel-etags counsel function-args ggtags gxref lotus-utils xref-js2 sws-mode calfw-org calfw-cal reader-mode mindshare-contrib-mgr clipmon occ counsel-org-clock timesheet task-manager orgit org-present ob-elixir org-projectile org-plus-contrib stickyfunc-enhance yaoddmuse uncrustify-mode srefactor oddmuse lotus-wrapper timer-utils-lotus org-onchange publishing project-buffer-file org-onchnage org-misc-utils-lotus org-clock-wrapper org-clock-unnamed-task org-clock-in-if-not org-clock-hooks org-clock-experimental org-clock-check org-capture+ lotus-misc-utils rcs-backup org-doing htmlize zenburn-theme zen-and-art-theme yapfify yaml-mode xterm-color xquery-mode xcscope ws-butler winum white-sand-theme wgrep web-beautify wcheck-mode wakatime-mode w3m volatile-highlights vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org thinks test-session tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme startup-hooks spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slim-mode shell-pop seti-theme sessions-unified scss-mode sass-mode reverse-theme restart-emacs remember-unified remember-idle recentf-ext rebecca-theme rainbow-identifiers rainbow-delimiters railscasts-theme pytest pyenv-mode py-isort purple-haze-theme pug-mode project-root professional-theme popup-kill-ring pointback planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastebin paradox package-dev-utils-lotus pabbrev outline-magic origami organic-green-theme org-scahachua org-pomodoro org-mime org-download org-context-clock org-clock-utils-lotus org-clock-table-misc-lotus org-clock-resolve-advanced org-clock-daysummary org-bullets org-agenda-refiletarget-files-filters open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme next-package neotree naquadah-theme mwim mustang-theme muse multibackup multi-term moz move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimap minimal-theme material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lusty-explorer lush-theme lotus-crypt-utils lorem-ipsum livid-mode live-py-mode linum-relative light-soap-theme less-css-mode json-mode js2-refactor js-doc jbeans-theme jazz-theme jade-mode ivy-hydra ir-black-theme inkpot-theme indent-guide idomenu hy-mode hungry-delete highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-pydoc helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio go-guru go-eldoc gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ ghub gh-md gandalf-theme fuzzy frame-utils folding fm flyspell-correct-helm flymake-cursor flycheck-pos-tip flycheck-mix flycheck-credo flx-ido flatui-theme flatland-theme find-file-in-project fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help erlang eproject emmet-mode elisp-slime-nav ecb dumb-jump dracula-theme dpaste django-theme disaster diminish diff-hl deft define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme d-mode cython-mode cyberpunk-theme counsel-projectile corral company-web company-tern company-statistics company-go company-dcd company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode clues-theme clean-aindent-mode cherry-blossom-theme calfw c-eldoc busybee-theme buffer-utils bubbleberry-theme bm bitlbee birds-of-paradise-plus-theme badwolf-theme autoinsert+ auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes alchemist aggressive-indent afternoon-theme adaptive-wrap activity ace-window ace-link ace-jump-helm-line ac-ispell zoutline outorg navi-mode persp-mode-projectile-bridge poporg orgnav outshine worf @)))
 '(planner-xhtml-footer "<lisp>(muse-insert-meta-file \"footer.html\")</lisp>")
 '(planner-xhtml-header "<lisp>(muse-insert-meta-file \"header.html\")</lisp>")
 '(safe-local-variable-values
   (quote
    ((major-mode . sh)
     (ee-comment-prefix . ";;")
     (ee-anchor-format . "defun %s ")
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (modes emacs-lisp-mode fundamental-mode)
     (modes fundamental-mode emacs-lisp-mode escript-mode)
     (folded-file . t)
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking))))
 '(send-mail-function (quote sendmail-send-it))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-default-content ((t :foreground "#bfebbf")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
 '(cfw:face-grid ((t :foreground "DarkGrey")))
 '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-periods ((t :foreground "cyan")))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
 '(cfw:face-select ((t :background "#2f2f2f")))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
 '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
 '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
 '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold)))
 '(muse-bad-link ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(muse-blosxom-base-directory
   "/home/s/hell/Documents/CreatedContent/gen/virtual/muse/default/web/site/blog")
 '(muse-colors-autogen-headings (quote outline))
 '(muse-colors-inline-image-method (quote muse-colors-use-publishing-directory))
 '(muse-completing-read-function (quote ido-completing-read))
 '(muse-html-charset-default "utf-8")
 '(muse-html-encoding-default (quote utf-8))
 '(muse-html-footer "<lisp>(muse-insert-meta-file \"footer.html\")</lisp>")
 '(muse-html-header "<lisp>(muse-insert-meta-file \"header.html\")</lisp>")
 '(muse-html-meta-content-encoding (quote utf-8))
 '(muse-html-style-sheet
   "<lisp>
       (concat
        (muse-insert-css-link \"all\" \"common.css\")
        (muse-insert-css-link \"screen\" \"screen.css\")
        (muse-insert-css-link \"print\" \"print.css\"))
       </lisp>")
 '(muse-latex-header "<lisp>(muse-insert-meta-file \"header.tex\")</lisp>")
 '(muse-latex-pdf-browser "evince %s &")
 '(muse-mode-hook (quote (flyspell-mode footnote-mode)))
 '(muse-project-alist nil)
 '(muse-publish-comments-p t)
 '(muse-publish-date-format "%b. %e, %Y")
 '(muse-publish-desc-transforms
   (quote
    (muse-wiki-publish-pretty-title muse-wiki-publish-pretty-interwiki muse-publish-strip-URL)))
 '(muse-wiki-publish-small-title-words
   (quote
    ("the" "and" "at" "on" "of" "for" "in" "an" "a" "page")))
 '(muse-xhtml-footer "<lisp>(muse-insert-meta-file \"footer.html\")</lisp>")
 '(muse-xhtml-header "<lisp>(muse-insert-meta-file \"header.html\")</lisp>")
 '(muse-xhtml-style-sheet
   "<lisp>
       (concat
        (muse-insert-css-link \"all\" \"common.css\")
        (muse-insert-css-link \"screen\" \"screen.css\")
        (muse-insert-css-link \"print\" \"print.css\"))
       </lisp>")
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (lotus-helm lotus-finance annot+ annot ox-pandoc orgit org-present org-category-capture org-projectile org-brain ob-elixir ob-async elfeed-org org-password-manager org-plus-contrib lotus-tree-manager nil ertx annotate debug-info memory-usage org-noter lotus-schedule lotus-note lotus-annotate yas-suggest tagging planner-utils paths-mapper pa-planner lotus-notify lotus-basic-utils iswitchb-fc ffw ff-relativedir emacs-panel emacs-db-mode echoserver copywithoutsel lotus-idle-utils abyss-theme org-clock-split geeknote feature-mode ahk-mode helm-gtags ivy-xref counsel-gtags counsel-etags counsel function-args gxref lotus-utils xref-js2 sws-mode calfw-org calfw-cal reader-mode mindshare-contrib-mgr clipmon occ counsel-org-clock timesheet task-manager stickyfunc-enhance yaoddmuse uncrustify-mode srefactor oddmuse lotus-wrapper timer-utils-lotus org-onchange publishing project-buffer-file org-onchnage org-misc-utils-lotus org-clock-wrapper org-clock-unnamed-task org-clock-in-if-not org-clock-hooks org-clock-experimental org-clock-check org-capture+ lotus-misc-utils rcs-backup org-doing htmlize zen-and-art-theme yapfify yaml-mode xquery-mode xcscope ws-butler winum white-sand-theme wgrep web-beautify wcheck-mode wakatime-mode volatile-highlights vi-tilde-fringe uuidgen unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org thinks test-session tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme startup-hooks spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle slim-mode shell-pop sessions-unified scss-mode sass-mode reverse-theme restart-emacs remember-unified remember-idle recentf-ext rebecca-theme rainbow-identifiers rainbow-delimiters railscasts-theme pytest pyenv-mode py-isort purple-haze-theme pug-mode project-root professional-theme popup-kill-ring pointback planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pastebin package-dev-utils-lotus pabbrev outline-magic origami organic-green-theme org-scahachua org-pomodoro org-context-clock org-clock-utils-lotus org-clock-table-misc-lotus org-clock-resolve-advanced org-clock-daysummary org-bullets org-agenda-refiletarget-files-filters open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme next-package neotree mwim mustang-theme muse multibackup multi-term moz move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimap material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lusty-explorer lush-theme lotus-crypt-utils lorem-ipsum livid-mode linum-relative light-soap-theme less-css-mode js2-refactor js-doc jbeans-theme jazz-theme jade-mode ir-black-theme inkpot-theme indent-guide idomenu hungry-delete highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-pydoc helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruber-darker-theme grandshell-theme gotham-theme golden-ratio go-guru go-eldoc gnuplot gitconfig-mode gitattributes-mode git-messenger git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme fuzzy frame-utils folding fm flyspell-correct-helm flymake-cursor flycheck-pos-tip flycheck-mix flycheck-credo flx-ido flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery exotica-theme evil-visualstar evil-unimpaired evil-tutor evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu espresso-theme eshell-z eshell-prompt-extras esh-help eproject emmet-mode elisp-slime-nav ecb dracula-theme dpaste django-theme disaster diminish diff-hl deft define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme d-mode cython-mode cyberpunk-theme counsel-projectile corral company-web company-tern company-statistics company-go company-dcd company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-identifiers-mode coffee-mode clues-theme clean-aindent-mode cherry-blossom-theme calfw c-eldoc busybee-theme buffer-utils bubbleberry-theme bm bitlbee birds-of-paradise-plus-theme badwolf-theme autoinsert+ auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes alchemist aggressive-indent afternoon-theme adaptive-wrap activity ace-window ace-link ace-jump-helm-line ac-ispell outorg navi-mode persp-mode-projectile-bridge poporg orgnav @)))
 '(planner-xhtml-footer "<lisp>(muse-insert-meta-file \"footer.html\")</lisp>")
 '(planner-xhtml-header "<lisp>(muse-insert-meta-file \"header.html\")</lisp>")
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t)
 '(safe-local-variable-values
   (quote
    ((major-mode . nxml-mode)
     (ee-line-start . "/+_? *. ")
     (ee-comment-prefix . "/")
     (ee-anchor-format . "«%s»")
     (major-mode . emacs-lisp)
     (default-tab-width . 3)
     (major-mode . sh)
     (ee-comment-prefix . ";;")
     (ee-anchor-format . "defun %s ")
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (modes emacs-lisp-mode fundamental-mode)
     (modes fundamental-mode emacs-lisp-mode escript-mode)
     (folded-file . t)
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking))))
 '(send-mail-function (quote sendmail-send-it))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-default-content ((t :foreground "#bfebbf")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
 '(cfw:face-grid ((t :foreground "DarkGrey")))
 '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-periods ((t :foreground "cyan")))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
 '(cfw:face-select ((t :background "#2f2f2f")))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
 '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
 '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
 '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold)))
 '(muse-bad-link ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))
)
