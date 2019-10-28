
(defun dotspacemacs/reinit ()
  (setq-default
   dotspacemacs-which-key-delay 3.0)    ;BUG not working
  )

;; bibtex -- org-ref is not available
;; ;; .spacemacs-mycontribution
;; basic-startup
;; messaging


(defun spacemacs-dist-layers-select ()
  '(
    ;; ----------------------------------------------------------------
    ;; Example of useful layers you may want to use right away.
    ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
    ;; <M-m f e R> (Emacs style) to install them.
    ;; ----------------------------------------------------------------

    ;; /home/s/hell/.emacs.d/layers/auto-layer.el
    ;; /home/s/hell/.emacs.d/layers/LAYERS.org

    ;; /home/s/hell/.emacs.d/layers/+chat:
    erc
    ;; jabber
    rcirc
    slack

    ;; /home/s/hell/.emacs.d/layers/+checkers:
    spell-checking
    syntax-checking

    ;; /home/s/hell/.emacs.d/layers/+completion:
    auto-completion
    helm
    ;; ivy
    templates

    ;; /home/s/hell/.emacs.d/layers/+distributions:
    spacemacs
    spacemacs-base
    spacemacs-bootstrap
    spacemacs-docker

    ;; /home/s/hell/.emacs.d/layers/+emacs:
    better-defaults
    ibuffer
    org
    semantic
    smex
    typography

    ;; /home/s/hell/.emacs.d/layers/+email:
    gnus
    mu4e
    notmuch

    ;; /home/s/hell/.emacs.d/layers/+filetree:
    neotree
    treemacs

    ;; /home/s/hell/.emacs.d/layers/+fonts:
    unicode-fonts

    ;; /home/s/hell/.emacs.d/layers/+frameworks:
    django
    emberjs
    phoenix
    react
    ruby-on-rails

    ;; /home/s/hell/.emacs.d/layers/+fun:
    emoji
    ;; games
    ;; selectric
    xkcd

    ;; /home/s/hell/.emacs.d/layers/+intl:
    ;; chinese
    ;; japanese
    ;; keyboard-layout

    ;; /home/s/hell/.emacs.d/layers/+lang:
    agda
    asciidoc
    asm
    autohotkey
    bibtex
    c-c++
    clojure
    coffeescript
    common-lisp
    coq
    crystal
    csharp
    csv
    d
    elixir
    elm
    emacs-lisp
    erlang
    ess
    factor
    faust
    forth
    fsharp
    go
    gpu
    graphviz
    groovy
    haskell
    html
    hy
    idris
    ipython-notebook
    java
    javascript
    jr
    json
    jsonnet
    julia
    kotlin
    latex
    lua
    major-modes
    markdown
    nim
    ocaml
    octave
    perl5
    perl6
    php
    plantuml
    protobuf
    purescript
    python
    racket
    restructuredtext
    ruby
    rust
    scala
    scheme
    semantic-web
    shell-scripts
    sml
    sql
    swift
    typescript
    vimscript
    windows-scripts
    yaml

    ;; /home/s/hell/.emacs.d/layers/+misc:
    copy-as-format
    nlinum
    parinfer

    ;; /home/s/hell/.emacs.d/layers/+music:
    spotify

    ;; /home/s/hell/.emacs.d/layers/+os:
    nixos
    ;; osx

    ;; /home/s/hell/.emacs.d/layers/+pair-programming:
    floobits

    ;; /home/s/hell/.emacs.d/layers/+readers:
    dash
    deft
    elfeed
    epub
    pdf
    speed-reading

    ;; /home/s/hell/.emacs.d/layers/+source-control:
    git
    github
    ;; perforce
    version-control

    ;; /home/s/hell/.emacs.d/layers/+spacemacs:
    spacemacs-completion
    spacemacs-defaults
    spacemacs-editing
    spacemacs-editing-visual
    spacemacs-evil
    spacemacs-language
    spacemacs-layouts
    spacemacs-misc
    spacemacs-modeline
    spacemacs-navigation
    spacemacs-org
    spacemacs-project
    spacemacs-purpose
    spacemacs-visual

    ;; /home/s/hell/.emacs.d/layers/+tags:
    cscope
    gtags

    ;; /home/s/hell/.emacs.d/layers/+themes:
    colors
    themes-megapack
    theming

    ;; /home/s/hell/.emacs.d/layers/+tools:
    ansible
    bm
    cfengine
    chrome
    cmake
    command-log
    debug
    docker
    fasd
    finance
    geolocation
    imenu-list
    lsp
    nginx
    node
    pandoc
    pass
    prettier
    prodigy
    puppet
    ranger
    ;; rebox ;; --- startup errors , (error "Style 371 defined more than once") signal(error ("Style 371 defined more than once"))
    restclient
    salt
    (shell :variables
     shell-default-height 30
     shell-default-position 'bottom)
    sphinx
    systemd
    tern
    terraform
    tmux
    transmission
    vagrant
    web-beautify
    xclipboard
    ycmd

    ;; /home/s/hell/.emacs.d/layers/+vim:
    ;; evil-commentary
    ;; evil-snipe
    ;; vim-empty-lines
    ;; vinegar

    ;; /home/s/hell/.emacs.d/layers/+web-services:
    confluence
    ;; evernote
    search-engine
    twitter))
;; wakatime

(defun spacemacs-dist-layers-exclude ()
  '(
    jabber
    ivy
    games
    selectric
    chinese
    japanese
    keyboard-layout
    osx
    perforce
    rebox ;; --- startup errors , (error "Style 371 defined more than once") signal(error ("Style 371 defined more than once"))
    shell
    evil-commentary
    evil-snipe
    vim-empty-lines
    vinegar
    wakatime
    window-purpose
    ))

(defun spacemacs-dist-layers-include ()
  '(
    (shell :variables
     shell-default-height 30
     shell-default-position 'bottom)
    ))


(defun lotus-dist-layers-group-dirs (&optional layers-group-top-dir)
  (let ((layers-group-top-dir (or layers-group-top-dir (expand-file-name "layers" spacemacs-start-directory))))
    (directory-files layers-group-top-dir t "^+.*")))

(defun lotus-dist-layers-select (layer-dir &optional match)
  (when (file-directory-p layer-dir)
    (mapcar
     #'(lambda (f)
         (intern f))
     (remove-if
      'file-directory-p
      (directory-files layer-dir nil match)))))

(defun lotus-dist-layers-group-dirs-layers-select (&optional layers-group-top-dir match)
  (let ((layers-group-top-dir (or layers-group-top-dir (expand-file-name "layers" spacemacs-start-directory))))
    (apply #'append
           (mapcar
            #'(lambda (path)
                (lotus-dist-layers-select path match))
            (lotus-dist-layers-group-dirs layers-group-top-dir)))))

;; (require 'cl-seq)

(defun lotus-layers-list ()
  (let* ((all-layers
          (append
           (lotus-dist-layers-group-dirs-layers-select (expand-file-name "layers" spacemacs-start-directory))
           (lotus-dist-layers-group-dirs-layers-select "~/.spacemacs-mycontribs/" "^lotus-[a-zA-Z]+")))
         (all-without-excluded-layers
          (set-difference
           all-layers (spacemacs-dist-layers-exclude)))
         (all-with-included-layers
          (append all-without-excluded-layers (spacemacs-dist-layers-include))))
    all-with-included-layers))


(defun lotus-startup-lists ()
  '((recents . 5)
    (projects . 3)
    (bookmarks . 10)
    (agenda . 7)
    (todos . 7)))


(defun lotus-themes ()
  '(ujelly
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
    ))


(defun lotus-disable-report-org ()
  (if (and
       ;; (not (buffer-file-name))
       (string-equal (file-truename "~/../paradise/")
                     (file-truename default-directory))
       (or
        (string= (buffer-name) "report.org")
        (string= (buffer-name) "report.org<2>")))
      (progn
        (message "creating report.org<2> buffer not going to debug.")
        (backtrace)
        (message "backtrace creating report.org<2> buffer not going to debug.")
        (debug))))

(defun lotus-debug-emacs-user-init-begin ()
  (add-hook 'fundamental-mode-hook
            #'lotus-disable-report-org)
  (add-hook 'org-mode-hook
            #'lotus-disable-report-org))


(defun cleanup-tty-process ()
  (interactive)
  (let ((tty-processes
         (remove-if-not
          'process-tty-name
          (process-list))))
    (dolist (tp tty-processes)
      (kill-process tp))))

(defun elscreen-keymap-setup ()
  (progn ;; "Keybinding: Elscreen"
    (when (featurep 'elscreen)
      ;;{{ elscreen
      ;; https://github.com/syl20bnr/spacemacs/issues/7372
      (define-key evil-emacs-state-map (kbd "C-z") nil)
      (global-unset-key [C-z])
      ;; (global-set-key [C-z c] 'elscreen-create)
      (funcall
       #'(lambda (symbol value)
           (when (boundp 'elscreen-map)
             (elscreen-set-prefix-key value))
           (custom-set-default symbol value))
       'elscreen-prefix-key "\C-z")
      (global-set-key [s-right] 'elscreen-next)
      (global-set-key [s-left]  'elscreen-previous)
      (global-set-key [H-right] 'elscreen-move-right)
      (global-set-key [H-left]  'elscreen-move-left)
      (global-set-key [M-H-right]    'elscreen-swap))))
      ;; (global-set-key-if-unbind [H-down]  'elscreen-previous)
      ;;}}

(defun lotus-emacs-user-init-begin ()
  (message "loading lotus-emacs-user-init-begin begin")
  ;; (debug-on-entry 'org-mode)
  (let ((osetup
         (expand-file-name
          ".repos/git/main/resource/userorg/main/readwrite/public/user/osetup" "~")))
    (push (expand-file-name "info.d/common/elisp" osetup) load-path)
    (let ((default-local-lib
            (expand-file-name "info.d/hosts/default/elisp" osetup))
          (local-lib
           (expand-file-name (concat "info.d/hosts/" (system-name) "/elisp") osetup)))
      (push
       (if (file-directory-p local-lib)
           local-lib
         default-local-lib)
       load-path)))

  (when nil
    (push "~/.xemacs/pkgrepos/mypkgs/utils/" load-path)
    ;; remove this
    (push "~/.xemacs/pkgrepos/mypkgs/experimental" load-path)
    (push "~/.spacemacs-mycontribs/local" load-path)
    (push "~/.xemacs/pkgrepos/mypkgs/testing" load-path)
    ;; remove this
    (push "~/.xemacs/pkgrepos/mypkgs/session-start" load-path)
    ;; remove this
    (push "~/.xemacs/pkgrepos/mypkgs/gnus-session-start" load-path)
    (push "~/.xemacs/pkgrepos/world/misc/misc" load-path)
    (push "~/.xemacs/pkgrepos/autoinstalled/auto-install" load-path)
    (push "~/.xemacs/pkgrepos/mypkgs/pa-planner" load-path)
    (push "~/.xemacs/pkgrepos/mypkgs/planner-utils" load-path))

  ;; Old order
  (when nil
    (push "~/.xemacs/pkgrepos/mypkgs/utils/" load-path)
    ;; remove this
    (push "~/.xemacs/pkgrepos/mypkgs/experimental" load-path)
    ;; (push "~/.spacemacs-mycontribs/local" load-path)
    (push "~/.xemacs/pkgrepos/mypkgs/testing" load-path)
    ;; remove this
    ;; (push "~/.xemacs/pkgrepos/mypkgs/session-start" load-path)
    ;; remove this
    (push "~/.xemacs/pkgrepos/mypkgs/gnus-session-start" load-path)
    (push "~/.xemacs/pkgrepos/world/misc/misc" load-path)
    (push "~/.xemacs/pkgrepos/autoinstalled/auto-install" load-path))
  ;; (push "~/.xemacs/pkgrepos/mypkgs/pa-planner" load-path)
  ;; (push "~/.xemacs/pkgrepos/mypkgs/planner-utils" load-path)


  ;; (push
  ;;  '("local" . "~/.xemacs/elpa/upload")
  ;;  package-archives)

  ;; (require 'lotus-utils)


  ;; New order
  ;; (add-to-list 'load-path "~/.xemacs/pkgrepos/spacemacs/misc/world/uncatagegoriged")

  (progn
    (unless (assoc "gnu" configuration-layer-elpa-archives)
      (add-to-list 'configuration-layer-elpa-archives '("gnu" . "https://elpa.gnu.org/packages/")))
    (unless (assoc "marmalade" configuration-layer-elpa-archives)
      (add-to-list 'configuration-layer-elpa-archives '("marmalade" . "https://marmalade-repo.org/packages/")))
    (unless (assoc "ELPA" configuration-layer-elpa-archives)
      (add-to-list 'configuration-layer-elpa-archives '("ELPA" . "https://tromey.com/elpa/")))
    (unless(assoc "melpa" configuration-layer-elpa-archives)
      (add-to-list 'configuration-layer-elpa-archives '("melpa" . "https://melpa.milkbox.net/packages/")))
    (unless(assoc "org" configuration-layer-elpa-archives)
      (add-to-list 'configuration-layer-elpa-archives '("org" . "https://orgmode.org/elpa/")))
    (unless(assoc "local" configuration-layer-elpa-archives)
      (add-to-list 'configuration-layer-elpa-archives '("local" . "~/.xemacs/elpa/upload"))))
  ;; (package-refresh-contents)

  (defvar *emacs-in-init* t "Emacs is in init.")
  (defvar user-emacs-directory spacemacs-start-directory)
  (defvar reloading-libraries nil "used in session-conf.el")
  (setq user-emacs-directory spacemacs-start-directory)
  (setq *emacs-in-init* t)
  (add-hook 'after-init-hook
            (lambda ()
              (setq *emacs-in-init* nil)
              (when (advice--p (advice--symbol-function 'server-create-window-system-frame))
                (ad-disable-advice 'server-create-window-system-frame 'around 'nocreate-in-init))))
  (when (or t (require 'subr nil t))
    (defvar old-messages-buffer-max-lines 100 "To keep all startup detail.")
    (setq
     old-messages-buffer-max-lines messages-buffer-max-lines
     messages-buffer-max-lines 2000))

  ;; BUG settle these
  ;; (require 'basic-utils)

  ;; (progn
  ;;    ;; server-auth-dir (auto-config-dir "server" t)
  ;;    (defadvice server-create-window-system-frame
  ;;        (around nocreate-in-init activate)
  ;;      "remove-scratch-buffer"
  ;;      (if *emacs-in-init*
  ;;          (message "loading init now.")
  ;;        ad-do-it)))

  (eval-after-load "server"
    '(progn
       ;; server-auth-dir (auto-config-dir "server" t)
       (defadvice server-create-window-system-frame
           (around nocreate-in-init activate)
         "remove-scratch-buffer"
         (if *emacs-in-init*
             (message "loading init now.")
           ad-do-it))))


  (when (require 'cl nil) ; a rare necessary use of REQUIRE
    ; http://a-nickels-worth.blogspot.in/2007/11/effective-emacs.html
    (defvar *emacs-load-start* (current-time)))

  (defconst *work-dir*
    (expand-file-name "paradise" "~/.."))

  (when t
    (progn ;; "custom setup"
      (defvar custom-override-file "~/.xemacs/hand-custom.el" "Hand Custom elisp")

      (defvar exclude-lib
        (if (string-equal (system-name) "spratap")
            '(tramp)))

      (when nil
        (when (file-exists-p (setq custom-file "~/.xemacs/custom.el"))
          (load-file custom-file))

        (when (file-exists-p custom-override-file)
          (load-file custom-override-file)))))


  ;;TODO
  ;; (require 'misc-utils)

  (progn ;;  server

    (defun resolveip (host)
      (= 0 (call-process "~/bin/resolveip" nil nil nil host)))

    (defun host-caccessable-p (&optional host)
      (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                         (if host host "www.google.com"))))

    (defun host-resolvable-accessible-p (host)
      (if (resolveip host)
          (host-accessable-p host)))

    (setq
     ;; server-auth-dir (auto-config-dir "server" t)
     server-use-tcp t
     server-name (or (getenv "EMACS_SERVER_NAME") server-name)
     server-host "0.0.0.0");; (if (host-resolvable-accessible-p (system-name)) (system-name) "localhost")


    (when nil
      (if (functionp 'server-running-p)
          (when (not (server-running-p))
            (condition-case e
                (server-start)
              ('error
               (progn
                 (message "Error: %s, now trying to run with tcp." e)
                 (let ((server-use-tcp nil))
                   (setq server-use-tcp nil)
                   (server-start))))))
        (message "server %s already running" server-name))
      (message (concat "SERVER: " server-name))
      (when (server-running-p (getenv "EMACS_SERVER_NAME"))
        (message (concat "YES SERVER: " server-name)))))


  (setq dotspacemacs-excluded-packages '(vi-tilde-fringe))

  (lotus-necessary-test)

  (spacemacs|use-package-add-hook org
    ;; https://github.com/syl20bnr/spacemacs/issues/8334#issuecomment-326200914
    :pre-init
    (package-initialize))

  (global-set-key (kbd "s-d") 'debug)

  (add-hook                             ;Cyclic symlink error solution
   'lotus-enable-startup-interrupting-feature-hook
   #'(lambda ()
       (progn
         (set-variable 'ycmd-server-command '("python" "/usr/bin/ycmd"))
         (set-variable 'ycmd-global-config  "~/.ycmd_global_conf.py")
         (set-variable 'ycmd-extra-conf-handler 'load)
         (set-variable 'ycmd-conf-whitelist (list "/home/s/paradise/releases/main/" (file-truename "/home/s/paradise/releases/main/")))))
   t)


  (setq kill-whole-line t)
  (delete-selection-mode 1)
  (message "loading lotus-emacs-user-init-begin finished"))

(defun lotus-emacs-user-init-finish ()
  (message "loading lotus-emacs-user-init-finish begin")
  (progn
    (unless (assoc "gnu" package-archives)
      (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
    (unless (assoc "marmalade" package-archives)
      (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/")))
    (unless (assoc "ELPA" package-archives)
      (add-to-list 'package-archives '("ELPA" . "https://tromey.com/elpa/")))
    (unless(assoc "melpa" package-archives)
      (add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/")))
    (unless(assoc "org" package-archives)
      (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/")))
    (unless(assoc "local" package-archives)
      (add-to-list 'package-archives '("local" . "~/.xemacs/elpa/upload"))))
  (package-initialize)
  (dotspacemacs/reinit)


  ;;   (font-family-list)
  ;; (setq
  ;;  dotspacemacs-default-font '("DejaVu Sans Mono:size=8:antialias=true"
  ;;                              :size 9
  ;;                              :weight normal
  ;;                              :width normal
  ;;                              :powerline-scale 0.8
  ;;                              :powerline-text-scale-factor 0.5
  ;;                              :powerline-default-separator 'curve))
  ;; (spacemacs/set-default-font dotspacemacs-default-font)

  (when nil
    (put-file-in-rcs (expand-file-name ".cache/startup/startup.log" user-emacs-directory))
    (with-current-buffer "*Messages*"
      (setq messages-buffer-max-lines 2000)
      ;; old-messages-buffer-max-lines

      ;; (append-to-buffer "*xxemacs-startup-log*" (point-min) (point-max))
      (copy-to-buffer "*emacs-startup-log*" (point-min) (point-max))))

  ;; (with-current-buffer "*emacs-startup-log*"
  ;;   ;; (with-temp-file file body)
  ;;   (set-buffer-file-coding-system
  ;;    (if (coding-system-p 'utf-8-emacs)
  ;;        'utf-8-emacs
  ;;        'emacs-mule))
  ;;   (write-region (point-min) (point-max) "~/.emacs.d/startup.log" t)
  ;;   (put-file-in-rcs "~/.emacs.d/startup.log"))



  ;; (redefine-function-remembered 'server-create-window-system-frame)
  (setq *emacs-in-init* nil)              ;how to ensure it will run.

  (add-hook
   'lotus-enable-startup-interrupting-feature-hook
   'lotus-necessary-functionality
   t)

  (add-hook
   'lotus-enable-startup-interrupting-feature-hook
   #'(lambda ()
       (with-temp-buffer
         (spacemacs/startup-hook))))
  ;; 'spacemacs-buffer/goto-buffer


  (when (advice--p (advice--symbol-function 'server-create-window-system-frame))
    (ad-disable-advice 'server-create-window-system-frame 'around 'nocreate-in-init))
  (lotus-necessary-functionality)
  (lotus-necessary-functionality-once-add-to-spacemacs-later)
  (lotus-necessary-test)
  ;; limiting gnus messages
  (setq gnus-verbose 1)
  (setq org-agenda-files nil)

  (progn ;; spacemacs
    (defun spacemacs/lazy-load-srefactor ()
      "Lazy load the package."
      (require 'srefactor)
      ;; currently, evil-mode overrides key mapping of srefactor menu
      ;; must expplicity enable evil-emacs-state. This is ok since
      ;; srefactor supports j,k,/ and ? commands when Evil is
      ;; available
      (add-hook 'srefactor-ui-menu-mode-hook 'evil-emacs-state))

    (defun spacemacs/lazy-load-stickyfunc-enhance ()
      "Lazy load the package."
      (require 'stickyfunc-enhance)))

  (lotus-debug-emacs-user-init-begin)


  (message "loading lotus-emacs-user-init-finish finished"))



(defun lotus-unnecessary-functionality ()
  (interactive)
  (message "loading lotus-unnecessary-functionality begin")

  ;; (debug-on-entry 'org-clock-load)

  (message "loading lotus-unnecessary-functionality finished"))

(defun lotus-necessary-functionality ()
  (interactive)
  (message "loading lotus-necessary-functionality begin")
  (progn ;; expand
    (progn ;; yasnippet
      ;; inplace of tab I want it to use C->
      (setq yas/trigger-key "C->")

      ;; ;; pabbrev-expand-maybe
      ;; ;; (pabbrev-get-previous-binding)

      (defun yas--keybinding-beyond-yasnippet-advice (orig-fun &rest args)
        ;; (let ((binding (apply orig-fun args)))
        (let ((binding (apply orig-fun args)))
          (if (eq binding 'pabbrev-expand-maybe)
              (call-interactively 'indent-for-tab-command)
            binding)))

      (when (fboundp 'advice-add)
        (advice-add 'yas--keybinding-beyond-yasnippet
                    :around
                    #'yas--keybinding-beyond-yasnippet-advice))

      (when nil
        (advice-remove 'yas--keybinding-beyond-yasnippet
                       #'yas--keybinding-beyond-yasnippet-advice))

      ;; (setq-default yas-fallback-behavior '(apply indent-for-tab-command . nil))

      (setq-default yas-fallback-behavior 'call-other-command)

      (setq x-select-enable-primary t))

    ;; do not want it.
    ;; (setq yas/trigger-key "")


    (when nil                           ; FROM where this came.
      (let (current-load-list)

        (defadvice indent-region (around remove-useless-whitespace
                                         (start end &optional column) activate)
          "Advised by Develock.
If Develock is on, remove useless leading and trailing whitespace in
Lisp modes, C modes and Java mode.  You can turn off this advice
permanently by customizing the `develock-energize-functions-plist'
variable."
          (if (and develock-mode font-lock-mode
                   (plist-get develock-energize-functions-plist 'indent-region)
                   (memq major-mode '(emacs-lisp-mode
                                      lisp-interaction-mode
                                      c-mode c++-mode java-mode jde-mode)))
              (save-excursion
                ;; Meddle with out of the region.
                (goto-char end)
                (while (and (zerop (forward-line 1))
                            (looking-at "[\t ]+$")))
                (let ((to (point))
                      (fn (cdr (assq
                                major-mode
                                '((emacs-lisp-mode . develock-lisp-indent-line)
                                  (lisp-interaction-mode . develock-lisp-indent-line)
                                  (c-mode . develock-c-indent-line)
                                  (c++-mode . develock-c-indent-line)
                                  (java-mode . develock-c-indent-line)
                                  (jde-mode . develock-c-indent-line))))))
                  (goto-char start)
                  (while (and (zerop (forward-line -1))
                              (or (looking-at "[\t ]+$")
                                  (progn
                                    (forward-line 1)
                                    nil))))
                  (save-restriction
                    (if (prog1
                            (zerop (forward-line -1))
                          (narrow-to-region (point) to))
                        (forward-line 1))
                    (while (not (eobp))
                      (or (eolp)
                          (progn
                            (funcall fn)
                            (if (and (not (bolp))
                                     (eolp))
                                (delete-region (develock-point-at-bol) (point)))))
                      (forward-line 1)))))
            ad-do-it)))))




  ;; (deh-require-maybe folding
  ;;   (defun toggle-hiding (column)
  ;;     (interactive "P")
  ;;     (if hs-minor-mode
  ;;         (if (condition-case nil
  ;;                 (hs-toggle-hiding)
  ;;               (error t))
  ;;             ;; (hs-show-all)
  ;;             (hs-show-block))
  ;;       (toggle-selective-display column)))  ;; set-selective-display is a simple, universal function which hides
  ;;   ;; code according to its indentation level. It can be used as a
  ;;   ;; fall-back for hs-toggle-hiding.

  ;;   ;; First, define a toggling function based on set-selective-display:

  ;;   (defun toggle-selective-display (column)
  ;;     (interactive "P")
  ;;     (set-selective-display
  ;;      (or column
  ;;          (unless selective-display
  ;;            (1+ (current-column))))))

  ;;   ;; The above is based on jao’s quick and dirty code folding code. The
  ;;   ;; hiding level can be passed as an prefix argument, or is based on
  ;;   ;; the horizontal position of point. Calling the function again brings
  ;;   ;; the code back.

  ;;   ;; Now, define another function which calls hs-toggle-hiding if it’s
  ;;   ;; available, or else falls back on toggle-selective-display:

  ;;   (defun toggle-hiding (column)
  ;;     (interactive "P")
  ;;     (if hs-minor-mode
  ;;         (if (condition-case nil
  ;;                 (hs-toggle-hiding)
  ;;               (error t))
  ;;             ;; (hs-show-all)
  ;;             (hs-show-block))
  ;;       (toggle-selective-display column)))

  ;;   (global-set-key-if-unbind (kbd "C-+") 'toggle-hiding)
  ;;   (global-set-key-if-unbind (kbd "C-=") 'toggle-selective-display))


  ;; http://emacs.stackexchange.com/questions/19096/how-do-i-turn-off-spacemacs-s-tildes-on-empty-lines
  (when (fboundp 'spacemacs/toggle-vi-tilde-fringe-off)
    (spacemacs/toggle-vi-tilde-fringe-off))

  (when (fboundp 'global-vi-tilde-fringe-mode)
    (global-vi-tilde-fringe-mode -1))

  (delete-selection-mode 1)


  (progn ;;
    (defun maxmin-optimized-value (val scale div &optional max min)
      (let ((opt (/ (* val scale) div)))
        (if (and max
                 (> max 0)
                 (> opt max))
            max
          (if (and min
                   (> min 0)
                   (< opt min))
              min
            opt))))
    ;; set attributes

    (when (fboundp 'set-default-face-height-by-resolution)
      (defalias 'mycustom-face-set #'set-default-face-height-by-resolution))

    (mycustom-face-set))

  (progn ;; other
    ;; (custom-available-themes)

    ;; custom-enabled-themes
    ;; theme using (ujelly)

    ;; trying (arjen)

    (defun theme-current ()
      (interactive)
      (message "%s" custom-enabled-themes))
    (defun theme-current-insert ()
      (interactive)
      (insert (format "%s" custom-enabled-themes)))

    (global-hl-line-mode -1))

  (progn
    (global-set-key (kbd "M-SPC") 'just-one-space)
    (elscreen-keymap-setup))

  (epa-file-enable)

  (remove-hook
   'lotus-enable-startup-interrupting-feature-hook
   'lotus-necessary-functionality)
  ;; limiting gnus messages
  (setq gnus-verbose 1)
  (set-default-face-height-by-resolution)
  ;; (lotus-powerline-setup)

  (progn
    ;; BUG TODO INQ(inquery)
    ;; don't why it is not happening in spacemacs, while vanilla emacs has this
    ;; it can be found in bindings.el
    ;; why it was not loaded, can be load it in spacemacs.
    (define-key ctl-x-map "r" ctl-x-r-map))

  (progn
    ;; org babel pain
    ;; https://stackoverflow.com/questions/26290924/fontify-r-code-blocks-in-org-mode-8
    ;; https://emacs.stackexchange.com/questions/14824/org-block-background-font-not-having-effect
    ;; https://www.reddit.com/r/emacs/comments/415imd/prettier_orgmode_source_code_blocks/
    ;; https://github.com/jabranham/mixed-pitch/issues/2#issuecomment-349385937
    ;; (add-to-list 'org-src-block-faces
    ;;              '("prog"
    ;;                (:foreground "#FFFFFF")))
    (set-face-attribute 'org-block nil :inherit 'src-block))


  (with-eval-after-load 'psgml
    (message "psgml loaded, now loading sgml-mode.el library")
    (load-library "sgml-mode"))

  (setq warning-minimum-log-level :debug)

  (lotus-unnecessary-functionality)

  (message "loading lotus-necessary-functionality finished"))

(defun lotus-necessary-functionality-once-add-to-spacemacs-later ()

  (progn

    (defun add-to-debug-ignore-errors(errstr)
      (unless (member errstr debug-ignored-errors)
        (push errstr debug-ignored-errors)))
    (add-to-debug-ignore-errors "Nothing to complete")
    (add-to-debug-ignore-errors "Unmatched Text during Lexical Analysis")
    (add-to-debug-ignore-errors "‘global’ non-zero exit: global: GTAGS not found.")))


(progn                                  ;debug testing code
  (defvar *test-idle-prints-timer* nil)
  (defvar *test-idle-prints-report-min-time* 21)

  (setq *test-idle-prints-report-min-time* (* 5 60))
  (defun test-idle-prints (print)
    (if print
        (progn
          (defvar known-last-input-event nil)
          (if *test-idle-prints-timer* (cancel-timer *test-idle-prints-timer*))
          (when t
            (setq
             *test-idle-prints-timer*
             (run-with-timer 1 2
                             #'(lambda ()
                                 (let* (display-last-input-event
                                        (idle (current-idle-time))
                                        (idle (if idle (float-time (current-idle-time)) 0)))
                                   (unless (eq known-last-input-event last-input-event)
                                     (setq display-last-input-event last-input-event
                                           known-last-input-event last-input-event))
                                   (when (> idle *test-idle-prints-report-min-time*)
                                     (lwarn 'idle :debug "Test: From timer idle for %f secs emacs, and last even is %s"
                                            idle
                                            display-last-input-event))))))))
      (when *test-idle-prints-timer*
        (cancel-timer *test-idle-prints-timer*))))
  (defun toggle-test-idle-prints ()
    (interactive)
    (test-idle-prints (null *test-idle-prints-timer*)))

  (defun lotus-necessary-test ()
    (interactive)
    (test-idle-prints t)))



(defun serve-window-manager-request (&rest args)
  (message "%s" args)
  ;; (display-about-screen)
  )
