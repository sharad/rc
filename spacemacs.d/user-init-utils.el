
(defun dotspacemacs/reinit ()
  (setq-default
   dotspacemacs-which-key-delay 3.0)    ;BUG not working
  )


(defun spacemacs-dist-layers-select ()
  '(
    go
    python
    d
    javascript
    markdown
    ;; ----------------------------------------------------------------
    ;; Example of useful layers you may want to use right away.
    ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
    ;; <M-m f e R> (Emacs style) to install them.
    ;; ----------------------------------------------------------------
    auto-completion
    better-defaults
    emacs-lisp
    helm
    git
    gnus
    markdown
    org
    (shell :variables
     shell-default-height 30
     shell-default-position 'bottom)
    spell-checking
    syntax-checking
    version-control
    company-mode
    company
    erlang
    elixir
    ;; osx
    html
    org
    colors
    editorconfig
    themes-megapack
    perspectives
    misc
    python
    yaml
    ;; .spacemacs-mycontribution
    basic-startup
    messaging
    ))

(defun lotus-dist-layers-select (&optional layer-dir)
  (let ((layer-dir "~/.spacemacs-mycontribs/+local-session/"))
    (when (file-directory-p layer-dir)
      (mapcar
       '(lambda (f)
         (intern f))
       (remove-if
        'file-directory-p
        (directory-files layer-dir nil "^lotus-[a-zA-Z]+"))))))


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
       '(lambda (symbol value)
         (when (boundp 'elscreen-map)
           (elscreen-set-prefix-key value))
         (custom-set-default symbol value))
       'elscreen-prefix-key "\C-z")
      (global-set-key [s-right] 'elscreen-next)
      (global-set-key [s-left]  'elscreen-previous)
      (global-set-key [H-right] 'elscreen-move-right)
      (global-set-key [H-left]  'elscreen-move-left)
      (global-set-key [M-H-right]    'elscreen-swap)
      ;; (global-set-key-if-unbind [H-down]  'elscreen-previous)
      ;;}}
      )))

(defun lotus-emacs-user-init-begin ()
  (message "loading lotus-emacs-user-init-begin begin")
  (push (concat "~/.osetup/info.d/common/elisp") load-path)
  (push (concat "~/.osetup/info.d/hosts/" (system-name) "/elisp") load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/utils/" load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/experimental" load-path)
  (push "~/.spacemacs-mycontribs/local" load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/testing" load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/session-start" load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/gnus-session-start" load-path)
  (push "~/.xemacs/pkgrepos/world/misc/misc" load-path)
  (push "~/.xemacs/pkgrepos/autoinstalled/auto-install" load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/pa-planner" load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/planner-utils" load-path)

  (push
   '("local" . "~/.xemacs/elpa/upload")
   package-archives)

  (require 'basic-utils)

  (defvar *emacs-in-init* t "Emacs is in init.")
  (defvar user-emacs-directory "~/.emacs.d")
  (defvar reloading-libraries nil "used in session-conf.el")
  (setq user-emacs-directory "~/.emacs.d")
  (setq *emacs-in-init* t)
  (add-hook 'after-init-hook
            (lambda ()
              (setq *emacs-in-init* nil)
              (ad-disable-advice 'server-create-window-system-frame 'around 'nocreate-in-init)))
  (when (or t (require 'subr nil t))
    (defvar old-messages-buffer-max-lines 100 "To keep all startup detail.")
    (setq
     old-messages-buffer-max-lines messages-buffer-max-lines
     messages-buffer-max-lines 2000))


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
        (load-file custom-override-file)))
    ))


  (progn ;;  server

    (defun resolveip (host)
      (= 0 (call-process "~/bin/resolveip" nil nil nil host)))

    (defun host-accessable-p (&optional host)
      (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                         (if host host "www.google.com"))))

    (defun host-resolvable-accessible-p (host)
      (if (resolveip host)
          (host-accessable-p host)))

    (setq
     ;; server-auth-dir (auto-config-dir "server" t)
     server-use-tcp t
     server-name (or (getenv "EMACS_SERVER_NAME") server-name)
     server-host "0.0.0.0";; (if (host-resolvable-accessible-p (system-name)) (system-name) "localhost")
     )

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
        (message (concat "YES SERVER: " server-name))))
    )


  (lotus-necessary-test)

  (message "loading lotus-emacs-user-init-begin finished"))

(defun lotus-emacs-user-init-finish ()
  (message "loading lotus-emacs-user-init-finish begin")
  (dotspacemacs/reinit)
  (when nil
    (put-file-in-rcs (auto-config-file "startup/startup.log"))
    (with-current-buffer "*Messages*"
      (setq messages-buffer-max-lines 2000
            ;; old-messages-buffer-max-lines
            )
      ;; (append-to-buffer "*xxemacs-startup-log*" (point-min) (point-max))
      (copy-to-buffer "*emacs-startup-log*" (point-min) (point-max)))

    ;; (with-current-buffer "*emacs-startup-log*"
    ;;   ;; (with-temp-file file body)
    ;;   (set-buffer-file-coding-system
    ;;    (if (coding-system-p 'utf-8-emacs)
    ;;        'utf-8-emacs
    ;;        'emacs-mule))
    ;;   (write-region (point-min) (point-max) "~/.emacs.d/startup.log" t)
    ;;   (put-file-in-rcs "~/.emacs.d/startup.log"))
    )


  ;; (redefine-function-remembered 'server-create-window-system-frame)
  (setq *emacs-in-init* nil)              ;how to ensure it will run.

  (add-hook
   'lotus-enable-startup-interrupting-feature-hook
   'lotus-necessary-functionality
   t)

  (ad-disable-advice 'server-create-window-system-frame 'around 'nocreate-in-init)
  (lotus-necessary-functionality)
  (lotus-necessary-test)
  ;; limiting gnus messages
  (setq gnus-verbose 1)
  (message "loading lotus-emacs-user-init-finish finished"))

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

      (setq
       x-select-enable-primary t)

      ;; do not want it.
      ;; (setq yas/trigger-key "")
      )

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
          ad-do-it))

      )))


  (deh-require-maybe folding
    (defun toggle-hiding (column)
      (interactive "P")
      (if hs-minor-mode
          (if (condition-case nil
                  (hs-toggle-hiding)
                (error t))
              ;; (hs-show-all)
              (hs-show-block))
        (toggle-selective-display column)))  ;; set-selective-display is a simple, universal function which hides
    ;; code according to its indentation level. It can be used as a
    ;; fall-back for hs-toggle-hiding.

    ;; First, define a toggling function based on set-selective-display:

    (defun toggle-selective-display (column)
      (interactive "P")
      (set-selective-display
       (or column
           (unless selective-display
             (1+ (current-column))))))

    ;; The above is based on jao’s quick and dirty code folding code. The
    ;; hiding level can be passed as an prefix argument, or is based on
    ;; the horizontal position of point. Calling the function again brings
    ;; the code back.

    ;; Now, define another function which calls hs-toggle-hiding if it’s
    ;; available, or else falls back on toggle-selective-display:

    (defun toggle-hiding (column)
      (interactive "P")
      (if hs-minor-mode
          (if (condition-case nil
                  (hs-toggle-hiding)
                (error t))
              ;; (hs-show-all)
              (hs-show-block))
        (toggle-selective-display column)))

    (global-set-key-if-unbind (kbd "C-+") 'toggle-hiding)
    (global-set-key-if-unbind (kbd "C-=") 'toggle-selective-display))

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
    (defun mycustom-face-set ()
      "thisandthat."
      (interactive)
      (when (and
             (fboundp 'any-frame-opened-p)
             (any-frame-opened-p))
        (ignore-errors
          (spacemacs/set-default-font dotspacemacs-default-font)
          (set-face-attribute 'default nil ;(/ (* (x-display-mm-width) 121) 600)
                              :height (maxmin-optimized-value (x-display-mm-height) 110 600 120 90)
                              :width  'normal))))

    ;; http://emacs.stackexchange.com/questions/19096/how-do-i-turn-off-spacemacs-s-tildes-on-empty-lines
    (when (fboundp 'spacemacs/toggle-vi-tilde-fringe-off)
      (spacemacs/toggle-vi-tilde-fringe-off))

    (delete-selection-mode 1)

      (mycustom-face-set))

  (progn ;; other
    ;; (custom-available-themes)
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
  (message "loading lotus-necessary-functionality finished"))

(progn                                  ;debug testing code
  (defvar *test-idle-prints-timer* nil)
  (defun test-idle-prints (print)
    (if print
        (progn
          (defvar known-last-input-event nil)
          (if *test-idle-prints-timer* (cancel-timer *test-idle-prints-timer*))
          (when t
            (setq
             *test-idle-prints-timer*
             (run-with-timer 1 2
                             '(lambda ()
                               ;; (message "Test: From timer idle for org %d secs emacs %d secs" (org-emacs-idle-seconds) (float-time (current-idle-time)))
                               (let* (display-last-input-event
                                      (idle (current-idle-time))
                                      (idle (if idle (float-time (current-idle-time)) 0)))
                                 (unless (eq known-last-input-event last-input-event)
                                   (setq display-last-input-event last-input-event
                                         known-last-input-event last-input-event))
                                 (message "Test: From timer idle for %f secs emacs, and last even is %s" idle display-last-input-event)))))))
        (when *test-idle-prints-timer*
          (cancel-timer *test-idle-prints-timer*))))
  (defun toggle-test-idle-prints ()
    (interactive)
    (test-idle-prints (null *test-idle-prints-timer*)))

  (defun lotus-necessary-test ()
    (interactive)
    (test-idle-prints nil)))
