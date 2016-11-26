
(defun sharad/emacs-user-init-begin ()
  (push (concat "~/.osetup/info.d/hosts/" (system-name) "/elisp") load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/utils/" load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/experimental" load-path)
  (push "~/.spacemacs-mycontribs/local" load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/testing" load-path)
  (push "~/.xemacs/pkgrepos/mypkgs/session-start" load-path)
  (push "~/.xemacs/pkgrepos/world/misc/misc" load-path)
  (push "~/.xemacs/pkgrepos/autoinstalled/auto-install" load-path)


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



  (progn ;; "custom setup"
    (defvar custom-override-file "~/.xemacs/hand-custom.el" "Hand Custom elisp")

    (when (file-exists-p (setq custom-file "~/.xemacs/custom.el"))
      (load-file custom-file))

    (when (file-exists-p custom-override-file)
      (load-file custom-override-file)))


  (progn ;;  server
    (setq
     ;; server-auth-dir (auto-config-dir "server" t)
     server-use-tcp t
     server-name (or (getenv "EMACS_SERVER_NAME") server-name))
    (setq server-host (system-name))

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
  )


(defun sharad/emacs-user-init-finish ()
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
   'sharad/enable-startup-interrupting-feature-hook
   'sharad/necessary-functionality
   )



  (ad-disable-advice 'server-create-window-system-frame 'around 'nocreate-in-init)
  (sharad/necessary-functionality)
  )



(defun sharad/necessary-functionality ()
  (interactive)

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

      ;; do not want it.
      ;; (setq yas/trigger-key "")
      )



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

      ))


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
      (set-face-attribute 'default nil ;(/ (* (x-display-mm-width) 121) 600)
                          :height (maxmin-optimized-value (x-display-mm-height) 110 600 120 75)
                          :width  'normal))

    ;; http://emacs.stackexchange.com/questions/19096/how-do-i-turn-off-spacemacs-s-tildes-on-empty-lines
    (when (fboundp 'spacemacs/toggle-vi-tilde-fringe-off)
     (spacemacs/toggle-vi-tilde-fringe-off))

    (delete-selection-mode 1)

    (when (any-frame-opened-p)
      (mycustom-face-set)))
  (remove-hook
   'sharad/enable-startup-interrupting-feature-hook
   'sharad/necessary-functionality)
  )
