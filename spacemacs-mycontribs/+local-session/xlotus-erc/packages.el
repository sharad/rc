;;; packages.el --- lotus-erc layer packages file for Spacemacs.
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
;; added to `lotus-erc-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-erc/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-erc/pre-init-PACKAGE' and/or
;;   `lotus-erc/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-ercS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-erc-packages
  '(
    (PACKAGE :location local)
    erc
    erc-join
    erc-services
    erc-nick-notify
    h4x0r
    bitlbee
    passwds
    )
  "The list of Lisp packages required by the lotus-erc layer.

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

(defun lotus-erc/init-erc ()
  (use-package erc
      :defer t
      :config
      (progn
        (progn
          ;; help
          ;; This is an example of how to make a new command.  Type "/uptime" to
          ;; use it.
          ;; (defun erc-cmd-UPTIME (&rest ignore)

          ;; (message "loading defun sharad/erc-start-or-switch")

          (defun sharad/erc-start-or-switch ()
            "Connect to ERC, or switch to last active buffer"
            (interactive)
            (if (or (get-buffer "localhost:6667")	;; ERC already active?
                    (get-buffer "irc.freenode.net:6667"))
                (erc-track-switch-buffer 1)  ;; yes: switch to last active
                (when (or t (y-or-n-p "Start ERC? ")) ;; no: maybe start ERC
                  (let ((logdir (concat (getenv "HOME") "/.logs/chat/erc")))
                    (unless (file-directory-p logdir)
                      (make-directory logdir t)))
                  (message "connecting bnc4free to connect freenode")
                  (condition-case e
                      (erc-bnc4free)
                    (error (message "Error: %s" e)))
                  ;; (erc-freenode)
                  (sleep-for 0 500)
                  (condition-case e
                      (erc-bitlbee)
                    (error (message "Error: %s" e)))
                                        ; can connect to multiple servers automatically
                                        ;(erc :server "irc.gimp.org" :port 6667 :nick "foo" :full-name "bar")
                  ;; http://
                  ;; (setq erc-log-insert-log-on-open t)
                  ))))

        (progn
          ;; useful stuff at http://www.emacswiki.org/emacs-en/AlexSchroederErcConfig
          ;; basic info at http://emacs-fu.blogspot.com/2009/06/erc-emacs-irc-client.html
          ;; and also http://nflath.com/2009/10/bitlbee-and-emacs/


          ;; registering
          ;; /msg NickServ register <password> <email>

          ;; user #help channel

          ;; join erc with this
          (defun erc-freenode ()
            (interactive)
            (erc :server "irc.freenode.net" :port 6667
                 :nick "sharad" :full-name "Sharad Pratap"
                 :password freenode-pass))
          )

        (progn
          ;; notify me of which erc buffers have been modified in the mode line
          (erc-track-mode 1)

          (setq erc-autojoin-channels-alist '(
                                              ;; ("freenode.net" ;"#bugfunk"
                                              ;;  "#emacs" "#lisp"
                                              ;;  "#python")
                                              ;; ("localhost" "#bitlbee")
                                              ("localhost" "&bitlbee"))
                erc-prompt-for-password nil
                erc-pals '("siprsuhane") ;'("pfeyz" "nenn" "echo" "SirCodesalot")
                erc-fools '()
                erc-hide-list '() ;'("JOIN" "PART" "QUIT")
                erc-track-exclude-types '("324" "353" "333"
                                          "JOIN" "NAMES" "NICK" "QUIT" "PART" "TOPIC")
                ;; logging! ... requires the `log' module
                ;; do it line-by-line instead of on quit
                erc-log-channels-directory (expand-file-name "erc/" "~/.logs/chat/")
                erc-save-buffer-on-part nil
                erc-save-queries-on-quit nil
                erc-log-write-after-send t
                erc-log-write-after-insert t
                erc-user-full-name "sharad"
                erc-email-userid "sharad@gmail.com"
                erc-nick "sharad"
                erc-nick-uniquifier "_"
                erc-modules '(netsplit fill track completion ring button autojoin
                              services match stamp track page log
                              scrolltobottom move-to-prompt irccontrols spelling))

          (setq erc-modules '(autoaway autojoin button
                              capab-identify
                              completion hecomplete dcc fill identd irccontrols
                              keep-place list log match menu move-to-prompt netsplit
                              networks noncommands notify page readonly replace ring
                              scrolltobottom services smiley sound stamp spelling track
                              truncate unmorse xdcc))


          ;; allow some channels to not auto-delay messages. This can
          ;; get you kicked from sane channels, so don't use it.
          (add-hook 'erc-mode-hook
                    (lambda ()
                      (let ((floodable-buffers
                             '(;; every channel in this list is floodable:
                               "#bugfunk"
                               )))
                        (when (member (buffer-name) floodable-buffers)
                          (make-local-variable 'erc-server-flood-penalty)
                          (setq erc-server-flood-penalty 0))))))

        (progn
          (add-hook 'erc-quit-hook
                    (lambda (process)
                      (message "%s" process)))

          (defun bwm-make-buffer-floodable ()
            (make-local-variable 'erc-server-flood-penalty)
            (setq erc-server-flood-penalty 0))

          ;; fancy prompt with channel name, or ERC if nil
          ;; http://www.emacswiki.org/emacs/ErcConfiguration#toc5
          (setq erc-prompt (lambda ()
                             (if (and (boundp 'erc-default-recipients)
                                      (erc-default-target))
                                 (erc-propertize (concat (erc-default-target) ">")
                                                 'read-only t
                                                 'rear-nonsticky t
                                                 'front-nonsticky t)
                                 (erc-propertize (concat "ERC>")
                                                 'read-only t
                                                 'rear-nonsticky t
                                                 'front-nonsticky t))))

          ;; Interpret mIRC-style color commands in IRC chats
          ;; seems to only work correctly when the irccontrols module is enabled
          (setq erc-interpret-mirc-color t
                erc-interpret-controls-p t))

        (progn
          (defun erc-notify-on-msg (msg)
            "Send a message via notify-send if a message specifically to me"
            (if (or (string-match "quodlibetor:" msg)
                    (string-match "Message from unknown handle" msg)
                    (and (string= "localhost" erc-session-server)
                         (not (string-match "\\*\\*\\*" msg))
                         (not (string-match "\<root\>" msg))))
                (let ((nameless-msg (replace-regexp-in-string "^\<.*?\> \(oscar - \)?" "" msg)))
                  (shell-command (concat "notify-send -t 1500 \"" (buffer-name) "\" \"" nameless-msg "\"")))))

          ;; not all messages
          ;; (add-hook 'erc-insert-pre-hook 'erc-notify-on-msg)

          (defun erc-ignore-unimportant (msg)
            "this is probably a really really horrible idea
for some reason bitlbee tells me that there is an Unkown error
while loading configuration every 60s, though"
            (when (or (string-match "Account already online" msg)
                      (string-match "Trying to get all accounts connected" msg)
                      (string-match "Unknown error while loading" msg)
                      (string-match "localhost has changed mode for" msg))
              (setq erc-insert-this nil)))))))

(defun lotus-erc/init-erc-join ()
  (use-package erc-join
      :defer t
      :config
      (progn
        (progn
          (erc-autojoin-mode t)))))

(defun lotus-erc/init-erc-services ()
  (use-package erc-services
      :defer t
      :config
      (progn
        )))

(defun lotus-erc/init-erc-nick-notify ()
  (use-package erc-nick-notify
      :defer t
      :config
      (progn
        )))

(defun lotus-erc/init-h4x0r ()
  (use-package h4x0r
      :defer t
      :config
      (progn
        )))

(defun lotus-erc/init-bitlbee ()
  (use-package bitlbee
      :defer t
      :config
      (progn
        (progn

          (defun erc-bitlbee ()
            (interactive)
            ;; (if (and (bitlbee-start)
            ;;          (bitlbee-running-p))
            ;;     (erc :server "localhost" :port "6667")
            ;;     (message "bitlbee is not running, install bitlbee and run it."))

            (if (shell-command-no-output "pgrep bitlbee") ;with bitlbee system-wide service.
                (erc :server "localhost" :port "6667")
                (message "bitlbee is not running, install bitlbee and run it."))
                                        ; (sleep-for 0 500)

            ;; (bitlbee-connect)
            ))

        (progn
          (defun sharad/erc-bnc4free-connect ()
            (interactive)
            (save-window-excursion
              (when (get-buffer "Grape.bnc4free.com:1337")
                (with-current-buffer "Grape.bnc4free.com:1337"
                  ;; (erc-message "PRIVMSG" (concat (erc-default-target) " quote PASS " (concat bnc4free-username ":" bnc4free-password)))
                  ;; (erc-send-input (concat "/quote PASS " bnc4free-username ":" bnc4free-password "\r\n"))
                  (insert (concat "/quote PASS " bnc4free-username ":" bnc4free-password ""))))))


          (defun erc-bnc4free ()
            (interactive)
            (erc :server "Grape.bnc4free.com" :port 1337
                 ;; :nick "sharad" :full-name "Sharad Pratap"
                 ;; :password bnc4free-password
                 )

            (sleep-for 0 500)
            (sharad/erc-bnc4free-connect)))

        (progn
          ;; (concat bnc4free-username ":" bnc4free-password)





          (defun erc-quit-bitlbee-maybe (process)
            (when (and (get-buffer-process bitlbee-buffer-name)
                       (equal (get-buffer-process bitlbee-buffer-name)
                              (get-process "bitlbee")))
              (bitlbee-stop)
              (kill-buffer bitlbee-buffer-name)))

          (add-hook 'erc-quit-hook
                    'erc-quit-bitlbee-maybe))

        (progn
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; bitlbee stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;http://nflath.com/tag/bitlbee/
          (defface erc-header-line-disconnected
              '((t (:foreground "black" :background "indianred")))
            "Face to use when ERC has been disconnected.")

          (defun erc-update-header-line-show-disconnected ()
            "Use a different face in the header-line when disconnected."
            (erc-with-server-buffer
              (cond ((erc-server-process-alive) 'erc-header-line)
                    (t 'erc-header-line-disconnected))))

          ;; (setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)


          ;; bitlbee-password is defvarred in my private file.

          (defun bitlbee-connect ()
            (interactive)
            (save-window-excursion
              (when (get-buffer "&bitlbee")
                (with-current-buffer "&bitlbee"
                  (erc-message "PRIVMSG" (concat (erc-default-target) " identify " bitlbee-password))
                  ;; (erc-message "PRIVMSG" (concat (erc-default-target) " account on"))
                  (erc-message "PRIVMSG" (concat (erc-default-target) " account off"))
                  (erc-message "PRIVMSG" (concat (erc-default-target) " blist all"))))))

          ;; (setq bitlbee-reconnect-timer (run-with-timer 0 60 'bitlbee-connect))

          (defun bitlbee-identify ()
            "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
            (when (and (string= "localhost" erc-session-server)
                       (string= "&bitlbee" (buffer-name)))
              (erc-message "PRIVMSG" (format "%s identify %s"
                                             (erc-default-target)
                                             bitlbee-password))))
          ;; (add-hook 'erc-join-hook 'bitlbee-identify)

          (setq erc-keywords '((".*Online.*" (:foreground "green"))
                               (".*Busy" (:foreground "red"))
                               (".*Away" (:foreground "red"))
                               (".*Do not" (:foreground "red"))
                               (".*Idle" (:foreground "orange"))))
          ))))

(defun lotus-erc/init-passwds ()
  (use-package passwds
      :defer t
      :config
      (progn
        )))

;;; packages.el ends here
