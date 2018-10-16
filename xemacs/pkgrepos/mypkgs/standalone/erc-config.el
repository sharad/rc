;;; erc-config.el --- Auto Loads

;; Copyright (C) 2015  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Started on  Thu Mar  3 16:12:21 2011 Sharad Pratap
;; $Id$
;; Keywords:convenience

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

;; erc config

;;; Code:



(defvar erc-autojoin-channels-alist nil)
(defvar erc-pals nil)
(defvar erc-fools nil)
(defvar erc-track-exclude-types nil)
(defvar erc-log-channels-directory nil)
(defvar erc-save-buffer-on-part nil)
(defvar erc-save-queries-on-quit nil)
(defvar erc-log-write-after-send nil)
(defvar erc-log-write-after-insert nil)
(defvar bitlbee-buffer-name nil)
(defvar erc-keywords nil)
(defvar fb/all-ids nil)
(defvar fb/old-erc-hooks nil)
(defvar erc-nickserv-passwords nil)
(defvar erc-prompt-for-nickserv-password nil)


(deh-require-maybe (and erc passwds bitlbee)

  ;; help
  ;; This is an example of how to make a new command.  Type "/uptime" to
  ;; use it.
  ;; (defun erc-cmd-UPTIME (&rest ignore)

  ;; (message "loading defun lotus-erc-start-or-switch")

  (defun lotus-erc-start-or-switch ()
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
          )))

  (deh-require-maybe erc-join
    (erc-autojoin-mode t))



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

  (defun lotus-erc-bnc4free-connect ()
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
      (lotus-erc-bnc4free-connect))




    ;; (concat bnc4free-username ":" bnc4free-password)

  (deh-require-maybe bitlbee
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
                  (setq erc-server-flood-penalty 0)))))

  (defun erc-quit-bitlbee-maybe (process)
    (when (and (get-buffer-process bitlbee-buffer-name)
               (equal (get-buffer-process bitlbee-buffer-name)
                      (get-process "bitlbee")))
      (bitlbee-stop)
      (kill-buffer bitlbee-buffer-name)))

  (add-hook 'erc-quit-hook
            'erc-quit-bitlbee-maybe)

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
        erc-interpret-controls-p t)


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
      (setq erc-insert-this nil)))


;;;--------------------------------------------------------------------------------
;;; Facebook-chat/bitlbee interaction stuff
;;;
;;; provides one extra erc command: /fbid, which will do absolutely
;;; crazy regexp matching of your facebook friends to rename them.
;;;
;;; There is also a function to add to erc-insert-pre-hook which
;;; maintains human names for new friends.
;;;
;;; first do /fbid in &bitlbee (warning, this took mee about 5 minutes
;;; with 100-ish friends. bitlbee messages are slow.)
;;;
;;; then do this:
;;;
;;; (add-hook 'erc-mode-hook
;;;      (lambda ()
;;;        (when (string= (buffer-name) "&bitlbee")
;;;          (add-hook 'erc-insert-pre-hook 'fb/maintain-human-names nil t))))
;;;
;;; with these three functions set, we will always automatically
;;; rename a person any time they log on to fb and they don't have a
;;; friendly nick.
;;;
;;; These hooks are basically one giant race condition though, so if
;;; you're not comfortable that it did the right thing, either don't
;;; save or do `info NICK' and you'll see if it did the right thing.
;;;
;;; Specifically, if two people sign on close-enough to simultaneously
;;; I can't think of any good way to guarantee that the ID I'm
;;; rename-ing is going to get their real name. (Because either
;;; bitlbee provides a multiline 'info' response, or erc converts it
;;; into a multiline response before erc-insert-pre-hook.) If you do
;;; /fbid before enabling these auto-id functions it really shouldn't
;;; be that big of a problem, though. I've got something like
;;; semaphores going on, but they're completely useless if the info
;;; responses do not arrive in a deterministic order.  Which, god
;;; knows.
;;;
;;; Another thing is that these do not save anything, because that
;;; could cause massively stupid errors. This is just a reminder: tell
;;; <root> to save if you want it to.
;;; --------------------------------------------------------------------------------

  (defvar fb/suffix "FB"
    "What to append to facebook nicks to make sure they don't clash
  with the same person in other networks")

  (defvar fb/last-info-request nil
    "the last name requested by an explicit 'info NICK'")
  (defvar fb/id nil
    "shitty attempt at semaphores.
Pretty sure these are guaranteed to not work, but they do
*reduce* the race window")
  (defvar fb/rename-in-progress nil
    "other part of the semaphore made with `fb/id'")

  (defun fb/maintain-human-names (msg)
    (cond ((string-match "localhost.*\\+v \\([u-][0-9]\\{4,\\}\\)" msg)
           (let ((fbid (match-string 1 msg)))
             (erc-message "PRIVMSG" (concat (erc-default-target) " info " fbid))))
          ((and (string-match "vCard information for[[:blank:]\n]+\\([u-][0-9]\\{4,\\}\\)" msg)
                (not fb/id))
           (setq fb/id (match-string 1 msg)))
          ((and fb/id
                (not fb/rename-in-progress)
                (string-match "Name: \\([[:alpha:] \\.-]+[[:alpha:]]\\)" msg))
           (setq fb/rename-in-progress t)
           (let ((fbid fb/id)
                 (name (concat (mapconcat 'identity (split-string (match-string 1 msg)) "") fb/suffix)))
             (erc-display-line (concat "<fbid> renaming " fbid " to " name))
             (erc-message "PRIVMSG" (concat (erc-default-target) " rename " fbid " " name)))
           (setq fb/rename-in-progress nil)
           (setq fb/id nil))))

  (defun fb/all-user-rename (msg)
    (when (string-match "Name: \\([[:alpha:] -]+[[:alpha:]]\\)" msg)
      (let ((fbid fb/id)
            ;; (name (concat (mapconcat 'identity (split-string (match-string 1 msg)) "") fb/suffix))
            (name (concat fb/suffix "/" (mapconcat 'identity (split-string (match-string 1 msg)) "")))
            )
        (erc-display-line (concat "<fbid> renaming " fbid " to " name))
        (erc-message "PRIVMSG" (concat (erc-default-target) " rename " fbid " " name)))
      (fb/get-info-all-names)))

  (defun fb/get-info-all-names ()
    (setq fb/id (pop fb/all-ids))
    (if fb/id
        (progn
          (erc-display-line (concat "<fbid> requesting info for " fb/id))
          (erc-message "PRIVMSG" (concat (erc-default-target) " info " fb/id)))
        (save-window-excursion
          (switch-to-buffer "&bitlbee")
          (setq erc-insert-pre-hook fb/old-erc-hooks)
          (erc-display-line "<fbid> Finished changing everyone's name. Don't forget to SAVE"))))

  (defun fb/grab-ids-from-blist (msg)
    (if (string-match "^\<root\> \\([u-][[:digit:]]\\{4,\\}\\)" msg)
        (push (match-string 1 msg) fb/all-ids)
        (when (string-match "[0-9]+ buddies (.*)" msg)
          (remove-hook 'erc-insert-pre-hook 'fb/grab-ids-from-blist)
          (fb/get-info-all-names))))

  (defun erc-cmd-FBID ()
    "Create nice names for all of your facebook friends

This takes a looong time, although basically just because of
waiting for responses from the server"
    (defvar fb/all-ids nil)
    (defvar fb/old-erc-hooks)
    (save-window-excursion
      (switch-to-buffer "&bitlbee")
      (setq fb/old-erc-hooks erc-insert-pre-hook)
      (setq erc-insert-pre-hook nil)
      (add-hook 'erc-insert-pre-hook 'fb/all-user-rename nil t)
      (add-hook 'erc-insert-pre-hook 'fb/grab-ids-from-blist nil t)
      (erc-message "PRIVMSG" (concat (erc-default-target) " blist all"))))


  ;; add all these hooks to only the &bitlbee channel
  (add-hook 'erc-mode-hook
            (lambda ()
              (when (string= (buffer-name) "&bitlbee")
                (add-hook 'erc-insert-pre-hook 'erc-ignore-unimportant nil t)
                ;; (add-hook 'erc-insert-pre-hook 'erc-facebook-user-identify nil t)
                ;; (add-hook 'erc-insert-pre-hook 'erc-facebook-set-fbid nil t)
                ;; (add-hook 'erc-insert-pre-hook 'erc-facebook-user-rename nil t)
                (add-hook 'erc-insert-pre-hook 'fb/maintain-human-names nil t)
                )))


  ;; The following are commented out by default, but users of other
  ;; non-Emacs IRC clients might find them useful.

  ;; these can all be accomplished by
  ;;     M-x ibuffer RET * M erc-mode RET D RET

  (setq
   ;; Kill buffers for server messages after quitting the server
   erc-kill-server-buffer-on-quit t
   ;; Kill buffers for private queries after quitting the server
   erc-kill-queries-on-quit t
   ;; Kill buffers for channels after /part
   erc-kill-buffer-on-part t
   ;; open query buffers in the current window
   erc-query-display 'window-noselect
   ;; enable/disable logging
   erc-log-p nil
   erc-log-insert-log-on-open nil
   )

  ;; (setq erc-encoding-coding-alist (quote (("#lisp" . utf-8)
  ;;                                         ("#nihongo" . iso-2022-jp)
  ;;                                         ("#truelambda" . iso-latin-1)
  ;;                                         ("#bitlbee" . iso-latin-1)
  ;;                                         ("&bitlbee" . iso-latin-1))))

  ;; (defvar erc-save-buffer-on-part nil)
  ;; (defvar erc-save-queries-on-quit nil)







  )



(deh-section "Bitlbee HTML"
  ;;   More ERC hacks
  ;; HTML

  ;; Using bitlbee to chat with people whose clients send everything as HTML can be a pain. Bitlbee provides settings to strip HTML, but you can also render it using w3m.
  (defun mah/maybe-wash-im-with-w3m ()
    "Wash the current im with emacs-w3m."
    (save-restriction
      (with-current-buffer (current-buffer)
        (let ((case-fold-search t))
          (goto-char (point-min))
          (when (re-search-forward "<HTML>.*</HTML>" nil t)
            (print (match-string 0))
            (narrow-to-region (match-beginning 0) (match-end 0))
            (let ((w3m-safe-url-regexp mm-w3m-safe-url-regexp)
                  w3m-force-redisplay)
              (w3m-region (point-min) (point-max))
              (goto-char (point-max))
              (delete-char -2))
            (when (and mm-inline-text-html-with-w3m-keymap
                       (boundp 'w3m-minor-mode-map)
                       w3m-minor-mode-map)
              (add-text-properties
               (point-min) (point-max)
               (list 'keymap w3m-minor-mode-map
                     ;; Put the mark meaning this part was rendered by emacs-w3m.
                     'mm-inline-text-html-with-w3m t))))))))
  (add-hook 'erc-insert-modify-hook 'mah/maybe-wash-im-with-w3m)
  (autoload 'w3m-region "w3m" "Render region using w3m")
)





(deh-require-maybe (and passwds erc-services)

  (erc-services-mode 1)

  (setq ;; erc-auto-query 'frame
        ;; erc-auto-query 'buffer
        erc-auto-query 'window-noselect
        erc-nickserv-passwords
        `(
          (freenode (,(cons "sharad"  freenode-pass)
                     ("nick-two" . "pass")))
          (BitlBee (,(cons "sharad"  bitlbee-password)))
          (DALnet (("nick" . "password"))))
        erc-prompt-for-nickserv-password nil))





(deh-section "ERC Auto Query"
;; (defcustom erc-reuse-frames t
;;  *Determines whether new frames are always created.
;; Non-nil means that a new frame is not created to display an ERC
;; buffer if there is already a window displaying it.  This only has
;; effect when `erc-join-buffer' is set to `frame'."

;; (defcustom erc-frame-dedicated-flag nil
;;   "*Non-nil means the erc frames are dedicated to that buffer.
;; This only has effect when `erc-join-buffer' is set to `frame'."

;; (defcustom erc-join-buffer 'buffer
;;   "Determines how to display a newly created IRC buffer.
;;
;; The available choices are:
;;
;;   'window          - in another window,
;;   'window-noselect - in another window, but don't select that one,
;;   'frame           - in another frame,
;;   'bury            - bury it in a new buffer,
;;   'buffer          - in place of the current buffer,
;;   any other value  - in place of the current buffer."

  (setf ;; erc-auto-query 'frame
   erc-auto-query 'window-noselect
   ;; erc-join-buffer 'window
   erc-join-buffer 'buffer
   erc-kill-queries-on-quit t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;; erc.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun erc-cmd-QUERY (&optional user)
;;   "Open a query with USER.
;; The type of query window/frame/etc will depend on the value of
;; `erc-query-display'.

;; If USER is omitted, close the current query buffer if one exists
;; - except this is broken now ;-)"
;;   (interactive
;;    (list (read-from-minibuffer "Start a query with: " nil)))
;;   (let ((session-buffer (erc-server-buffer))
;;      (erc-join-buffer erc-query-display))
;;     (if user
;;      (erc-query user session-buffer)
;;       ;; currently broken, evil hack to display help anyway
;;       ;(erc-delete-query))))
;;       (signal 'wrong-number-of-arguments ""))))
;; (defalias 'erc-cmd-Q 'erc-cmd-QUERY)
;; (defun erc-auto-query (proc parsed)
;;   ;; FIXME: This needs more documentation, unless it's not a user function --
;;   ;; Lawrence 2004-01-08
;;   "Put this on `erc-server-PRIVMSG-functions'."
;;   (when erc-auto-query
;;     (let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
;;         (target (car (erc-response.command-args parsed)))
;;         (msg (erc-response.contents parsed))
;;         (query  (if (not erc-query-on-unjoined-chan-privmsg)
;;                     nick
;;                   (if (erc-current-nick-p target)
;;                       nick
;;                     target))))
;;       (and (not (erc-ignored-user-p (erc-response.sender parsed)))
;;         (or erc-query-on-unjoined-chan-privmsg
;;             (string= target (erc-current-nick)))
;;         (not (erc-get-buffer query proc))
;;         (not (erc-is-message-ctcp-and-not-action-p msg))
;;         (let ((erc-join-buffer erc-auto-query))
;;           (erc-cmd-QUERY query))
;;         nil))))
;; ;; call from erc-open which call from erc-query
;; (defun erc-setup-buffer (buffer)
;;   "Consults `erc-join-buffer' to find out how to display `BUFFER'."
;;   (cond ((eq erc-join-buffer 'window)
;;       (if (active-minibuffer-window)
;;           (display-buffer buffer)
;;         (switch-to-buffer-other-window buffer)))
;;      ((eq erc-join-buffer 'window-noselect)
;;       (display-buffer buffer))
;;      ((eq erc-join-buffer 'bury)
;;       nil)
;;      ((eq erc-join-buffer 'frame)
;;       (when (or (not erc-reuse-frames)
;;                 (not (get-buffer-window buffer t)))
;;         ((lambda (frame)
;;                   (raise-frame frame)
;;                   (select-frame frame))
;;                (make-frame (or erc-frame-alist
;;                                default-frame-alist)))
;;       (switch-to-buffer buffer)
;;       (when erc-frame-dedicated-flag
;;         (set-window-dedicated-p (selected-window) t))))
;;      (t
;;       (if (active-minibuffer-window)
;;           (display-buffer buffer)
;;         (switch-to-buffer buffer)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; erc.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; erc.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun erc-auto-query (proc parsed)
  ;; FIXME: This needs more documentation, unless it's not a user function --
  ;; Lawrence 2004-01-08
  "Put this on `erc-server-PRIVMSG-functions'."
  (when erc-auto-query
    (let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
           (target (car (erc-response.command-args parsed)))
           (msg (erc-response.contents parsed))
           (query  (if (not erc-query-on-unjoined-chan-privmsg)
                       nick
                     (if (erc-current-nick-p target)
                         nick
                       target))))
      (and (not (erc-ignored-user-p (erc-response.sender parsed)))
           (or erc-query-on-unjoined-chan-privmsg
               (string= target (erc-current-nick)))
           (not (erc-get-buffer query proc))
           (not (erc-is-message-ctcp-and-not-action-p msg))
           (let ((erc-join-buffer erc-auto-query)
                 (session-buffer (erc-server-buffer)))
             ;; (erc-cmd-QUERY query)
             (if query
                 (erc-query query session-buffer)
                 ;; currently broken, evil hack to display help anyway
                                        ;(erc-delete-query))))
                 (signal 'wrong-number-of-arguments "")))
           nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; erc.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(deh-section "ERC Notify"
  ;; from: http://www.enigmacurry.com/2008/08/07/emacs-irc-erc-with-noticeable-notifications/
  ;; http://www.emacswiki.org/cgi-bin/wiki/ErcPageMe

  (deh-require-maybe erc-nick-notify
    (setq erc-nick-notify-delay '(0 0 10)
          erc-nick-notify-cmd "notify-send"
          erc-nick-notify-icon "~/MyEmacs/Image/Irc.png"
          erc-nick-notify-timeout 10000
          erc-nick-notify-urgency "normal"
          erc-nick-notify-category "im.received")
    ;; not required
    (erc-nick-notify-mode -1))

  (add-hook 'lotus-enable-login-session-interrupting-feature-hook
            '(lambda ()
              ;; update python DISPLAY env variable.
              (if (and (getenv "DISPLAY" (selected-frame))
                       (eq window-system 'x))
                  (pymacs-exec (format "os.environ['DISPLAY'] = '%s'" (getenv "DISPLAY" (selected-frame)))))))

  (require 'python-config)

  (defun notify-desktop (title message &optional duration &optional icon)
    "Pop up a message on the desktop with an optional duration (forever otherwise)"
    ;; (message "Notification")
    (condition-case e
        (progn
          (pymacs-exec "import os")
          (pymacs-exec "print os.getenv('DISPLAY',False)"))
      ;; ('error (pymacs-terminate-services-force)))
      ('error nil))
    (condition-case e
        (progn
          (pymacs-exec "import pynotify")
          (pymacs-exec "pynotify.init('Emacs')")
          (unless (pymacs-eval "os.getenv('DISPLAY',False)")
            (pymacs-exec (format "os.environ['DISPLAY'] = '%s'" (getenv "DISPLAY" (selected-frame)))))
          (let ((title   (replace-regexp-in-string "'" "\\'" title nil t))
                (message (replace-regexp-in-string "'" "\\'" message nil t)))
           (if icon
              (pymacs-exec (format "msg = pynotify.Notification('%s','%s','%s')"
                                   title message icon))
              (pymacs-exec (format "msg = pynotify.Notification('%s','%s')"
                                   title message))))
          (if duration
              (pymacs-exec (format "msg.set_timeout(%s)" duration)))
          (pymacs-exec "msg.show()"))
      ('error (progn
                (message "Error in notify-desktop: %s" e)
                (pymacs-terminate-services-force)))))

  (when nil
    (notify-desktop "ss" "aaa")
    (notify-desktop (format "%s - %s" "asfsadf"
                            (format-time-string "%b %d %I:%M %p"))
                    "Test" 0 "gnome-emacs")

    (notify-desktop (format "%s - %s" "asfsadf"
                            (format-time-string "%b %d %I:%M %p"))
                    "Test" 1 "gnome-emacs"))





  ;; (undefine-function-remember 'notify-desktop)
  ;; (redefine-function-remembered 'notify-desktop)

  ;; Notify me when someone wants to talk to me.
  ;; Heavily based off of ErcPageMe on emacswiki.org, with some improvements.
  ;; I wanted to learn and I used my own notification system with pymacs
  ;; Delay is on a per user, per channel basis now.
  (defvar erc-page-nick-alist nil
    "Alist of 'nickname|target' and last time they triggered a notification"
    )

  (defvar erc-page-nick-block-list nil
    "Alist of 'nickname|target' and last time they triggered a notification")

  (add-to-list 'erc-page-nick-block-list "skypeconsole")
  (add-to-list 'erc-page-nick-block-list "root")

  (defvar erc-page-duration 100 "notification duration.")

  (defun erc-notify-allowed (nick target &optional delay)
    "Return true if a certain nick has waited long enough to notify"
    (unless delay (setq delay 30))
    (unless (member nick erc-page-nick-block-list)
      (let ;; ((cur-time (time-to-seconds (current-time)))
          ((cur-time (float-time (current-time)))
           (cur-assoc (assoc (format "%s|%s" nick target) erc-page-nick-alist))
           (last-time))
        (if cur-assoc
            (progn
              (setq last-time (cdr cur-assoc))
              (setcdr cur-assoc cur-time)
              (> (abs (- cur-time last-time)) delay))
            (push (cons (format "%s|%s" nick target) cur-time) erc-page-nick-alist)
            t))))

  (defvar debug-erc-local nil "debug-erc-local")
  (setq debug-erc-local t)

  (require 'frame-config)

  (defun erc-notify-PRIVMSG (proc parsed)
    (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
          (target (car (erc-response.command-args parsed)))
          (msg (erc-response.contents parsed)))
      ;;Handle true private/direct messages (non channel)
      (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
                 (erc-current-nick-p target)
                 (erc-notify-allowed nick target))
                                        ;Do actual notification
        (ding)
        ;; (x-urgency-hinthf (selected-frame) t)
        (notify-desktop (format (if debug-erc-local "%s - %s" "PRIVMSG-%s - %s")
                                nick
                                target
                                (format-time-string "%b %d %I:%M %p"))
                        msg erc-page-duration "gnome-emacs")
        (x-urgency-hinthf (selected-frame) t))

      ;;Handle channel messages when my nick is mentioned
      (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
                 (string-match (erc-current-nick) msg)
                 (erc-notify-allowed nick target))
                                        ;Do actual notification
        (ding)
        ;; (x-urgency-hinthf (selected-frame) t)
        (notify-desktop (format (if debug-erc-local "%s - %s" "PRIVMSG-%s - %s")
                                target
                                (format-time-string "%b %d %I:%M %p"))
                        (format "%s: %s" nick msg) erc-page-duration "gnome-emacs")
        (x-urgency-hinthf (selected-frame) t))))

  (add-hook 'erc-server-PRIVMSG-functions 'erc-notify-PRIVMSG))






(deh-section "monitor"

  (defvar lotus-erc-monitor-user-list nil "list")

  (defun erc-cmd-MONITOR (user &optional server)
  "Display whois information for USER.

If SERVER is non-nil, use that, rather than the current server."
  ;; FIXME: is the above docstring correct?  -- Lawrence 2004-01-08
  (let ()
    (message "name %s server %s" user server)
    (push user lotus-erc-monitor-user-list))
  t)

  (defun erc-notify-MONITOR (proc parsed)
    (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
          (target (car (erc-response.command-args parsed)))
          (msg (erc-response.contents parsed)))
      (when (member nick lotus-erc-monitor-user-list)
          ;;Handle true private/direct messages (non channel)
          (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
                     (erc-current-nick-p target)
                     (erc-notify-allowed nick target))
                                        ;Do actual notification
            (ding)
            (notify-desktop (format (if debug-erc-local "%s - %s" "MONITOR-%s - %s")
                                    nick
                                    (format-time-string "%b %d %I:%M %p"))
                            msg erc-page-duration "gnome-emacs")
            )
          ;; (message "nick %s" nick)
          ;;Handle channel messages when my nick is mentioned
          (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
                     (string-match (erc-current-nick) msg)
                     (member nick lotus-erc-monitor-user-list)
                     (erc-notify-allowed nick target))
                                        ;Do actual notification
            ;; (message "ttttttttt")
            (ding)
            (notify-desktop (format (if debug-erc-local "%s - %s" "MONITOR-%s - %s")
                                    target
                                    (format-time-string "%b %d %I:%M %p"))
                            (format "%s: %s" nick msg) erc-page-duration "gnome-emacs")))))

  (add-hook 'erc-server-PRIVMSG-functions 'erc-notify-MONITOR))




(deh-section "h4x0r"
  ;; http://www.emacswiki.org/emacs/ErcHaxorCode

  ;; EliteSpeech (h4x0r!) has code to transform normal speech into
  ;; elite (31337!) speech. We can use that for a little haxor output –
  ;; useful when you are trapped with script kiddies on a
  ;; channel. Remember to set the ‘h4x0r-unreadable’ variable to your
  ;; favorite level. The default is 5.

  (add-hook 'erc-send-pre-hook 'erc-maybe-h4x0r)

  (define-minor-mode erc-h4x0r-mode
      "Toggle automatic usage of h4x0r code for everything you type in ERC.")

  (defun erc-maybe-h4x0r (ignore)
    "Change the text to h4x0r code, if `erc-h4x0r-mode' is non-nil."
    (when erc-h4x0r-mode
      (with-temp-buffer
        (insert str)
        (erc-h4x0r)
        (setq str (buffer-string)))))

  (defun erc-h4x0r ()
    "Transform the buffer into h4x0r code."
    (h4x0r-region (point-min) (point-max)))

    (autoload 'h4x0r-region "h4x0r"))

(deh-section "erc-logs"
 (defun erc-save-buffers-in-logs ()
   (interactive)
   (mapc (lambda(buf)
    (save-excursion
      (set-buffer buf)
      (erc-save-buffer-in-logs)))
  (erc-buffer-filter (lambda() t))))

 (defadvice save-buffers-kill-emacs
   (before save-logs-before-save-buffers-kill-emacs (&rest args) activate)
   'erc-save-buffers-in-logs)

 (defadvice save-some-buffers
   (before save-logs-before-save-some-buffers (&rest args) activate)
   'erc-save-buffers-in-logs)

 (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

 (setq erc-save-buffer-on-part nil
       erc-save-queries-on-quit nil
       erc-log-write-after-send t
       erc-log-write-after-insert t)


 ;; Character encoding problems
 ;; If you log, and erc frequently hangs on you upon seeing certain international characters, try this:

 ;; (setq bbdb-file-coding-system 'raw-text)
 ;; (setq-default buffer-file-coding-system 'raw-text)
 ;; (setq buffer-file-coding-system 'raw-text)
 (add-hook 'erc-mode-hook (lambda () (setq buffer-file-coding-system 'raw-text))))




(deh-section "erc/fix"
  (defun erc-cmd-QUERY (&optional user)
    "Open a query with USER.
The type of query window/frame/etc will depend on the value of
`erc-query-display'.

If USER is omitted, close the current query buffer if one exists
- except this is broken now ;-)"
    (interactive
     (list (read-from-minibuffer "Start a query with: " nil)))
    (let ((session-buffer (erc-server-buffer))
          (erc-join-buffer erc-query-display))
      (message "%s" erc-default-recipients)
      (if user
          (erc-query user session-buffer)
          ;; currently broken, evil hack to display help anyway
          (erc-delete-query))))
  ;;(signal 'wrong-number-of-arguments ""))))

  (defun erc-delete-query ()
    "Delete the topmost target if it is a QUERY."
    (message "%s" erc-default-recipients)
    (let ((d1 (car erc-default-recipients))
          (d2 (cdr erc-default-recipients)))
      (if (and (listp d1)
               (eq (car d1) 'QUERY))
          (setq erc-default-recipients d2)
          (if (and
               (stringp d1)
               (equal (buffer-name) d1))
              (kill-buffer d1))
          ;; (error "Current target is not a QUERY")
          ))))

  ;;http://www.emacswiki.org/emacs/ErcStartupFiles
  ;; (defun my-irc ()
  ;;   "Start to waste time on IRC with ERC."
  ;;   (interactive)
  ;;   (select-frame (make-frame '((name . "Emacs IRC")
  ;;                               (minibuffer . t))))
  ;;   ;; (call-interactively 'erc-ircnet)
  ;;   (call-interactively 'lotus-start-erc))

  ;;http://www.emacswiki.org/emacs/ErcAutoJoin
  ;; (setq erc-autojoin-channels-alist
  ;;       '(
  ;;         ;; ("freenode.net" "#emacs" "#wiki" "#nethack")
  ;;         ("freenode.net" "#emacs")
  ;;         ;; ("localhost" "#bitlbee")
  ;;         ("localhost" "&bitlbee")))

  ;;   (defun lotus-start-erc ()
  ;;     (interactive)
  ;;     (erc :server "irc.freenode.net" :port 6667 :nick "sharad" :password "ctrplmqw")
  ;;     (erc :server "localhost" :port 6667 :nick "sharad" :password "qwe123"))

  ;; )


  ;;

(provide 'erc-config)

;;; erc-config.el ends here
