;;; session-config.el --- session setting

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@merunetworks.com>
;; Keywords: convenience

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


(require 'general-testing)
(require 'cl)

(eval-when-compile
  '(require 'cl))



;; (sharad/elscreen-get-screen-to-name-alist)

(eval-after-load "elscreen"
   '(defun sharad/elscreen-get-screen-to-name-alist ()
    ;; (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
     (elscreen-notify-screen-modification-suppress
      (elscreen-set-window-configuration (elscreen-get-current-screen)
       (elscreen-current-window-configuration))
      (let* ((screen-list (sort (elscreen-get-screen-list) '<))
             screen-name screen-to-name-alist nickname-type-map)
        (elscreen-save-screen-excursion
         (mapcar
          (lambda (screen)
            ;; If nickname exists, use it.
            (setq screen-name (elscreen-get-screen-nickname screen))
            ;; Nickname does not exist, so examine major-mode and buffer-name.
            (when (null screen-name)
              (elscreen-goto-internal screen)

              (setq nickname-type-map
                    (mapcar
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (or (elscreen-get-alist-to-nickname
                              elscreen-mode-to-nickname-alist-internal
                              'string-match (symbol-name major-mode))
                             (elscreen-get-alist-to-nickname
                              elscreen-buffer-to-nickname-alist-internal
                              'string-match (buffer-name))
                             (cons 'buffer-name (buffer-name)))))
                     (window-list)))

              (let (nickname-list)
                (while (> (length nickname-type-map) 0)
                  (let ((type (caar nickname-type-map))
                        (name (cdar nickname-type-map)))
                    (when name
                      (setq nickname-list (cons name nickname-list)))
                    (setq nickname-type-map
                          (if (eq type 'nickname)
                              (delete (car nickname-type-map) nickname-type-map)
                              (cdr nickname-type-map)))))
                ;; (setq screen-name
                ;;       (mapconcat 'identity (reverse nickname-list) ":"))
               (setq screen-name (reverse nickname-list))))

            (set-alist 'screen-to-name-alist screen screen-name))
          screen-list))

       ;; (elscreen-set-screen-to-name-alist-cache screen-to-name-alist)
       screen-to-name-alist))))

(deh-require elscreen

  (defvar elscreen-session-restore-create-scratch-buffer nil "elscreen-session-restore-create-scratch-buffer")

  (setq desktop-base-file-name "session.desktop")

  ;;{{ http://stackoverflow.com/a/13711234
  ;; from: http://stackoverflow.com/questions/847962/what-alternate-session-managers-are-available-for-emacs
  (defvar *emacs-frame-session-directory*
    "~/.emacs.d/session/frames"
    "The directory where the emacs configuration files are stored.")

  (defvar *elscreen-tab-configuration-store-filename*
    "elscreen"
    "The file where the elscreen tab configuration is stored.")

  ;; (desktop-save (fmsession-read-location))
  ;; (desktop-read (fmsession-read-location))

  (require 'utils-config)

  (defun elscreen-session-make-session-list ()
    (let (session-list)
      (push (cons 'screens (reverse (sharad/elscreen-get-screen-to-name-alist))) session-list)
      (push (cons 'current-buffer (buffer-name (current-buffer))) session-list)
      (push (cons 'current-screen (elscreen-get-current-screen)) session-list)))

  (defun elscreen-session-store (elscreen-session)
    (interactive "Ffile: " )
    (with-temp-file elscreen-session
      (insert
       (prin1-to-string (elscreen-session-make-session-list)))))

  ;; (defun elscreen-session-restore (elscreen-session)
  ;;   (let ((screens (reverse (sharad/read-file elscreen-session))))
  ;;     (elscreen-set-screen-to-name-alist-cache screens)))

  (defvar *elscreen-session-restore-data* nil "")

  (defun elscreen-session-restore (elscreen-session)
    (interactive "ffile: " )
    (testing
     (message "Nstart: session-current-buffer %s" elscreen-session))
    (let* (screen buffers
                  (elscreen-session-list (sharad/read-file elscreen-session))
           (screens
            (or
             (cdr (assoc 'screens elscreen-session-list))
             '((0 "*scratch*"))))
           (session-current-buffer
            (cadr (assoc
                   (cdr (assoc 'current-screen elscreen-session-list))
                   screens))))
      (testing
       (message "Bstart: session-current-buffer %s" session-current-buffer)
       (message "Astart: screen-to-name-alist %s" elscreen-session-list))
      (while screens
        (setq screen (caar screens))
        (setq buffers (cdar screens))
        (if (when (bufferp (get-buffer (car buffers)))
              ;; (message "if screen: %s buffer: %s" screen buffers)
              (if (eq screen 0) ;; (eq (elscreen-get-current-screen) 0)
                    (switch-to-buffer (car buffers))
                    (elscreen-find-and-goto-by-buffer (car buffers) t t))
              (cdr buffers))
            (while (cdr buffers)
              (testing (message "while: screen: %s buffer: %s" screen (cadr buffers)))
              (switch-to-buffer-other-window (car (cdr buffers)))
              (setq buffers (cdr buffers)))
            (testing (message "else")))
        (setq screens (cdr screens)))

      ;; (when elscreen-session-restore-create-scratch-buffer
      ;;   (elscreen-find-and-goto-by-buffer (get-buffer-create "*scratch*") t t))
      (elscreen-create)                 ;trap

      (if (get-buffer session-current-buffer)
          (progn
            ;; (elscreen-find-and-goto-by-buffer (get-buffer session-current-buffer) nil nil)
            (setq *elscreen-session-restore-data* (list (cons 'cb session-current-buffer)))
            (testing
             (message "*elscreen-session-restore-data* %s" *elscreen-session-restore-data*)))
          (testing
           (message "in when session-current-buffer %s" session-current-buffer))))
    (testing
     (message "elscreen-notify-screen-modification"))
    (elscreen-notify-screen-modification 'force-immediately))

  (defun fmsession-read-location (&optional initial-input)
    (let ((used t)
          (sel))
      (while used
        (setq used
              (member
               (setq sel (fmsession-read-location-internal initial-input))
               (remove-if #'null
                          (mapcar (lambda (f) (frame-parameter f 'frame-spec-id)) (frame-list))))))
      sel))

  (defun fmsession-read-location-internal (&optional initial-input)
    (unless (file-directory-p *emacs-frame-session-directory*)
      (make-directory *emacs-frame-session-directory*))
    (condition-case terr
        (concat
         *emacs-frame-session-directory* "/"
         (ido-completing-read "Session: "
                              (remove-if-not
                               #'(lambda (dir)
                                   (and
                                    (file-directory-p
                                     (concat *emacs-frame-session-directory* "/" dir))
                                    (not
                                     (member
                                      (concat *emacs-frame-session-directory* "/" dir)
                                      (remove-if #'null
                                                 (mapcar (lambda (f) (frame-parameter f 'frame-spec-id)) (frame-list)))))))
                               (directory-files *emacs-frame-session-directory* nil "[a-zA-Z]+"))
                              nil
                              nil
                              initial-input))
      ('quit nil)))

  (defun fmsession-store (session-dir)
    "Store the elscreen tab configuration."
    (interactive
     (list (fmsession-read-location)))
    (let ((elscreen-session (concat session-dir "/" *elscreen-tab-configuration-store-filename*)))
      (make-directory session-dir t)
      (when (file-directory-p session-dir)
        (elscreen-session-store elscreen-session))))

  ;; (push #'elscreen-store kill-emacs-hook)

  (defun fmsession-restore (session-dir)
    "Restore the elscreen tab configuration."
    (interactive
     (list (fmsession-read-location)))
    (if session-dir
        (let ((elscreen-session (concat session-dir "/" *elscreen-tab-configuration-store-filename*)))
          (if (file-directory-p session-dir)
              (progn ;; (eq (type-of (desktop-read session-dir)) 'symbol)
                (message "elscreen-session-restore %s" elscreen-session)
                (elscreen-session-restore elscreen-session))
              (message "no such %s dir exists." session-dir)))
        (message "session-dir is nil, not doing anything.")))

  ;; (elscreen-restore)
  ;;}}

    ;;{{

  (defvar *restore-frame-session* nil "*restore-frame-session*")

  (defadvice server-create-window-system-frame
      (before set-restore-frame-session activate)
    "remove-scratch-buffer"
    (setq *restore-frame-session* t))

  (defadvice server-create-window-system-frame
      (after remove-scratch-buffer activate)
    "remove-scratch-buffer"
    (if *restore-frame-session*
        (progn
          (setq *restore-frame-session* nil)
          (testing (message "in running server-create-window-system-frame afer advise"))
          (if *elscreen-session-restore-data*
              (let ((cb (get-buffer (cdr (assoc 'cb *elscreen-session-restore-data*)))))
                (testing
                 (message "running server-create-window-system-frame afer advise if")
                 (message "*elscreen-session-restore-data* %s" *elscreen-session-restore-data*))
                (when cb
                  (elscreen-kill)
                  (elscreen-find-and-goto-by-buffer cb nil nil)
                  (setq *elscreen-session-restore-data* nil)
                  (elscreen-notify-screen-modification 'force-immediately)))
              (testing (message "running server-create-window-system-frame afer advise else")))))
    ad-return-value)


  ;;}}

  ;;{{
  (deh-section "per frame session"

    (require 'emacs-panel)

    (defun set-this-frame-session-location (frame)
      (interactive
       (list (selected-frame)))
      (select-frame frame)
      (message "in set-this-frame-session-location")
      (let* ((wm-hints
              (ignore-errors (emacs-panel-wm-hints)))
             (desktop-name (if wm-hints
                               (nth
                                (cadr (assoc 'current-desktop wm-hints))
                                (cdr (assoc 'desktop-names wm-hints)))))
             (location (fmsession-read-location desktop-name)))
        (unless wm-hints
          (message "Some error in wm-hints"))
        (message "set-this-frame-session-location: %s" location)
        (set-frame-parameter frame 'frame-spec-id location)
        location))

    ;; (defun set-this-frame-session-location (frame)
    ;;   (select-frame frame)
    ;;   (message "in set-this-frame-session-location")
    ;;   (let* ((wm-hints
    ;;           (ignore-errors (emacs-panel-wm-hints)))
    ;;          (desktop-name (if wm-hints
    ;;                            (nth
    ;;                             (cadr (assoc 'current-desktop wm-hints))
    ;;                             (cdr (assoc 'desktop-names wm-hints)))))
    ;;          (location (fmsession-read-location desktop-name)))
    ;;     (unless wm-hints
    ;;       (message "Some error in wm-hints"))
    ;;     (message "set-this-frame-session-location: %s" location)
    ;;     (when location
    ;;       (set-frame-parameter frame 'frame-spec-id location)
    ;;       ;; (modify-frame-parameters frame
    ;;       ;;                          (list (cons 'frame-spec-id location)))
    ;;       (fmsession-restore location))))

    (defun restore-frame-session (frame)
      (if *restore-frame-session*
          (progn
            (select-frame frame)
            (fmsession-restore (set-this-frame-session-location frame)))
          (message "not restoring screen session.")))

    (defun save-frame-session (frame)
      (message "in save-frame-session:")
      (let ((location (frame-parameter frame 'frame-spec-id)))
        (when location
          (message "saved the session for %s" location)
          (fmsession-store location))))

    (defun save-all-frames-session ()
      (dolist (f (frame-list))
	(save-frame-session f)))


    ;; (add-hook '*sharad/after-init-hook*
    (add-hook 'sharad/enable-startup-inperrupting-feature-hook
              '(lambda ()
                ;; (add-hook 'after-make-frame-functions 'set-this-frame-session-location t)
                (add-hook 'after-make-frame-functions 'restore-frame-session t)
                (add-hook 'delete-frame-functions 'save-frame-session)
                (add-hook 'kill-emacs-hook 'save-all-frames-session))
              t)

  (testing
     (frame-parameter (selected-frame) 'frame-spec-id)
     after-make-frame-functions
     delete-frame-functions
     *sharad/after-init-hook*
     ))


  ;;}}
  )


(deh-require-maybe desktop
  ;; (testing
  ;; http://stackoverflow.com/questions/2703743/restore-emacs-session-desktop
  ;; (desktop-save-mode 1)
  ;; (desktop-read)

  ;; from: http://www.emacswiki.org/emacs/DeskTop
  ;; You can add any extra variables you want saved across sessions to the list ‘desktop-globals-to-save’. For example:
  ;; (setq history-length 250)
  ;; (add-to-list 'desktop-globals-to-save 'file-name-history)

  ;; Specifying Files Not to be Opened

  ;; You can specify buffers which should not be saved, by name or by mode, e.g.:


  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$")

        ;; check it http://stackoverflow.com/a/4055504/341107
        desktop-files-not-to-save       ;very important
        ;; default value
        ;; "\\(^/[^/:]*:\\|(ftp)$\\)"
        ;; that do not allow to save trampe file
        "^$")

  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


  ;;{{
  ;; Automatically Overriding Stale Locks

  ;; If you are using desktop-mode and your emacs crashes (or more
  ;; likely, your system crashes), then your desktop file will not be
  ;; released, and emacs will bother you about using it next time you
  ;; start it up. To avoid this, I wrote the following code to
  ;; override the lock on a desktop file if the indicated process is
  ;; not still running.

  ;; desktop-override-stale-locks.el begins here
  ;;   (defun emacs-process-p (pid)
  ;;     "If pid is the process ID of an emacs process, return t, else nil.
  ;; Also returns nil if pid is nil."
  ;;     (when pid
  ;;       (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
  ;;         (when (file-exists-p cmdline-file)
  ;;           (with-temp-buffer
  ;;             (insert-file-contents-literally cmdline-file)
  ;;             (goto-char (point-min))
  ;;             (search-forward "emacs" nil t)
  ;;             pid)))))

  (defadvice desktop-owner (after pry-from-cold-dead-hands activate)
    "Don't allow dead emacsen to own the desktop file."
    (when (not (emacs-process-p ad-return-value))
      (setq ad-return-value nil)))
  ;; desktop-override-stale-locks.el ends here

  ;; If anyone has a more robust implementation of `emacs-process-p,’ feel free to provide it.

  ;; I implemented ‘emacs-process-p’ by following way, it could work on both Windows and Linux.

  (defun emacs-process-p (pid)
    "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
    (when pid
      (let ((attributes (process-attributes pid)) (cmd))
        (dolist (attr attributes)
          (if (string= "comm" (car attr))
              (setq cmd (cdr attr))))
        (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

  ;; I think the original function contains an error. Should it not end something like:

  ;; (when (search-forward "emacs" nil t)
  ;;   pid))))))
  ;;}}

  ;;{{
  ;; Minimal Setup

  ;; This is for people who only want minimal session management
  ;; functionality, and don’t want their previous sessions automatically
  ;; restored at start-up. Note that you need desktop-save-mode NOT
  ;; ENABLED for this to work as intended.

  ;; It works for me with one desktop, with more than one may need some tweaking.

  ;; use only one desktop
  (setq desktop-path '("~/.emacs.d/"))
  (setq desktop-dirname "~/.emacs.d/")
  (setq desktop-base-file-name
        (concat
         "emacs-desktop"
         (if (boundp 'server-name)
             (concat "-" server-name))))

  ;; remove desktop after it's been read
  (add-hook 'desktop-after-read-hook
            '(lambda ()
              ;; desktop-remove clears desktop-dirname
              (setq desktop-dirname-tmp desktop-dirname)
              (desktop-remove)
              (setq desktop-dirname desktop-dirname-tmp)))

  ;; Since all lists will be truncated when saved, it is important to
  ;; have a high default history length, for example. If that is not
  ;; enough, follow the suggestions in the doc-string of
  ;; ‘desktop-globals-to-save’:

  ;;   An element may be variable name (a symbol) or a cons cell of
  ;;   the form (VAR . MAX-SIZE), which means to truncate VAR’s value
  ;;   to at most MAX-SIZE elements (if the value is a list) before
  ;;   saving the value.


  ;; Auto-Saving the Desktop

  ;; I’m starting to work on a new package called desktop-recover.el
  ;; with some improvements like this. Alternatively, you can just add
  ;; something like this to your init file to auto-save your desktop
  ;; when Emacs is idle: – Doom


  ;; (defun my-desktop-save ()
  ;;   (interactive)
  ;;   ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  ;;   (let ((owner (or (desktop-owner) -1)))
  ;;     (if (eq owner (emacs-pid))
  ;;       (desktop-save desktop-dirname)
  ;;       ;; (desktop-save-in-desktop-dir)
  ;;       (error "You %d are not the desktop owner %d."
  ;;              (emacs-pid) owner))))

  (defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (let ((owner (or (desktop-owner) -1)))
      (if (or
           (eq owner (emacs-pid))
           ;; TODO: it was mean to be used as non-obtrusive and non-interctive
           (y-or-n-p (format
                      "You %d are not the desktop owner %d\nOverwrite existing desktop (might be it was not restore properly at startup)? "
                      (emacs-pid) owner)))
          (desktop-save desktop-dirname)
        ;; (desktop-save-in-desktop-dir)
          (progn
            (remove-hook 'auto-save-hook 'my-desktop-save)
            (error "You %d are not the desktop owner %d. removed my-desktop-save from auto-save-hook."
                   (emacs-pid) owner)))))

  (testing
   (remove-hook 'auto-save-hook 'my-desktop-save))
  ;; giving life to it.
  (add-hook 'auto-save-hook 'my-desktop-save)

  (defun sharad/desktop-saved-session ()
    (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

  ;; use session-save to save the desktop manually
  (defun sharad/desktop-session-save ()
    "Save an emacs session."
    (interactive)
    (if (sharad/desktop-saved-session)
        (if (y-or-n-p "Overwrite existing desktop (might be it was not restore properly at startup)? ")
            (desktop-save-in-desktop-dir)
            (message "Session not saved."))
        (desktop-save-in-desktop-dir)))

  ;; use session-restore to restore the desktop manually
  (defun sharad/desktop-session-restore ()
    "Restore a saved emacs session."
    (interactive)
    (message "in desktop-session-restore")
    (if (sharad/desktop-saved-session)
        (progn
          (message "desktop-session-restore")
          (condition-case e
              (desktop-read)
            ('error (message "Error in desktop-read: %s" e)))
          t)
        (message "No desktop found."))
    (when (y-or-n-p "Do you want to set session of frame? ")
      (restore-frame-session (selected-frame)))
    (message "leaving desktop-session-restore"))

  (add-hook 'session-before-save-hook
            'my-desktop-save)
  ;; 'sharad/desktop-session-save)

  (testing
   (remove-hook 'session-before-save-hook
                'my-desktop-save))

  ;; ;; ask user whether to restore desktop at start-up
  (add-hook ;; 'after-init-hook
   'sharad/enable-startup-inperrupting-feature-hook
   'sharad/desktop-session-restore)

  ;; Then type ‘M-x session-save’, or ‘M-x session-restore’ whenever you want to save or restore a desktop. Restored desktops are deleted from disk.

  ;;}}


  )



(deh-require-maybe savehist-20+
  )

(deh-require-maybe workspaces
  )

;;For Session
(deh-require-maybe session ;;
  (add-hook 'after-init-hook 'session-initialize)
  (add-hook 'kill-emacs-hook 'session-save-session)

  (setq session-initialize t)

  ;;{{ http://www.emacswiki.org/emacs/EmacsSession

  ;; There is a function in session that’s not really persistence
  ;; related – ‘session-jump-to-last-change’ <C-x C-/>. This is the
  ;; singular most useful function of any Emacs add-on to me. It moves
  ;; the point to the last modified location. Keep calling it and you
  ;; will visit all the locations you’ve made
  ;; modifications. Absolutely brilliant. Unobstrusive, unlike
  ;; highlight-changes-mode.

  ;; However, it doesn’t automatically reveal folded sections. Here is
  ;; the fix:


  ;; expanded folded secitons as required
  (defun le::maybe-reveal ()
    (when (and (or (memq major-mode  '(org-mode outline-mode))
                   (and (boundp 'outline-minor-mode)
                        outline-minor-mode))
               (outline-invisible-p))
      (if (eq major-mode 'org-mode)
          (org-reveal)
          (show-subtree))))

  (add-hook 'session-after-jump-to-last-change-hook
            'le::maybe-reveal)
  ;;}}
  ;;  (session-initialize))
  ;; Something like this is recommended to get emacs to shut-up
  ;; and never ask you for a coding system. Otherwise this can
  ;; happen on *every* desktop-save triggered by the auto-save-hook:
  (prefer-coding-system 'utf-8)




  (add-hook 'delete-frame-functions
            '(lambda (frame)
              (if (and
                   (< (length (frame-list)) 3)
                   (functionp 'session-save-sessoin))
                  (session-save-sessoin)))))


;; (deh-require-maybe desktop-recover
;;   ;; ssee:http://www.emacswiki.org/emacs/DesktopRecover
;;   ;; from: https://github.com/doomvox/desktop-recover/blob/master/desktop-recover-setup.el
;;   ;; And this brings up the interactive buffer restore menu
;;   (desktop-recover-interactive))

;; (deh-require-maybe desktopaid
(testing
  ;; see: http://desktopaid.sourceforge.net/
  (dta-hook-up))

;; (deh-require-maybe frame-restore
;;   ;; check this library will know what to do.
;;   ;; http://www.emacswiki.org/emacs/frame-restore.el
;;   )

(deh-require-maybe revive)

;; first test it with startup
;; (deh-require-maybe winner
;;   ;; see: http://emacs.wordpress.com/2007/01/28/simple-window-configuration-management/
;;   (winner-mode 1))

(deh-require-maybe tapestry
  ;; http://superuser.com/questions/383560/how-to-restore-emacs-windows-and-buffers-from-the-last-session
  (defvar my-tapestry-file "~/.tapestry")

  (defun load-my-tapestry ()
    (interactive)
    (let ((b (find-file-noselect my-tapestry-file)))
      (sit-for 0)
      (set-tapestry (read b))
      (kill-buffer b)))

  (defun save-my-tapestry ()
    (interactive)
    (let ((tap (tapestry)))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (setcar tap (make-list (length (car tap)) nil))
          (print tap)
          (write-region (point-min) (point-max) my-tapestry-file)))))

  (add-hook 'kill-emacs-hook 'save-my-tapestry))


(deh-section "emacs session management"
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Session-Management.html
  ;; {{
  ;; 39.17 Session Management

  ;; Emacs supports the X Session Management Protocol, which is used
  ;; to suspend and restart applications. In the X Window System, a
  ;; program called the session manager is responsible for keeping
  ;; track of the applications that are running. When the X server
  ;; shuts down, the session manager asks applications to save their
  ;; state, and delays the actual shutdown until they respond. An
  ;; application can also cancel the shutdown.

  ;; When the session manager restarts a suspended session, it directs
  ;; these applications to individually reload their saved state. It
  ;; does this by specifying a special command-line argument that says
  ;; what saved session to restore. For Emacs, this argument is
  ;; ‘--smid session’.  — Variable: emacs-save-session-functions

  ;; Emacs supports saving state via a hook called
  ;; emacs-save-session-functions. Emacs runs this hook when the
  ;; session manager tells it that the window system is shutting
  ;; down. The functions are called with no arguments, and with the
  ;; current buffer set to a temporary buffer. Each function can use
  ;; insert to add Lisp code to this buffer. At the end, Emacs saves
  ;; the buffer in a file, called the session file.

  ;; Subsequently, when the session manager restarts Emacs, it loads
  ;; the session file automatically (see Loading). This is performed
  ;; by a function named emacs-session-restore, which is called during
  ;; startup. See Startup Summary.

  ;; If a function in emacs-save-session-functions returns non-nil,
  ;; Emacs tells the session manager to cancel the shutdown.

  ;; Here is an example that just inserts some text into *scratch*
  ;; when Emacs is restarted by the session manager.

  ;; (add-hook 'emacs-save-session-functions 'save-yourself-test)

  (defun save-yourself-test ()
    (insert "(save-current-buffer
       (switch-to-buffer \"*scratch*\")
       (insert \"I am restored\"))")
    nil)

;; }}
  )

(provide 'session-config)
;;; session-config.el ends here
