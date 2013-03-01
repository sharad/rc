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

(deh-section "session per frames"
  ;;{{ http://stackoverflow.com/a/13711234
  ;; from: http://stackoverflow.com/questions/847962/what-alternate-session-managers-are-available-for-emacs
  (defvar emacs-frame-session-directory
    "~/.emacs.d/session/frames/"
    "The directory where the emacs configuration files are stored.")

  (defvar elscreen-tab-configuration-store-filename
    "elscreen"
    "The file where the elscreen tab configuration is stored.")

  (defun elscreen-store (frame-id)
    "Store the elscreen tab configuration."
    (interactive "ssession-name:")
    (let* ((session-dir (concat emacs-frame-session-directory (or
                                                               frame-id
                                                               (completing-read "session name: " nil))))
           (elscreen-session (concat session-dir "/" elscreen-tab-configuration-store-filename) ))
      (if (progn
            (make-directory session-dir t)
            (desktop-save session-dir))
          (with-temp-file elscreen-session
            (insert (prin1-to-string (elscreen-get-screen-to-name-alist)))))))

  ;; (push #'elscreen-store kill-emacs-hook)

  (defun elscreen-restore (frame-id)
    (interactive "ssession-name:")
    "Restore the elscreen tab configuration."
    (interactive)
    (let* ((session-dir (concat emacs-frame-session-directory (or
                                                               frame-id
                                                               (completing-read "session name: " nil))))
           (elscreen-session (concat session-dir "/" elscreen-tab-configuration-store-filename))
           (desktop-load-locked-desktop t))
      (if (file-directory-p session-dir)
          (if (eq (type-of (desktop-read session-dir)) 'symbol)
              (let ((screens (reverse
                              (read
                               (with-temp-buffer
                                 (insert-file-contents elscreen-session)
                                 (buffer-string))))))
                (while screens
                  (setq screen (car (car screens)))
                  (setq buffers (split-string (cdr (car screens)) ":"))
                  (if (eq screen 0)
                      (switch-to-buffer (car buffers))
                      (elscreen-find-and-goto-by-buffer (car buffers) t t))
                  (while (cdr buffers)
                    (switch-to-buffer-other-window (car (cdr buffers)))
                    (setq buffers (cdr buffers)))
                  (setq screens (cdr screens)))))
          (message "no such %s dir exists." session-dir))))

  ;; (elscreen-restore)
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
  )
;;  (session-initialize))


;; (deh-require-maybe desktop
(testing
  ;; http://stackoverflow.com/questions/2703743/restore-emacs-session-desktop
  (desktop-save-mode 1)
  ;; (desktop-read)

  ;; from: http://www.emacswiki.org/emacs/DeskTop
  ;; You can add any extra variables you want saved across sessions to the list ‘desktop-globals-to-save’. For example:
    ;; (setq history-length 250)
    ;; (add-to-list 'desktop-globals-to-save 'file-name-history)

  ;; Specifying Files Not to be Opened

  ;; You can specify buffers which should not be saved, by name or by mode, e.g.:

  ;; (setq desktop-buffers-not-to-save
  ;;       (concat "\\("
  ;;               "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
  ;;               "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
  ;;               "\\)$"))
  ;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  ;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  ;; (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  ;; (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


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


  (defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'my-desktop-save)



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
(setq desktop-base-file-name "emacs-desktop")

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
	  '(lambda ()
	     ;; desktop-remove clears desktop-dirname
	     (setq desktop-dirname-tmp desktop-dirname)
	     (desktop-remove)
	     (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
	  (desktop-save-in-desktop-dir)
	(message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
	  '(lambda ()
	     (if (saved-session)
		 (if (y-or-n-p "Restore desktop? ")
		     (session-restore)))))

;; Then type ‘M-x session-save’, or ‘M-x session-restore’ whenever you want to save or restore a desktop. Restored desktops are deleted from disk.

  ;;}}


  )

;; Something like this is recommended to get emacs to shut-up
;; and never ask you for a coding system. Otherwise this can
;; happen on *every* desktop-save triggered by the auto-save-hook:
(prefer-coding-system 'utf-8)


;; (deh-require-maybe desktop-recover
;;   ;; ssee:http://www.emacswiki.org/emacs/DesktopRecover
;;   ;; from: https://github.com/doomvox/desktop-recover/blob/master/desktop-recover-setup.el
;;   ;; And this brings up the interactive buffer restore menu
;;   (desktop-recover-interactive))

;; (deh-require-maybe desktopaid
(testing
  ;; see: http://desktopaid.sourceforge.net/
  (dta-hook-up))

(deh-require-maybe frame-restore
  ;; http://www.emacswiki.org/emacs/frame-restore.el
  )

(deh-require-maybe revive)

(deh-require-maybe winner
  ;; see: http://emacs.wordpress.com/2007/01/28/simple-window-configuration-management/
   (winner-mode 1))

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

     (add-hook 'emacs-save-session-functions 'save-yourself-test)

     (defun save-yourself-test ()
       (insert "(save-current-buffer
       (switch-to-buffer \"*scratch*\")
       (insert \"I am restored\"))")
       nil)

;; }}
)

(provide 'session-config)
;;; session-config.el ends here
