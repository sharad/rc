;;
;; notification.el
;; Login : <s@taj>
;; Started on  Fri Nov 26 00:06:27 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;


;;{{ from: http://www.emacswiki.org/emacs/GnusNotify
;; I am using gnus-notify+

(defvar gnus-mst-display-new-messages "New Mails" "doc")
(defvar gnus-mst-notify-groups "*" "doc")

(when (xrequire 'gnus-notify)
  (setq gnus-mst-display-new-messages "New mails"
        gnus-mst-notify-groups
        (if (equal (system-name) "spratap")
            '("nnimap+localhost:Office.INBOX" "nnimap+localhost:Office.lists.info.india" "nnimap+localhost:Office.lists.info.india-misc")
            '("nnimap+localhost:nnimap+localhost:Gmail.INBOX"))))

(when nil                               ;guess this is the culprit
 (when (xrequire 'gnus-notify+)
  (defun gnus-notify+-around (orig-fun &rest args)
    (when gnus-current-startup-file
      (apply orig-fun args)))
  (advice-add 'gnus-notify+ :around #'gnus-notify+-around)
  ;; adding (modeline-notify t) to group for gnus-notify+
  (set-or-nconc gnus-parameters           ;check for set-or-nconc in macros.el
                `((,(mapconcat 'identity gnus-mst-notify-groups "\\|")
                    '(modeline-notify t)))) ;BUG here this modeline-notify must be causing trouble.

  ;; (add-hook 'gnus-summary-exit-hook 'gnus-notify+)
  ;; (add-hook 'gnus-group-catchup-group-hook 'gnus-notify+)
  ;; (add-hook 'mail-notify-pre-hook 'gnus-notify+)
  ))

;; (macroexpand `(set-or-nconc xgnus-parameters           ;check for set-or-nconc in macros.el
;;                             ( ,(mapconcat 'identity gnus-mst-notify-groups "|")
;;                                (modeline-notify t))))

;;}}

;;{{ from: http://www.emacswiki.org/emacs/GnusBiff
;; biff
(defvar foundnewmbox "")

(defun fmbiff ()
  (interactive)
  (save-excursion
    (with-current-buffer gnus-group-buffer
      ;; (set-buffer "*Group*")
      ; (beginning-of-buffer)
      (goto-char (point-min))
      (defvar foundanymbox nil)
      (cond ((re-search-forward "INBOX.ALL" nil t)
             (setq foundanymbox t))
            (t (setq foundanymbox nil)))
      ;; (set-buffer "*Group*")
      ; (beginning-of-buffer)
      (goto-char (point-min))
      (cond ((re-search-forward "0: INBOX.ALL" nil t)
             (setq foundnewmbox ""))
            (t (if foundanymbox (setq foundnewmbox "[M]")
                   (setq foundnewmbox ""))))
      (message nil))))

(unless (member 'foundnewmbox global-mode-string)
   (setq global-mode-string (append global-mode-string
                                    (list 'foundnewmbox))))

(add-hook 'gnus-after-getting-new-news-hook 'fmbiff)

;; How about:

(defvar mac-biff-lighter ""
  "Lighter used by `mac-biff-mode'.")

(defvar mac-biff-mail-re "\\([[:digit:]]+\\)"
  "Regular expression to match number counts in a Gnus buffer.")

(define-minor-mode mac-biff-mode
  "Minor mode to display state of new email."
  nil mac-biff-lighter nil
  (if mac-biff-mode
      (progn (add-hook 'gnus-after-getting-new-news-hook 'mac-biff-update)
             (add-hook 'gnus-exit-group-hook 'mac-biff-update)
             (mac-biff-update))
    (remove-hook 'gnus-after-getting-new-news-hook 'mac-biff-update)
    (remove-hook 'gnus-exit-group-hook 'mac-biff-update)))

(defun mac-biff-update1 ()
  "Read the mail count from Gnus."
  (let ((buffer (get-buffer "*Group*"))
        (count 0))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward mac-biff-mail-re nil t)
          (setq count (+ count (string-to-number (match-string 1)))))))
    (setq mac-biff-lighter (if (= count 0)
                               ""
                             (format " [%d]" count)))))
;;}}


;;{{ from: http://stackoverflow.com/questions/1053245/new-mail-notifications-in-gnus-for-emacs
(defun mac-biff-update ()
  "Read the mail count from Gnus."
  (let ((buffer (get-buffer "*Group*"))
        (count 0))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward mac-biff-mail-re nil t)
          (setq count (+ count (string-to-number (match-string 1)))))))
    (if (> count 0)
        (if (= 0 (shell-command
                  ;(format "/usr/local/bin/growlnotify -a Emacs.app -m 'You have %d new messages!'" count)))))
                  (format "zenity --question --text 'You have %d new messages!'" count)))
            (make-frame))
        )))

;; test
;; (if (= 0 (shell-command "zenity --question --text Hi"))
;;     (make-frame))



;;}}


;; {{ http://exal.0x2.org/conf/gnus.html
;;;
;;; Get messages automaticaly
;;;

;; (if (xrequire 'gnus-demon)
;;     (setq gnus-use-demon t)
;;     (gnus-demon-add-handler 'gnus-group-get-new-news 3 2)
;;     (gnus-demon-init))

(when (xrequire 'gnus-demon)

  ;; Group Level
  ;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Levels.html
  ;; http://www.emacswiki.org/emacs/GnusNiftyTricks
  ;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Parameters.html
  ;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Scanning-New-Messages.html
  ;; http://www.emacswiki.org/emacs/GnusRss
  ;; http://www.emacswiki.org/emacs/GnusDemon



  (when nil ;; old




    (defun gnus-demon-scan-mail-or-news-and-update ()
      "Scan for new mail/news and update the *Group* buffer"
      (when (gnus-alive-p)
        (save-window-excursion
          (save-excursion
            (with-current-buffer gnus-group-buffer
              ;; (set-buffer gnus-group-buffer)
              (gnus-group-get-new-news)
              (message nil))))))

    (defun gnus-demon-scan-and-update ()
      (gnus-demon-scan-mail-or-news-and-update)))



  ;; (defvar gnus-scan-man-idle-timer
  ;;   (progn                                ; Cancel to prevent duplication.
  ;;     (when (boundp 'gnus-scan-man-idle-timer) (cancel-timer gnus-scan-man-idle-timer))
  ;;     (run-with-idle-timer gnus-scan-man-idle-interval nil 'hl-line-highlight-now))
  ;;   "Timer used to turn on `global-hl-line-mode' whenever Emacs is idle.")

  (setq gnus-use-demon t)

  ;; not being used
  ;; (defun gnus-demon-scan-mail-and-news ()
  ;;   (cancel-timer
  ;;    (run-with-idle-timer 6 nil 'gnus-demon-scan-mail-and-news-now)))

  (defun gnus-demon-scan-mail-and-news-now (&optional level)
    "Scan for new mail/news and update the *Group* buffer."
    (let ((level (or level 3))
          (time-format "%a %H:%M:%S"))
      (message "gnus-demon-scan-mail-and-news-now: current time %s, idle time %d"
               (format-time-string time-format (current-time))
               (float-time (current-idle-time)))
      (when (gnus-alive-p)
        (message "gnus-demon-scan-mail-and-news-now %d" level)
        (save-window-excursion
          (save-excursion
            (with-current-buffer gnus-group-buffer
              ;; (set-buffer gnus-group-buffer)
              (let ((idle-time (current-idle-time)))
                (if (> (float-time idle-time) 7)
                    (with-timeout (3 (message "gnus demon timeout"))
                                  (gnus-group-get-new-news level))
                    (message "not running gnus demon")))
              (message nil)))))))

  (add-hook 'gnus-group-mode-hook 'gnus-demon-init)
  (add-hook 'gnus-exit-gnus-hook 'gnus-demon-cancel)

  ;; Sort threads by the date of the root node.
  (setq gnus-thread-sort-functions `(gnus-thread-sort-by-date))
  ;; Initialize the Gnus daemon, check new mail every six minutes.
  ;; (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news 1 nil))
  ;; (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news-now 2 nil)
  (gnus-demon-add-handler 'gnus-demon-scan-mail-and-news-now 10 10)

  (gnus-demon-add-handler '(lambda () (gnus-demon-scan-mail-and-news-now 6)) 22 20))

;;}}


(provide 'notification-config)
