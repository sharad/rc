;;
;; attachment.el
;; Login : <s@taj>
;; Started on  Sun Jan  9 23:49:19 2011 Sharad Pratap
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

;; Code

;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus
;; Avoid "Here's an attachment oops I forget to attach it augh" embarrassment.
;; Taken from <http://ww.telent.net/diary/2003/1/>.
(defun check-attachments-attached ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* (
           ;; Nil when message came from outside (eg calling emacs as editor)
           ;; Non-nil marker of end of headers.
           (internal-messagep
            (re-search-forward
             (concat "^" (regexp-quote mail-header-separator) "$") nil t))
           (end-of-headers              ; Start of body.
            (copy-marker
             (or internal-messagep
                 (re-search-forward "^$" nil t)
                 (point-min))))
           (limit
            (or (re-search-forward "^-- $" nil t)
                (point-max)))
           (old-case-fold-search case-fold-search))
      (unwind-protect
          (progn
            (goto-char end-of-headers)
            (when (search-forward "attach" limit t)
              (goto-char end-of-headers)
              ;; the word 'attach' has been used, can we find an
              ;; attachment?
              (unless
                  (or (re-search-forward "^<#/" limit t)
                      (re-search-forward "^<#/" nil t)
                      (y-or-n-p
                       "Found the word `attach' but no MIME attachment: send anyway? "
                      )
                     (error "Aborted send")))))
        (set-marker end-of-headers nil)))))

(add-hook 'message-send-hook 'check-attachments-attached)

;;}}

;;{{ from: http://www.emacswiki.org/emacs/GnusAndPine
(require 'message)

(defvar my-message-attachment-regexp
  "attach\\|\Wfiles?\W\\|enclose\\|\Wdraft\\|\Wversion")

(defun check-mail ()
  "ask for confirmation before sending a mail. Scan for possible attachment"
  (require 'message)
  (save-excursion
    (message-goto-body)
    (let ((warning ""))
      (when (and (search-forward-regexp my-message-attachment-regexp nil t nil)
                 (not (search-forward "<#part" nil t nil)))
        (setq warning "No attachment.\n"))
      (goto-char (point-min))
      (unless (message-y-or-n-p (concat warning "Send the message ? ") nil nil)
        (error "Message not sent")))))
  (add-hook 'message-send-hook 'check-mail)






(defun message-attach-all-files-from-folder(&optional disposition dir-to-attach)
  ;; from: http://www.emacswiki.org/emacs/MessageMode#toc5
  "create the mml code to attach all files found in a given directory"
  (interactive)

  (if (eq disposition nil)
      (setq disposition (completing-read "Enter default disposition to use: " '(("attachment" 1) ("inline" 2)) nil t)))

  (if (eq dir-to-attach nil)
      (setq dir-to-attach (read-directory-name "Select a folder to attach: ")))

  (if (not (string-match "/$" dir-to-attach))
      (setq dir-to-attach (concat dir-to-attach "/")))

  (dolist (file (directory-files dir-to-attach))
    (when (and (not (string= "." file)) (not (string= ".." file)))
      (let (full-file-path mime-type)
        (setq full-file-path (concat dir-to-attach file))
        (if (file-readable-p full-file-path)
            (if (file-directory-p full-file-path)
                (message-attach-all-files-from-folder disposition full-file-path)

                (setq mime-type (substring (shell-command-to-string (concat "file --mime-type --brief " (shell-quote-argument (expand-file-name full-file-path)))) 0 -1))
                (insert (concat "<#part type=\"" mime-type "\" filename=\"" full-file-path "\" disposition=" disposition ">\n"))))))))

;; (setq gnus-gcc-externalize-attachments 'all)
(setq gnus-gcc-externalize-attachments nil)
;    If nil, attach files as normal parts in Gcc copies; if a regexp
;    and matches the Gcc group name, attach files as external parts;
;    if it is all, attach local files as external parts; if it is
;    other non-nil, the behavior is the same as all, but it may be
;    changed in the future.
;;}}


(provide 'attachment-config)

;;}}
