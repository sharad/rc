;;
;; mailto.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Thu Nov 25 16:56:09 2010 Sharad Pratap
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


;; from: http://journal.wjsullivan.net/185095.html

;; (defun johnsu01/mailto (url)
;;   "Follow a mailto URL as passed from Iceweasel, prompting for a posting style."
;;   (let ((gnus-newsgroup-name
;;          (completing-read "Use posting style of group: "
;;                           gnus-active-hashtb nil
;;                           (gnus-read-active-file-p))))
;;     (setq url (url-unhex-string url))
;;     (browse-url-mail url))
;;   ;; message-mail does not do anything with the body argument, so we have to.
;;   (if (string-match (regexp-quote "?") url)
;;       (let* ((start (match-end 0))
;;              (args (url-parse-query-string
;;                     (substring url start nil)))
;;              (body (cadr (assoc-string "body" args t))))
;;         (when body
;;           (switch-to-buffer (car (message-buffers)))
;;           (save-excursion
;;             (message-goto-body)
;;             (insert body))))))

(setq mail-user-agent                   ;see Chvmail-user-agent
      'gnus-user-agent)




;; (defun gnus-summary-mail-other-window (&optional arg)
;;   "Start composing a mail in another window.
;; Use the posting of the current group by default.
;; If ARG, don't do that.  If ARG is 1, prompt for group name to find the
;; posting style."
;;   (interactive "P")
;;   ;; We can't `let' gnus-newsgroup-name here, since that leads
;;   ;; to local variables leaking.
;;   (let ((group gnus-newsgroup-name)
;; 	;; make sure last viewed article doesn't affect posting styles:
;; 	(gnus-article-copy)
;; 	(buffer (current-buffer)))
;;     (unwind-protect
;; 	(progn
;; 	  (setq gnus-newsgroup-name
;; 		(if arg
;; 		    (if (= 1 (prefix-numeric-value arg))
;; 			(gnus-group-completing-read "Use group: "
;; 						    nil nil
;; 						    (gnus-read-active-file-p))
;; 		      "")
;; 		  gnus-newsgroup-name))
;; 	  ;; #### see comment in gnus-setup-message -- drv
;; 	  (gnus-setup-message 'message (message-mail)))
;;       (with-current-buffer buffer
;; 	(setq gnus-newsgroup-name group)))))

(defun johnsu01/mailto (url)
  "Follow a mailto URL as passed from Iceweasel, prompting for a posting style."
  (let ((gnus-newsgroup-name
         (completing-read "Use posting style of group: "
                          gnus-active-hashtb nil
                          (gnus-read-active-file-p))))
    (setq url (url-unhex-string url))
    ;; (browse-url-mail url)
    (gnus-url-mailto url))
  ;; message-mail does not do anything with the body argument, so we have to.
  (if (string-match (regexp-quote "?") url)
      (let* ((start (match-end 0))
             (args (url-parse-query-string
                    (substring url start nil)))
             (body (cadr (assoc-string "body" args t))))
        (when body
          (switch-to-buffer (car (message-buffers)))
          (save-excursion
            (message-goto-body)
            (insert body))))))

(provide 'mailto-config)
;; browse-url-mail
