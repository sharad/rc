;;; article-config.el --- Article related setting

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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


;; (create-image )


(defun gnus-article-mst-show-country ()
  ;; from http://dishevelled.net/elisp/gnus-mst-show-country.el
  (interactive)
  (let ((from (message-fetch-field "From" t)))
    (when from
      (let ((addr (car (ietf-drums-parse-address from))))
        (when addr
          (let* ((field (progn
                          (string-match "\\.\\(\\sw+\\)$" addr)
                          (match-string 1 addr)))
                 (country (tld-to-country field)))
            (when country
              (save-restriction
                (article-narrow-to-head)
                (goto-char (point-max))
                (insert (propertize (concat "X-Country: " country "\n")
                                    'face 'gnus-header-subject-face))
               ;; (previous-line 1)
                (line-move-1 1)
                (beginning-of-line)))))))))


;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus
;; Show the time since the article came in.
;; see http://www.gnu.org/software/emacs/manual/html_node/gnus/Customizing-Articles.html#Customizing-Articles
(setq
 gnus-treat-body-boundary 'head
 gnus-treat-date-lapsed 'head
 gnus-treat-display-x-face 'head
 gnus-treat-strip-cr 2
 gnus-treat-strip-leading-blank-lines t
 gnus-treat-strip-multiple-blank-lines t
 gnus-treat-strip-trailing-blank-lines t
 gnus-treat-unsplit-urls t

 gnus-treat-date-english 'head
 gnus-treat-date-iso8601 'head
 gnus-treat-date-lapsed 'head
 gnus-treat-date-local 'head
 gnus-treat-date-original 'head
 gnus-treat-date-user-defined 'head
 gnus-treat-date-ut 'head
 gnus-treat-date-original 'head
 ;; Make sure Gnus doesn't display smiley graphics.
 gnus-treat-display-smileys t
 gnus-treat-hide-boring-headers 'head
 gnus-treat-hide-signature nil ;; (unless (equal (system-name) office-host-name) 'last)
 gnus-treat-strip-banner t)

(setq gnus-article-date-lapsed-new-header t)

(add-hook 'gnus-part-display-hook 'gnus-article-date-lapsed)
(add-hook 'gnus-part-display-hook 'gnus-article-date-local)

(add-hook 'gnus-article-prepare-hook
          '(lambda ()
             ;; 	     (gnus-article-de-quoted-unreadable)
             (gnus-article-emphasize)
             (gnus-article-hide-boring-headers)
             (gnus-article-hide-headers-if-wanted)
             ;; 	     (gnus-article-hide-pgp)
             (gnus-article-highlight)
             (gnus-article-highlight-citation)
             (gnus-article-date-lapsed)
             (gnus-article-date-local)              ; will actually convert timestamp from other timezones to yours
             (gnus-article-strip-trailing-space)
             ;;              (gnus-article-fill-cited-article)
             ;; (with-selected-window (get-buffer-window gnus-article-buffer)
             ;;   (gnus-summary-goto-subject (cdr gnus-article-current)))
             ;; (select-window (get-buffer-window gnus-summary-buffer))
             ))

(unless (fboundp 'gnus-article-goto-subject)
  (defun gnus-article-goto-subject ()
    (interactive)
    (with-selected-window
        (get-buffer-window gnus-article-buffer)
      (gnus-summary-goto-subject
       (cdr gnus-article-current)))))


;;}}


;; gnus-visible-headers

;; "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:"


;;{{
(gnus-start-date-timer)

;; Start a timer to update the Date headers in the article buffers.
;; The numerical prefix says how frequently (in seconds) the function
;; is to run.
;;}}







;;{{
(defun article-show-attachment (&optional arg)
  "Hide the signature in the current article.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (gnus-show-attachment-arg))
  (unless (gnus-article-check-hidden-text 'signature arg)
    (save-excursion
      (save-restriction
	(let ((inhibit-read-only t))
	  (when (gnus-article-narrow-to-signature)
	    (gnus-article-hide-text-type
	     (point-min) (point-max) 'signature))))))
  (gnus-set-mode-line 'article))

(defun gnus-show-attachment-arg ()
  'head)

;; (defun article-show-attachment (&optional arg)
(defun article-show-attachment ()
  "Translate article using an online translation service."
  ;; (interactive (gnus-show-attachment-arg))
  (interactive)
  (gnus-with-article-buffer
    (when (article-goto-body)
      (let* ((start (point))
	     (end (point-max))
	     (orig (buffer-substring start end)))
	     ;; (trans (babel-as-string orig)))

             (insert "\nArticle has attachment\n")
	;; (save-restriction
	;;   (narrow-to-region start end)
	;;   (delete-region start end)
	;;   (insert trans))
        ))))


;;}}

(provide 'article-config)

;;; article-config.el ends here
