;;; group-config.el --- GNUS group related behaviours.

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

(require 'host-info)
(require 'common-info)
(require 'passwds)

;; stolen from:
;; http://linil.wordpress.com/2008/01/18/gnus-gmail
(setq
 gnus-invalid-group-regexp "[:`'\"]\\|^$"
 ;; gnus-group-sort-function gnus-thread-sort-functions
 )


(setq gnus-permanently-visible-groups ".*INBOX")

        ;; "^nnimap+localhost:Office\\.INBOX\\|^nnimap+localhost:Office\\.sent-mail$"
        ;; "^nnimap+localhost:Gmail\\.INBOX\\|^nnimap+localhost:Gmail\\.sent-mail$")))


;;{{ Group setting
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;;}}

;;{{

;; (setq gnus-message-archive-group        ;even I have handled it in gnus-posting-style
;;       `((if (message-news-p)
;;             "sent-news"
;;             ,(if (equal (system-name) office-host-name)
;;                  "Office.Sent Items"
;;                  "sent"))))

(setq
 gnus-message-archive-method '(nnimap "localhost")
 gnus-message-archive-group        ;even I have handled it in gnus-posting-style
 #'(lambda (group)
     (append
      (list "sent")
      (if (message-news-p)
          '("sent-news")
        `("sent-mail"
          ,@(if (member (system-name) office-host-names)
                '("Office.Meru.Sent Items" "Office.Fortinet.Sent Items"))))
      (list (format-time-string "sent.%Y-%m")))))


;; http://www.gnus.org/manual/gnus_153.html
(setq gnus-gcc-mark-as-read t)
;    If non-nil, automatically mark Gcc articles as read.

;; (setq gnus-gcc-externalize-attachments 'all)
(setq gnus-gcc-externalize-attachments nil)
;    If nil, attach files as normal parts in Gcc copies; if a regexp
;    and matches the Gcc group name, attach files as external parts;
;    if it is all, attach local files as external parts; if it is
;    other non-nil, the behavior is the same as all, but it may be
;    changed in the future.

;;}}

;; I keep hitting "b" by mistake in the group view, and it messes things up.
(define-key gnus-group-mode-map "b" 'gnus-group-get-new-news)


;;{{Face http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_2.html#SEC20
;; 2.1.3 Group Highlighting

;; Highlighting in the group buffer is controlled by the
;; gnus-group-highlight variable. This is an alist with elements that
;; look like (form . face). If form evaluates to something non-nil,
;; the face will be used on the line.

;; Here's an example value for this variable that might look nice if the background is dark:


  (when t
    (face-spec-set 'my-group-face-1
                   '((t (:foreground "Red" :bold t))))
    (face-spec-set 'my-group-face-2
                   '((t (:foreground "SeaGreen" :bold t))))
    (face-spec-set 'my-group-face-3
                   '((t (:foreground "SpringGreen" :bold t))))
    (face-spec-set 'my-group-face-4
                   '((t (:foreground "SteelBlue" :bold t))))
    (face-spec-set 'my-group-face-5
                   '((t (:foreground "SkyBlue" :bold t))))

    (setq gnus-group-highlight
          '(((> unread 200) . my-group-face-1)
            ((and (< level 3) (zerop unread)) . my-group-face-2)
            ((< level 3) . my-group-face-3)
            ((zerop unread) . my-group-face-4)
            (t . my-group-face-5))))

;; Also see section 8.6 Faces and Fonts.

;; Variables that are dynamically bound when the forms are evaluated include:

;; group
;;     The group name.
;; unread
;;     The number of unread articles in the group.
;; method
;;     The select method.
;; mailp
;;     Whether the group is a mail group.
;; level
;;     The level of the group.
;; score
;;     The score of the group.
;; ticked
;;     The number of ticked articles in the group.
;; total
;;     The total number of articles in the group. Or rather, MAX-NUMBER minus MIN-NUMBER plus one.
;; topic
;;     When using the topic minor mode, this variable is bound to the current topic being inserted.

;; When the forms are evaled, point is at the beginning of the line of
;; the group in question, so you can use many of the normal Gnus
;; functions for snarfing info on the group.

;; gnus-group-update-hook is called when a group line is changed. It
;; will not be called when gnus-visual is nil. This hook calls
;; gnus-group-highlight-line by default.

;;}}



;;{{
;http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_2.html#SEC41
;;  2.17.3 Group Timestamp
;; It can be convenient to let Gnus keep track of when you last read a group. To set the ball rolling, you should add gnus-group-set-timestamp to gnus-select-group-hook:

;; (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; After doing this, each time you enter a group, it'll be recorded.
;; This information can be displayed in various ways--the easiest is to use the `%d' spec in the group line format:

;; (setq gnus-group-line-format
;;       "%M\%S\%p\%P\%5y: %(%-40,40g%) %d\n")

;; This will result in lines looking like:

;; *        0: mail.ding                                19961002T012943
;;          0: custom                                   19961002T012713

;; As you can see, the date is displayed in compact ISO 8601 format. This may be a bit too much, so to just display the date, you could say something like:

;; (setq gnus-group-line-format
;;       "%M\%S\%p\%P\%5y: %(%-40,40g%) %6,6~(cut 2)d\n")

(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(setq gnus-group-line-format
      ;"%M\%S\%p\%P\%5y: %(%-40,40g%) %d\n")
      "%M\%S\%p\%P\%5y: %(%-100,100g%) %6,6~(cut 2)d\n")

;;}}

;;{{ http://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Parameters.html
(deh-section "GNUS Group Parameters."
  (when (require 'summary-config)
    (setq gnus-parameters
          `(
            (".*"
             (gnus-summary-line-format ,lotus-gnus/global-summry-line-format)
             (gnus-summary-display-arrow t)
             (gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")
             (gnus-article-sort-functions '(gnus-article-sort-by-date gnus-article-sort-by-score)))
                                        ;"Gnus: %g [%A] %Z"

            ("nnimap.*\\.bugs"
             (gnus-summary-line-format ,lotus-gnus/bugzilla-summry-line-format))

            ("nnimap.*\\.sent-mail\\|.*sent"
             (gnus-summary-line-format ,lotus-gnus/sent-summry-line-format)
             (gnus-summary-display-arrow t)
             (gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")
                                        ;"Gnus: %g [%A] %Z"
             (gnus-extra-headers '(To Newsgroups X-Newsreader))
             (gnus-ignored-from-addresses "Sharad Pratap\\|sh4r4d.*\\|spratap.*"))
            ("nnshimbun.*"
             (encapsulate-images t))))))

(setq nnshimbun-group-parameters-alist
 '(
   ("^nnshimbun.*:" index-range all prefetch-articles off
    encapsulate-images on expiry-wait 6)))

;; ("mail\\..*"
;;  (gnus-show-threads nil)
;;  (gnus-use-scoring nil)
;;  (gnus-summary-line-format
;;   "%U%R%z%I%(%[%d:%ub%-23,23f%]%) %s\n")
;;  (gcc-self . t)
;;  (display . all))

;;  ("^nnimap:\\(foo.bar\\)$"
;;  (to-group . "\\1"))

;; ("mail\\.me"
;;  (gnus-use-scoring  t))

;; ("list\\..*"
;;  (total-expire . t)
;;  (broken-reply-to . t))


;;}}

(provide 'group-config)
;;; group-config.el ends here
