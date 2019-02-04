;;; summary-config.el --- Summary relate functions, summary format etc.

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



;; x-bugzilla-target-milestone: 6.1.0.0
;; x-bugzilla-url: https://bugzilla/
;; x-bugzilla-changed-fields: Status CC
;; x-bugzilla-assigned-to:
;; x-bugzilla-reason: CC
;; auto-submitted: auto-generated
;; x-bugzilla-type: changed
;; x-bugzilla-watch-reason: None
;; x-bugzilla-product:
;; x-bugzilla-component:
;; x-bugzilla-keywords:
;; x-bugzilla-severity: major
;; x-bugzilla-who:
;; x-bugzilla-status: VERIFIED
;; x-bugzilla-priority: P1

(require 'nnheader)

;;{{http://eschulte.github.com/emacs-starter-kit/starter-kit-gnus.html
;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(when window-system
  (setq
   gnus-sum-thread-tree-indent "  "
   gnus-sum-thread-tree-root "● "
   gnus-sum-thread-tree-false-root "◯ "
   gnus-sum-thread-tree-single-indent "◎ "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-leaf-with-other "├─► "
   gnus-sum-thread-tree-single-leaf     "╰─► "))

;;}}

(deh-section "summary line format user functions"
  (defun sdfsdgfdsgdfg-gnus-user-format-function-b (header)
    (let ((descr
           ;; (assq 'x-bugzilla-who (mail-header-extra header))))
           (or
            (gnus-extra-header 'x-bugzilla-who header)
            (gnus-extra-header 'X-Bugzilla-Who header))))
      ;; (if descr (cdr descr) "bugzilla")))
      (if descr descr "bugzilla")))


  (defun gnus-user-format-function-atch (header)
    ;; http://osdir.com/ml/emacs.gnus.user/2006-08/msg00011.html
    "Display @ for message with attachment in summary line.

You need to add `Content-Type' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'."
    (let ((case-fold-search t)
          (ctype (or (cdr (assq 'Content-Type (mail-header-extra header)))
                     "text/plain"))
          indicator)
      (when (string-match "^multipart/mixed" ctype)
        (setq indicator "@"))
      (if indicator
          indicator
          " ")))



  (deh-require-maybe rs-gnus-summary
  ;; Setup all:
    ;; (rs-gnus-summary-line-initialize)

    ;; Usage for the format functions:

    (rs-gnus-summary-tree-arrows-01)

    ;; Usage for the balloon face:

    (deh-require-maybe gnus-summary-stripe
      (setq gnus-summary-stripe-regexp "^.+│.+│.+│"))


    (defun my-gnus-summary-line-initialize ()
      "Setup my summary line."
      (interactive)
      ;; Alias for the content-type function:
    (defalias 'gnus-user-format-function-ct 'rs-gnus-summary-line-content-type)
    ;; Alias for the size function:
    (defalias 'gnus-user-format-function-size 'rs-gnus-summary-line-message-size)
    ;; Alias for the score function:
    (defalias 'gnus-user-format-function-score 'rs-gnus-summary-line-score)
    ;;
    (defalias 'gnus-user-format-function-label 'rs-gnus-summary-line-label)
    ;;
    ;; Use them:
    (setq gnus-balloon-face-0 'rs-gnus-balloon-0)
    (setq gnus-balloon-face-1 'rs-gnus-balloon-1)
    ;; Unbold face for UTF arrows: (FIXME: Doesn't work on marauder.)
    (copy-face 'default 'rs-gnus-face-1)
    (setq gnus-face-1 'rs-gnus-face-1)
    ;; (set-face-italic-p 'rs-gnus-face-1 nil)
    ;; (dolist (el '(gnus-summary-low-ancient-face
    ;; 		gnus-summary-low-read-face
    ;; 		gnus-summary-low-ticked-face
    ;; 		gnus-summary-low-undownloaded-face
    ;; 		gnus-summary-low-unread-face))
    ;;   (message "%s" el)
    ;;   (set-face-italic-p el nil)
    ;;   (set-face-bold-p el nil)
    ;;   (sit-for 1))
    (if (or (not window-system)
            (string-match "marauder\\|siogo" system-name))
        (rs-gnus-summary-tree-arrows-latin)
        (rs-gnus-summary-tree-arrows))
    ;; Set line format:
    (setq gnus-summary-line-format
          "%«%U%R%u&score;%u&ct; %4u&size;%»%* %(%-20,20f%) %1«%1{%B %}%s%»\n"))


  ))


(setq rs-gnus-summary-line-content-type-alist
      '(("^text/plain"             " ")
        ("^text/html"              "h")
        ("^message/rfc822"         "f") ;; forwarded
        ("^multipart/mixed"        "m")
        ("^multipart/alternative"  "a")
        ("^multipart/related"      "r")
        ("^multipart/signed"       "s")
        ("^multipart/encrypted"    "e")
        ("^multipart/report"       "t")
        ("^application/"           "A")
        ("^image/"                 "I")))

(deh-section "summary line formats"



  (defvar lotus-gnus/global-summry-line-format   nil "")
  (defvar lotus-gnus/bugzilla-summry-line-format nil "")
  (defvar lotus-gnus/sent-summry-line-format     nil "")


  ;; Alias for the content-type function:
  (defalias 'gnus-user-format-function-ct 'rs-gnus-summary-line-content-type)
  ;;   You need to add `Content-Type' to `nnmail-extra-headers' and
  ;; `gnus-extra-headers', see Info node `(gnus)To From Newsgroups'."

  ;; Alias for the size function:
  (defalias 'gnus-user-format-function-size 'rs-gnus-summary-line-message-size)
  ;; Alias for the score function:
  (defalias 'gnus-user-format-function-score 'rs-gnus-summary-line-score)
  ;;
  (defalias 'gnus-user-format-function-label 'rs-gnus-summary-line-label)
  ;;
  ;; Use them:
  (setq gnus-balloon-face-0 'rs-gnus-balloon-0
        gnus-balloon-face-1 'rs-gnus-balloon-1
        gnus-face-1         'rs-gnus-face-1)
  ;; Unbold face for UTF arrows: (FIXME: Doesn't work on marauder.)
  (copy-face 'default 'rs-gnus-face-1)
  (let* (;;(marks "%0{%«%U%R%z %u&score;%u&ct; %4u&size;%»%}")
         ;; (marks "%0«%U%R%z%u&atch;%u&score;%u&ct;%4u&size;%»")
         (marks "%0«%U%R%z%u&atch;%u&score;%u&ct;%4k%»")
         ;; (marks "%0{%U%R%z%}")
         ;; (attachment "%0{%@%}")
         (pipe "%3{│%}")
         ;; (date  (concat pipe "%1{%d%}" pipe))
         (date  (concat pipe "%1{%&user-date;%}" pipe))
         (lines " %1{%-4L%}: ")
         (from "%4{%-20,20f%}")
         (thread-mark "%1{%B%}")
         (subject "%s")
         (sp " ")
         (nl "\n")
                                        ;(bugzilla-who "%4{%-20,20ub%}")
         )
    (setq
     lotus-gnus/global-summry-line-format   (concat marks date lines from sp pipe sp thread-mark subject nl)
     lotus-gnus/bugzilla-summry-line-format (concat marks date lines from sp pipe sp thread-mark subject nl)
     lotus-gnus/sent-summry-line-format     (concat marks date lines from sp pipe sp thread-mark subject nl)))

  ;; lotus-gnus/bugzilla-summry-line-format (concat attachment marks date lines bugzilla-who sp pipe sp thread-mark subject nl)


  ;; With a custom date format :
  ;; affichage de la date en relatif

  ;; gnus-user-date-format-alist '((t . "%Y-%b-%d %H:%M"))

  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . " %k:%M") ;dans la journée = 14:39
          ((+ 86400 (gnus-seconds-today)) . "hier %k:%M")
                                        ;hier = hier 14:39
          ((+ 604800 (gnus-seconds-today)) . "%a %k:%M")
                                        ;dans la semaine = sam 14:39
          ((gnus-seconds-month) . "%a %d") ;ce mois = sam 28
          ((gnus-seconds-year) . "%b %d") ;durant l'année = mai 28
          (t . "%b %d '%y"))) ;le reste = mai 28 '05

  )

;; ;; Specify the order of the header lines
;; (setq gnus-sorted-header-list '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^Followup-To:" "^To:" "^Cc:" "^Date:" "^User-Agent:" "^X-Mailer:" "^X-Newsreader:"))





;; (concat
;;  "%0{%U%R%z%}"
;;  "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
;;                                         ;" %L: "
;;  " %1{%4,-4L%}: "
;;                                         ;"  "
;;  "%4{%-20,20f%}"               ;; name
;;  "  "
;;  "%3{│%}"
;;  " "
;;  "%1{%B%}"
;;  "%s\n")







(provide 'summary-config)
