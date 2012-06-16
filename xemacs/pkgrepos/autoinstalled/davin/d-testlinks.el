;;; d-testlinks.el --- An automated internal hyperlink checker

;; Copyright (C) 2006-2011 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: Hyperlink checker
;; Version: 1.0

;;; Limitation of Warranty

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install Instructions:

;; See the following URL for the latest info and a tarball:

;; http://davin.50webs.com/research/2010/mopa2e2.html#d-testlinks

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'd-testlinks)

;;; Known Bugs:

;; certain hyperlinks are hard wired into my website.
;; 
;; http://davin.50webs.com
;; 
;; but this should not stop users from finding this code useful.
;; For example the above URL could be changed to suit your own
;; website.

;;; (setq first "b.html")
;;; (setq second "foo")
;;; (testlinks--find-name "b.html" "foo")
;;;
(defun testlinks--find-name (first second)
  (if (and (not (string= first ""))
           (file-exists-p first)
           (string-match "\\.hHtTmMlL?$" first))
      (save-excursion
        (let* ((is-editing (d-currently-editing-file first))
               (is-readonly nil))

          (find-file first)
          (setq is-readonly buffer-read-only)
          (setq buffer-read-only t)
          (setq second (regexp-quote second))

          (let ((result (cond ((save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward (concat "name=" second " \r\n\t>") nil t))
                               t)
                              ((save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward (concat "name='" second "'") nil t))
                               t)
                              ((save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward (concat "name=\"" second "\"") nil t))
                               t)
                              (t
                               nil))))

            (if is-editing
                (setq buffer-read-only is-readonly)
              (kill-buffer nil))
            result)))))

(defun testlinks--search-for-url-regexp (re)

  ;;(message "buffer=%s cq1" (buffer-name))

  (if testlinks--verbose
      (save-excursion
        (set-buffer testlinks--lbuf)
        (insert "searching for re: " re "\n")))

  (let ((case-fold-search t)) ; lexical scoping should make this unecessary...
    (goto-char (point-min))
    (while (re-search-forward re nil t)

      (let (s)

        (setq s (buffer-substring-no-properties (match-beginning 1) (match-end 1)))

        (if testlinks--verbose
            (save-excursion
              (set-buffer testlinks--lbuf)
              (insert "testing link: " s "\n")))

        (when (string-match "^file:/+\\(.*\\)" s)
          (setq s (substring s (match-beginning 1) (match-end 1))))

        (when (string-match "^http://davin.50webs.com/\\(.*\\)" s)
          (setq s (concat "d:/home/hairy-lemon/output/50webs-com/"
                          (substring s (match-beginning 1) (match-end 1))))
          ;;(save-excursion (set-buffer testlinks--lbuf) (insert "**** munged=" s "\n"))
          )

        (if (or (string-match "^http:" s)
                (string-match "^https:" s)
                (string-match "^javascript:" s)
                (string-match "^mailto:" s))
            (progn
              ;; NOTE: do nothing
              )
          (if (string-match "^\\(^#*\\)#\\(^#*\\)$" s)
              (let (first second)
                (setq first (substring s (match-beginning 1) (match-end 1)))
                (setq second (substring s (match-beginning 2) (match-end 2)))

                ;;(message "first=%s" first)
                ;;(message "second=%s" second)

                (if (string= first "")
                    (setq first (buffer-file-name)))

                (if (not (testlinks--find-name first second))
                    (save-excursion
                      (setq line (d-what-line))
                      (set-buffer testlinks--lbuf)
                      (assert (boundp 'testlinks--bname))
                      (insert testlinks--bname ":" (format "%d" line) ": Error=" s "\n")
                      )))
            (if (or (string= s "")
                    (not (file-exists-p s)))
                (save-excursion
                  (setq line (d-what-line))
                  (set-buffer testlinks--lbuf)
                  (assert (boundp 'testlinks--bname))
                  (insert testlinks--bname ":" (format "%d" line) ": Error=" s "\n")
                  ))))))))

;; (insert "sdff-" 123 "-foo")
;; (insert "sdff-" (format "%d" 123) "-foo")

(defun testlinks--search-for-regexp (re cfs)
  (let ((case-fold-search cfs)
        (name (buffer-file-name)))
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (save-excursion
        (setq line (d-what-line))
        (set-buffer testlinks--lbuf)
        (insert testlinks--bname ":" (format "%d" line) ": Regexp Error=" re ", file=" name "\n"))
      ;;(message "buffer=%s" (buffer-name))
      ;;(debug)
      )))

(defvar testlinks--bufname "*testlinks*")

(defvar testlinks--verbose nil)

;; (testlinks-inner "~/hairy-lemon/output")
(defun testlinks-inner (dir &optional currently-recursing)

  ;; make sure it ends with a slash!
  (if (not (string= "/" (substring dir -1)))
      (setq dir (concat dir "/")))

  (if (not currently-recursing)
      (progn
        (if (get-buffer testlinks--bufname)
            (kill-buffer testlinks--bufname))
        (save-excursion
          (set-buffer (generate-new-buffer testlinks--bufname))
          (compilation-mode)
          (toggle-read-only -1)
          )))

  (setq testlinks--lbuf (or (get-buffer testlinks--bufname) (generate-new-buffer testlinks--bufname)))

  (set-buffer testlinks--lbuf)
  (insert "* called testlinks with args dir=" dir ", and currently-recursing=" (prin1-to-string currently-recursing) "\n")
  (if (not currently-recursing)
      (insert "* \n"))

  (let* ((list (directory-files-no-dotdotdot dir))
         (ptr  list))

    ;;(insert (concat "*** going to recurse on list: " (prin1-to-string ptr) "\n"))
    (while ptr
      (setq subdir (concat dir (car ptr)))
      ;;(insert "** testing for subdir: " subdir "\n")
      (if (file-directory-p subdir)
          (progn
            ;;(insert "*** test succeeded!\n")
            (testlinks-inner subdir t))
        ;;(insert "** test failed!\n")
        )
      (setq ptr (cdr ptr))))

  ;; (cons '("\\.hHtTmLlL?$" . fundamental-mode) auto-mode-alist)
  (let* ((list             (directory-files dir t ".*\\.\\(pPhHpP\\|hHtTmMlL?\\)$" t))
         (ptr              list)
         (case-fold-search t)
         ;; COOL! temporarily disables HTML mode!
         (auto-mode-alist  nil))
    (while ptr

      (let* ((is-editing (d-currently-editing-file (car ptr)))
             (is-readonly nil))

        (find-file (car ptr))
        (setq is-readonly buffer-read-only)
        (setq buffer-read-only t)

        (setq testlinks--bname (buffer-file-name))
        ;;(message "Visiting file: %s" testlinks--bname)

        (if testlinks--verbose
            (save-excursion
              (set-buffer testlinks--lbuf)
              (insert "visiting file: '" testlinks--bname "'\n")))

        ;; NOTE: don't need to smeg         ;; NOTE: don't need to smeg <!-- --> since they can appear inside <...> tags
        ;;
        (testlinks--search-for-url-regexp "<a \t\r\n+href=\\(^\"'>^ >*\\) \t\r\n>")
        (testlinks--search-for-url-regexp "<a \t\r\n+href=\"\\(^\"+\\)\"")
        (testlinks--search-for-url-regexp "<a \t\r\n+href='\\(^'+\\)'")
        (testlinks--search-for-url-regexp "<img \t\r\n+src=\\(^\"'>^ >+\\) \t\r\n>")
        (testlinks--search-for-url-regexp "<img \t\r\n+src=\"\\(^\"+\\)\"")
        (testlinks--search-for-url-regexp "<img \t\r\n+src='\\(^'+\\)'")
        (testlinks--search-for-url-regexp "<link rel=\"^\"+\" href=\"\\(^\"+\\)\"")

        (when prefs-home-emacs
          ;;(message "buffer=%s" (buffer-name))
          ;;(debug)
          (testlinks--search-for-regexp "SECT_" nil)
          (testlinks--search-for-regexp "QEST_" nil)
          (testlinks--search-for-regexp "fuck" t)
          ;;(testlinks--search-for-regexp "shit" t)
          (testlinks--search-for-regexp "mailblocks.com" t)
          )

        (if is-editing
            (setq buffer-read-only is-readonly)
          (kill-buffer nil))
        (setq ptr (cdr ptr))
        )
      )
    )
  )

(defun testlinks ()
  (interactive)
  (let (dir is-a-dir time-start time-stop dif)
    (setq time-start (current-time))
    (setq dir (read-file-name "Enter dir: " default-directory))
    (save-some-buffers 'NOQUESTIONS)
    (setq is-a-dir (car (file-attributes dir)))
    (if (not is-a-dir) (setq dir (file-name-directory dir)))
    (testlinks-inner dir)
    (setq time-stop (current-time))
    (setq dif (seconds-of-time-difference time-start time-stop))

    (progn
      (set-buffer testlinks--lbuf)
      (goto-char (point-max))
      (insert "**** TIME TOOK: = " (seconds-to-readable-string dif) "\n"))

    (if prefs-home-emacs
        (save-excursion
          (set-buffer testlinks--lbuf)
          (goto-char (point-min))
          (flush-lines "d:/home/hairy-lemon/output/50webs-com/email.html:"))) ;;; encoded email mailto:

    (progn
      (switch-to-buffer testlinks--bufname)
      (goto-char (point-max))
      )

    (d-random-play-emacs-midi)

    ))

(defun d-random-play-emacs-midi (&optional file)
  (interactive)
  (if (not file) (setq file "game-over-b.wav"))
  (play-sound (list 'sound :file file :volume 1.0))
  )

(provide 'd-testlinks)
 
