;;; d-groups.el --- keeping files in color-coded groups

;; Copyright (C) 2006-2011 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: Color Coded Groups
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

;; Under this system, the modeline is coloured by which "group" the
;; currently editing file or directory is a part of.  For example:
;; ~/dlisp is a group.  You will probably need to edit the function
;; d-groups-get-face to get optimum groups for your computer.

;;; Install Instructions:

;; See the following URL for the latest info and a tarball:

;; http://davin.50webs.com/research/2010/mopa2e2.html#d-groups.el

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'd-groups.el) 

;;; Known Bugs:

;; None so far!

(copy-face 'fg:lightblue        'd-face-groups-home)
(copy-face 'fg:orange           'd-face-groups-programming)
(copy-face 'fg:lightgreen       'd-face-groups-webdata)
(copy-face 'fg:lightmagenta     'd-face-groups-dlisp)
(copy-face 'default             'd-face-groups-output)
(set-face-background            'd-face-groups-output "#0ff")
(copy-face 'bg:lightred         'd-face-groups-bak)
(copy-face 'bg:red              'd-face-groups-bak-c)
(set-face-foreground            'd-face-groups-bak-c "#0f0")
(copy-face 'bg:red              'd-face-groups-readonly)

;;;
;;; NOTE: don't use face-foreground as the background color will be different from the foreground color
;;;
;;; NOTE: for some reason the following doesn't work:
;;;
;;; (copy-face 'fg:yellow 'mode-line)
;;;

(defun d-groups-get-face (dirname)

  (save-match-data
    ;;(debug)

    (setq dirname (safe-expand-file-name dirname))

    ;;
    ;; NOTE: patches dirname to end with slash if it's a directory
    ;;
    (if (and dirname (file-directory-p dirname))
        (setq dirname (concat dirname "/")))

    (let ((black "#000"))
      ;;(debug)

      ;;(message "dirname=%s" dirname)
      (cond
       ((not dirname)
        (list 'font-lock-comment-face "#ccc" black))

       ((string-match "^[a-bd-z]:/" dirname)
        (list 'fg:lightred "#fcc" black))

       ((or (string-match "My Documents" dirname)
            (string-match "Davin's Stuff" dirname))
        (list 'd-face-groups-webdata "#0f0" black))

       ((or (string-match "^c:/bak/" dirname)
            (string-match "^c:/bak-unix/"   dirname))
        (list 'd-face-groups-bak-c "#f00" "#ff0"))

       ((or (string-match "/bak/" dirname)
            (string-match "/bak-unix/"    dirname)
            (string-match "/TRASHCAN/"    dirname)
            (string-match "/RECYCLER/"    dirname)
            (string-match "a:/"           dirname)
            (string-match "2010"          dirname)
            (string-match "Guest"         dirname)
            )
        (list 'd-face-groups-bak "#f00" "#fff"))

       ((string-match "/r4/" dirname)
        (list 'bg:yellow "#ff0" black))

       ((string-match "/output/" dirname)
        ;;
        ;; NOTE: integration with d-readonly.el
        ;;
        (list 'd-face-groups-output "#0ff" black))

       ((and prefs-home-emacs (string-match "/ro[a-z]*/" dirname))
        ;;
        ;; NOTE: integration with d-readonly.el
        ;;
        (list 'd-face-groups-readonly "#f88" "white"))

       ((or (string-match "/tutorial-[0-9]+/" dirname)
            (and prefs-home-emacs (string-match "/webdesign/" dirname))
            (string-match "sjs-tutorials" dirname)
            (string-match (regexp-quote "/c++2lisp++/") dirname))
        (list 'bg:yellow "#ffff00" black))

       ;; (setenv "DLISP" "d:/")
       ((string-match (concat "^" (safe-expand-file-name "~/dlisp") "/") dirname)
        (list 'd-face-groups-dlisp "#fcf" black))

       ((and prefs-home-emacs
             (boundp 'webdata)
             (string-match (safe-expand-file-name webdata) dirname))
        ;;(d-foo)
        (list 'd-face-groups-webdata "#0f0" black))

       ((and prefs-home-emacs
             (or (string-match (safe-expand-file-name "~/[0-9][^/]*/") dirname)
                 (string-match (safe-expand-file-name "~/cosc[0-9][0-9][0-9]-[^/]*/") dirname)
                 (string-match (safe-expand-file-name "~/java-projects/") dirname)))
        (list 'd-face-groups-programming "#fc0" black))

       ;;
       ;; NOTE: extra slash is added here because safe-expand-file-name never returns a trailing slash
       ;;
       ((string-match (concat "^" (safe-expand-file-name (getenv "HOME")) "/") dirname)
        (list 'd-face-groups-home "#ccf" black))
       
       (t
        (list 'default "#ccc" black))))))

(defun d-groups-online ()
  (interactive)
  (progn
    (add-hook 'post-command-hook 'd-groups-modeline-hook)
    (add-hook 'electric-buffer-menu-mode-hook 'd-groups--electric-buffer-list-hook)
    ;;
    ;; NOTE: was defadvice list-buffers
    ;;
    (defun d-groups--electric-buffer-list-hook ()
      ;;(set-buffer "*Buffer List*")
      ;;(d-beeps "buffer-name=%s" (buffer-name))
      (toggle-read-only -1)
      (goto-char (point-min))
      (forward-line 2)
      (let (min max string (case-fold-search t))
        (while (/= (point) (point-max))
          (setq min (point-at-bol))
          (setq max (point-at-eol))
          (setq string (buffer-substring-no-properties min max))
          (put-text-property min
                             (1+ max)
                             'face
                             (if (string-match "\\(/\\|[acd]:/\\|~/\\)" string)
                                 (car (d-groups-get-face (file-name-directory (substring string (match-beginning 0)))))
                               'font-lock-comment-face)
                             )
          (forward-line)))
      (toggle-read-only 1)
      )))

(d-groups-online)

;;(global-set-key "\C-l" 'd-groups-modeline-hook)

(defun d-groups-modeline-hook ()
  (interactive)
  (when (not (eq major-mode 'fundamental-mode))
    (let (f list c1 c2)
      (setq f (buffer-file-name))
      (if (eq major-mode 'dired-mode)
          (setq f dired-directory))
      (setq list (d-groups-get-face f))
      (setq c1 (cadr  list))
      (setq c2 (caddr list))
      (if c1 (set-face-background 'mode-line c1))
      (if c2 (set-face-foreground 'mode-line c2))
      (if c1 (set-face-background 'mode-line-buffer-id c1))
      )))

(defadvice d-recenter (after d-groups activate)
  (when (not (memq 'd-groups-modeline-hook post-command-hook))
    (d-beeps "*** Warning post-command-hook missing d-groups-modeline-hook")
    (add-hook 'post-command-hook 'd-groups-modeline-hook)))

;;
;; (setq post-command-hook (remq 'd-groups-modeline-hook post-command-hook))

(d-groups-modeline-hook)


(d-quote
 ;;
 ;; NOTE: doesn't work
 ;;
 (defadvice bak (after d-groups activate)
   (d-foo)
   (d-groups-modeline-hook)))

;;(copy-face 'fg:yellow 'mode-line)

(provide 'd-groups)


