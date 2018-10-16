;;; hand-custom.el --- Custom Override setings

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <>
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




;;{{{ custom-set-variables
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(bbdb-check-zip-codes-p nil)
 '(bbdb-default-country "INDIA")
 '(bbdb-north-american-phone-numbers-p nil)
 '(blink-cursor-mode t)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.bmk")
 '(calendar-mark-diary-entries-flag t)
 '(calendar-view-diary-initially-flag t)
 '(canlock-password "93d68f0366b0f7bb850c020c9b41a029023b0526")
 '(case-fold-search t)
;; '(color-theme-selection "Arjen" nil (color-theme))
 '(column-number-mode t)
 '(current-language-environment "ASCII")
 '(default-input-method "english-dvorak")
 '(diary-entry-marker "+")
 ;; '(diary-file "~/.Organize/emacs/diary/diary")
 '(diary-mail-addr "sh4r4d _at_ _G-mail_")
 '(diary-mail-addr "sh4r4d _at_ _G-mail_")
 '(display-time-24hr-format nil)
 '(display-time-day-and-date t)
 '(display-time-mail-face (quote default))
 '(display-time-mail-file nil)
 '(display-time-mode t)
 ;; '(ecb-auto-activate nil)
 ;; '(ecb-layout-window-sizes (quote (("sharad-leftright-analyse-etc-reverse" (ecb-directories-buffer-name 0.1179245283018868 . 0.4909090909090909) (ecb-analyse-buffer-name 0.1179245283018868 . 0.4909090909090909) (ecb-methods-buffer-name 0.17452830188679244 . 0.38181818181818183) (ecb-sources-buffer-name 0.17452830188679244 . 0.2909090909090909) (ecb-history-buffer-name 0.17452830188679244 . 0.3090909090909091)))))
 ;; '(ecb-major-modes-show-or-hide (quote ((js-mode cperl-mode perl-mode emacs-lisp-mode lisp-mode c-mode shell-script-mode) mail-mode fundamental-mode planner-mode)))
 ;; '(ecb-options-version "2.40")
 ;; '(ecb-other-window-behavior (quote edit-and-compile))
 ;; '(ecb-toggle-layout-sequence (quote ("sharad-leftright-analyse" "left9" "left14")))
 '(erc-modules (quote (autoaway autojoin button capab-identify completion hecomplete dcc fill identd irccontrols keep-place list log match menu move-to-prompt netsplit networks noncommands notify page readonly replace ring scrolltobottom services smiley sound stamp spelling track truncate unmorse xdcc)))
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-registry-install t t)
 '(gnus-treat-from-picon (quote head))
 '(gnus-treat-mail-picon (quote head))
 '(gnus-treat-newsgroups-picon (quote head))
 '(ido-enable-tramp-completion nil)
 '(ido-use-filename-at-point (quote guess))
 '(load-home-init-file t t)
 '(menu-bar-mode nil)
 '(mm-inline-text-html-with-images t)
 '(newsticker-url-list (quote (("OSNews" "http://www.osnews.com/feeds" nil nil nil))))
 ;; '(org-agenda-files (file-expand-wildcards "~/.Organize/emacs/org/*/*.org"))
 '(org-agenda-include-diary t)
 '(planner-annotation-strip-directory t)
 '(planner-annotation-use-relative-file t)
 '(planner-appt-forthcoming-look-at-cyclic-flag t)
 '(planner-appt-schedule-cyclic-behaviour (quote future))
 '(planner-appt-update-appts-on-save-flag t)
 '(planner-create-section-function (quote planner-create-at-bottom))
 '(planner-cyclic-diary-nag t)
 ;; '(planner-diary-appts-file "~/.Organize/emacs/diary/diary")
 ;; '(planner-diary-cal-desk-file "~/.Organize/emacs/diary/diary")
 '(planner-diary-exclude-appts-from-diary nil)
 ;; '(planner-diary-file "~/.Organize/emacs/diary/diary")
 '(planner-diary-number-of-days 5)
 ;; '(planner-diary-private-file "~/.Organize/emacs/diary/private")
 '(planner-diary-private-number-of-days 1)
 ;; '(planner-diary-public-file "~/.Organize/emacs/diary/public")
 '(planner-diary-public-number-of-days 1)
 '(planner-diary-use-appts t)
 '(planner-diary-use-cal-desk t)
 '(planner-diary-use-diary t)
 '(planner-diary-use-private-diary t)
 '(planner-diary-use-public-diary t)
 '(planner-expand-name-favor-future-p t)
 '(planner-renumber-notes-automatically t)
 '(planner-renumber-tasks-automatically t)
 '(planner-sections (quote ((tasks . "Tasks") (notes . "Notes") (diary . "Diary") (env . "Environment"))))
 '(planner-tasks-file-behavior (quote close))
 '(safe-local-variable-values (quote ((folded-file . t) (auto-fill-mode))))
 '(save-abbrevs t)
 '(save-place t nil (saveplace))
 '(scroll-conservatively 4)
 '(send-mail-function (quote sendmail-send-it))
 ;; '(session-set-file-name-exclude-regexp "/\\.overview\\|.session\\|News/\\|\\.Organize/")
 '(session-use-package t)
 '(show-paren-mode t nil (paren))
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|.deps\\)\\'")
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (set-background-color "black"))))
 '(speedbar-supported-extension-expressions (quote (".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".tex\\(i\\(nfo\\)?\\)?" ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".f\\(90\\|77\\|or\\)?" ".ad*" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g" ".s?html" "[Mm]akefile\\(\\.in\\|am\\)?" "configure.ac" ".ml*" ".tig" ".\\(ll\\|yy\\)")))
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tramp-fish-method "fish")
 '(tramp-verbose 10)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-follow-symlinks t)
 '(view-read-only t)
 '(w3m-default-display-inline-images t)
 '(weblogger-config-alist (quote (("default" "http://www.livejournal.com/interface/blogger/" "sh4r4d" "" "sh4r4d"))))
 '(yas/trigger-key "C->"))
;;}}}


;;{{{ custom-set-faces
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(background "blue")
 '(family "DejaVu Sans Mono")
 '(etask-face-day-marker ((((class color)) (:foreground "white"))))
 '(etask-face-normaltask ((((class color)) (:foreground "white"))))
 '(etask-face-normaltaskname-behind-schedule ((((class color)) (:foreground "white" :weight bold))))
 '(etask-face-normaltaskname-completed ((((class color)) (:foreground "white" :strike-through t))))
 '(etask-face-status-ok ((((class color)) (:foreground "white" :weight bold))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
 '(font-lock-comment-face ((t (:foreground "MediumAquamarine"))))
 '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
 '(font-lock-doc-string-face ((t (:foreground "green2"))))
 '(font-lock-function-name-face ((t (:foreground "SkyBlue"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
 '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
 '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-string-face ((t (:foreground "LimeGreen"))))
 '(font-lock-type-face ((t (:foreground "#9290ff"))))
 '(font-lock-variable-name-face ((t (:foreground "PaleGreen"))))
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "yellow" :background "red"))))
 '(gnus-summary-normal-unread ((t (:foreground "light salmon" :inverse-video nil :slant italic :weight bold))))
 '(highlight ((t (:background "CornflowerBlue"))))
 '(list-mode-item-selected ((t (:background "gold"))))
 '(makefile-space ((t (:background "wheat"))))
 '(makefile-space-face ((t (:background "wheat"))) t)
 '(mode-line ((t (:background "Navy"))))
 '(paren-match ((t (:background "darkseagreen4"))) t)
 '(region ((t (:background "DarkSlateBlue"))))
 '(show-paren-match ((t (:foreground "black" :background "wheat"))))
 '(show-paren-mismatch ((((class color)) (:foreground "white" :background "red"))))
 '(speedbar-button-face ((((class color) (background dark)) (:foreground "green4"))))
 '(speedbar-directory-face ((((class color) (background dark)) (:foreground "khaki"))))
 '(speedbar-file-face ((((class color) (background dark)) (:foreground "cyan"))))
 '(speedbar-tag-face ((((class color) (background dark)) (:foreground "Springgreen"))))
 '(trailing-whitespace ((((class color) (background light)) (:foreground unspecified :strike-through "chartreuse2"))))
 '(vhdl-speedbar-architecture-selected-face ((((class color) (background dark)) (:underline t :foreground "Blue"))))
 '(vhdl-speedbar-entity-face ((((class color) (background dark)) (:foreground "darkGreen"))))
 '(vhdl-speedbar-entity-selected-face ((((class color) (background dark)) (:underline t :foreground "darkGreen"))))
 '(vhdl-speedbar-package-face ((((class color) (background dark)) (:foreground "black"))))
 '(vhdl-speedbar-package-selected-face ((((class color) (background dark)) (:underline t :foreground "black"))))
 '(widget-field ((((class grayscale color) (background light)) (:background "DarkBlue")))))
;;}}}



;;{{{ setq lot of variables
(setq
 case-fold-search t       ; searches and matches should ignore case
 column-number-mode t     ; display the column number
 confirm-kill-emacs nil   ; don't confirm when exiting emacs
                          ; kill-ring-max 20 ;number of remembered
                          ; cutted texts
 show-paren-style 'mixed  ; my favorite style of highlighting matching
                          ; parentheses

 ;;standard-indent 2 ;don't go to fast towards right
 transient-mark-mode t             ; show the region
 scroll-preserve-screen-position t ; don't move the cursor when scrolling
 ;;doxymacs: emacs mode for doxygen, a source code documentation generator
 european-calendar-style t
 frame-title-format "%b (%f)"  ; the title-bar displays the filename
                               ; of the current buffer
 font-lock-maximum-decoration t         ; as colored as possible
 ;; visible-bell t                      ; stop beeping
 show-paren-delay 0                     ;Show the matching immediately
 default-indicate-empty-lines t ;show me empty lines at the end of the buffer
 ;; inhibit-startup-message t              ;no startup message
 ;; next-line-add-newlines t ; for use with comment-and-go-down, see below
 kill-whole-line t                    ;take the CR when killing a line
 x-stretch-cursor t      ;when on a TAB, the cursor has the TAB length
 require-final-newline t ;adds a newline at the end of the file beeing
                                        ;saved if it doesn't already have one
 compile-command '"make"
 ;; version-control t ; Allow numbered backups, enough
 version-control nil  ; Allow numbered backups, enough
 time-stamp-active t                    ; update timestamps
 time-stamp-warn-inactive t             ; warn if unable
 ispell-dictionary "english" ; check ispell-dictionary-base-alist variable for possible vaules
 save-abbrevs 'silently

 ;; english british american
)
;;}}}


(defvar exclude-lib
  (if (string-equal (system-name) "spratap")
      '(tramp)))






;;{{{ Mode line and custom-set-faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line
;; Set a Mode Line that tells me which machine, which directory,
;; and which line I am on, plus the other customary information.
;; (setq default-mode-line-format
;;  (quote
;;   (#("-" 0 1
;;      (help-echo
;;       "mouse-1: select window, mouse-2: delete others ..."))
;;    mode-line-mule-info
;;    mode-line-modified
;;    mode-line-frame-identification
;;    "    "
;;    mode-line-buffer-identification
;;    "    "
;;    (:eval (substring
;;            (system-name) 0 (string-match "\\..+" (system-name))))
;;    ":"
;;    default-directory
;;    #(" " 0 1
;;      (help-echo
;;       "mouse-1: select window, mouse-2: delete others ..."))
;;    (line-number-mode " Line %l ")
;;    global-mode-string
;;    #("   %[(" 0 6
;;      (help-echo
;;       "mouse-1: select window, mouse-2: delete others ..."))
;;    (:eval (mode-line-mode-name))
;;    mode-line-process
;;    minor-mode-alist
;;    #("%n" 0 2 (help-echo "mouse-2: widen" local-map (keymap ...)))
;;    ")%] "
;;    (-3 . "%P")
;;    ;;   "-%-"
;;    )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(put 'scroll-left 'disabled nil)


(provide 'custom-override)
;;; custom-override.el ends here
