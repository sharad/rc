



(when
    (xrequire 'color-theme)


  ;; (if (not window-system)
  ;;     nil
  ;;   (progn
  (when (functionp 'color-theme-initialize)
    (color-theme-initialize))
  (setq
   custom-theme-load-path "~/.xemacs/color-themes"
   favorite-color-themes
        '(color-theme-arjen           ;all time nice
          color-theme-robin-hood      ;green nice
          color-theme-goldenrod
          color-theme-comidia         ;good
          color-theme-charcoal-black  ;blck board
          color-theme-clarity
          color-theme-ld-dark
          color-theme-matrix
          color-theme-oswald
          color-theme-kingsajz
          color-theme-parus
          color-theme-dark-blue2
          color-theme-gray30
          color-theme-subtle-hacker))
  (random t)                            ;set the seed according to the
                                        ;system clock
  (setq current-color-theme (nth (random (length
                                          favorite-color-themes)) favorite-color-themes))
  ;; (funcall current-color-theme)
  )

(defun color-theme-arjen/sharad (&optional not-modeline)
  "Color theme by awiersma, created 2001-08-27."
  (interactive "P")
  (color-theme-install
   `(color-theme-arjen/sharad
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "yellow")
      (foreground-color . "White")
      (mouse-color . "sienna1"))
     ((buffers-tab-face . buffers-tab)
      (cperl-here-face . font-lock-string-face)
      (cperl-invalid-face quote underline)
      (cperl-pod-face . font-lock-comment-face)
      (cperl-pod-head-face . font-lock-variable-name-face)
      (vc-mode-face . highlight))
     ;; (default ((t (:inherit t :background "black" :foreground "white"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "black" :foreground "white"))))
     (calendar-today-face ((t (:underline t))))
     (cperl-array-face ((t (:foreground "darkseagreen"))))
     (cperl-hash-face ((t (:foreground "darkseagreen"))))
     (cperl-nonoverridable-face ((t (:foreground "SkyBlue"))))
     (custom-button-face ((t (nil))))
     (custom-changed-face ((t (:background "blue" :foreground "white"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face ((t (:underline t :foreground "light blue"))))
     (custom-group-tag-face-1 ((t (:underline t :foreground "pink"))))
     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))
     (custom-modified-face ((t (:background "blue" :foreground "white"))))
     (custom-rogue-face ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:background "white" :foreground "blue"))))
     (custom-state-face ((t (:foreground "lime green"))))
     (custom-variable-button-face ((t (:underline t :bold t))))
     (custom-variable-tag-face ((t (:underline t :foreground "light blue"))))
     (diary-face ((t (:foreground "IndianRed"))))
     (erc-action-face ((t (:bold t))))
     (erc-bold-face ((t (:bold t))))
     (erc-default-face ((t (nil))))
     (erc-direct-msg-face ((t (:foreground "sandybrown"))))
     (erc-error-face ((t (:bold t :foreground "IndianRed"))))
     (erc-input-face ((t (:foreground "Beige"))))
     (erc-inverse-face ((t (:background "wheat" :foreground "darkslategrey"))))
     (erc-notice-face ((t (:foreground "MediumAquamarine"))))
     (erc-pal-face ((t (:foreground "pale green"))))
     (erc-prompt-face ((t (:foreground "MediumAquamarine"))))
     (erc-underline-face ((t (:underline t))))
     (eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))
     (eshell-ls-backup-face ((t (:foreground "Grey"))))
     (eshell-ls-clutter-face ((t (:foreground "DimGray"))))
     (eshell-ls-directory-face ((t (:bold t :foreground "MediumSlateBlue"))))
     (eshell-ls-executable-face ((t (:foreground "Coral"))))
     (eshell-ls-missing-face ((t (:foreground "black"))))
     (eshell-ls-picture-face ((t (:foreground "Violet"))))
     (eshell-ls-product-face ((t (:foreground "sandybrown"))))
     (eshell-ls-readonly-face ((t (:foreground "Aquamarine"))))
     (eshell-ls-special-face ((t (:foreground "Gold"))))
     (eshell-ls-symlink-face ((t (:foreground "White"))))
     (eshell-ls-unreadable-face ((t (:foreground "DimGray"))))
     (eshell-prompt-face ((t (:foreground "MediumAquamarine"))))

     (etask-face-normaltask ((t (:foreground "white"))))
     (etask-face-day-marker ((t (:foreground "white"))))
     (etask-face-normaltaskname-behind-schedule ((t (:foreground "white" :weight bold))))
     (etask-face-normaltaskname-completed ((t (:foreground "white" :strike-through t))))
     (etask-face-status-ok ((t (:foreground "white" :weight bold))))


     (fl-comment-face ((t (:foreground "pink"))))
     (fl-doc-string-face ((t (:foreground "purple"))))
     (fl-function-name-face ((t (:foreground "red"))))
     (fl-keyword-face ((t (:foreground "cadetblue"))))
     (fl-string-face ((t (:foreground "green"))))
     (fl-type-face ((t (:foreground "yellow"))))
     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-comment-face ((t (:foreground "IndianRed"))))
     (font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "YellowGreen"))))

     ;; (font-lock-keyword-face ((t (:foreground "PaleYellow"))))
     (font-lock-keyword-face ((t (:foreground "yellow"))))

     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))
     (font-lock-string-face ((t (:foreground "Orange"))))
     (font-lock-type-face ((t (:foreground "Green"))))
     (font-lock-variable-name-face ((t (:foreground "darkseagreen"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (qt-classes-face ((t (:foreground "Red"))))
     (gnus-cite-attribution-face ((t (nil))))
     (gnus-cite-face-1 ((t (:bold nil :foreground "deep sky blue"))))
     (gnus-cite-face-10 ((t (:foreground "medium purple"))))
     (gnus-cite-face-11 ((t (:foreground "turquoise"))))
     (gnus-cite-face-2 ((t (:bold nil :foreground "cadetblue"))))
     (gnus-cite-face-3 ((t (:bold nil :foreground "gold"))))
     (gnus-cite-face-4 ((t (:foreground "light pink"))))
     (gnus-cite-face-5 ((t (:foreground "pale green"))))
     (gnus-cite-face-6 ((t (:bold nil :foreground "chocolate"))))
     (gnus-cite-face-7 ((t (:foreground "orange"))))
     (gnus-cite-face-8 ((t (:foreground "magenta"))))
     (gnus-cite-face-9 ((t (:foreground "violet"))))
     (gnus-emphasis-bold ((t (:bold nil))))
     (gnus-emphasis-bold-italic ((t (:bold nil))))
     (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))
     (gnus-emphasis-italic ((t (nil))))
     (gnus-emphasis-underline ((t (:underline t))))
     (gnus-emphasis-underline-bold ((t (:underline t :bold nil))))
     (gnus-emphasis-underline-bold-italic ((t (:underline t :bold nil))))
     (gnus-emphasis-underline-italic ((t (:underline t))))
     (gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))
     (gnus-group-mail-1-face ((t (:bold nil :foreground "aquamarine1"))))
     (gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))
     (gnus-group-mail-2-face ((t (:bold nil :foreground "aquamarine2"))))
     (gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))
     (gnus-group-mail-3-face ((t (:bold nil :foreground "aquamarine3"))))
     (gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))
     (gnus-group-mail-low-face ((t (:bold nil :foreground "aquamarine4"))))
     (gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))
     (gnus-group-news-1-face ((t (:bold nil :foreground "PaleTurquoise"))))
     (gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))
     (gnus-group-news-2-face ((t (:bold nil :foreground "turquoise"))))
     (gnus-group-news-3-empty-face ((t (nil))))
     (gnus-group-news-3-face ((t (:bold nil))))
     (gnus-group-news-4-empty-face ((t (nil))))
     (gnus-group-news-4-face ((t (:bold nil))))
     (gnus-group-news-5-empty-face ((t (nil))))
     (gnus-group-news-5-face ((t (:bold nil))))
     (gnus-group-news-6-empty-face ((t (nil))))
     (gnus-group-news-6-face ((t (:bold nil))))
     (gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))
     (gnus-group-news-low-face ((t (:bold nil :foreground "DarkTurquoise"))))
     (gnus-header-content-face ((t (:foreground "forest green"))))
     (gnus-header-from-face ((t (:bold nil :foreground "spring green"))))
     (gnus-header-name-face ((t (:foreground "deep sky blue"))))
     (gnus-header-newsgroups-face ((t (:bold nil :foreground "purple"))))
     (gnus-header-subject-face ((t (:bold nil :foreground "orange"))))
     (gnus-signature-face ((t (:bold nil :foreground "khaki"))))
     (gnus-splash-face ((t (:foreground "Brown"))))
     (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
     (gnus-summary-high-ancient-face ((t (:bold nil :foreground "SkyBlue"))))
     (gnus-summary-high-read-face ((t (:bold nil :foreground "PaleGreen"))))
     (gnus-summary-high-ticked-face ((t (:bold nil :foreground "pink"))))
     (gnus-summary-high-unread-face ((t (:bold nil))))
     (gnus-summary-low-ancient-face ((t (:foreground "SkyBlue"))))
     (gnus-summary-low-read-face ((t (:foreground "PaleGreen"))))
     (gnus-summary-low-ticked-face ((t (:foreground "pink"))))
     (gnus-summary-low-unread-face ((t (nil))))
     (gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))

     ;; (gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))
     (gnus-summary-normal-read-face ((t (:foreground "light green"))))

     ;; (gnus-summary-normal-ticked-face ((t (:foreground "pink"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "IndianRed4"))))

     (gnus-summary-normal-unread-face ((t (:foreground "light salmon" :bold t :slant italic :underline nil))))     ;; it should be tolally different.

     (gnus-summary-selected-face ((t (:underline t))))
     (green ((t (:foreground "green"))))
     (gui-button-face ((t (:background "grey75" :foreground "black"))))
     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (highlight ((t (:background "darkolivegreen"))))
     (highline-face ((t (:background "SeaGreen"))))
     (holiday-face ((t (:background "DimGray"))))
     (info-menu-5 ((t (:underline t))))
     (info-node ((t (:underline t :bold t :foreground "DodgerBlue1"))))
     (info-xref ((t (:underline t :foreground "DodgerBlue1"))))
     (isearch ((t (:background "blue"))))
     (isearch-secondary ((t (:foreground "red3"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (list-mode-item-selected ((t (:background "gray68" :foreground "white"))))
     (message-cited-text-face ((t (:bold t :foreground "green"))))
     (message-header-cc-face ((t (:bold t :foreground "green4"))))
     (message-header-name-face ((t (:bold t :foreground "orange"))))
     (message-header-newsgroups-face ((t (:bold t :foreground "violet"))))
     (message-header-other-face ((t (:bold t :foreground "chocolate"))))
     (message-header-subject-face ((t (:bold t :foreground "yellow"))))
     (message-header-to-face ((t (:bold t :foreground "cadetblue"))))
     (message-header-xheader-face ((t (:bold t :foreground "light blue"))))
     (message-mml-face ((t (:bold t :foreground "Green3"))))
     (message-separator-face ((t (:foreground "blue3"))))

     ,@(unless not-modeline
         '(
           ;; see page http://www.gnu.org/software/libtool/manual/emacs/Standard-Faces.html Standard Faces
           ;; q: which one is correct nodeline or mode-line ?
           ;; a: modeline is working
           ;; setup for `mode-line-highlight' face also
           (modeline ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
           (modeline-inactive ((t (:background "DarkGreen" :foreground "white" :box (:line-width 1 :style released-button)))))
           (modeline-buffer-id ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
           (modeline-mousable ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
           (modeline-mousable-minor-mode ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))


           (mode-line ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
           (mode-line-inactive ((t (:background "DarkGreen" :foreground "white" :box (:line-width 1 :style released-button)))))
           (mode-line-buffer-id ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
           (mode-line-mousable ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))
           (mode-line-mousable-minor-mode ((t (:background "DarkRed" :foreground "white" :box (:line-width 1 :style released-button)))))))


     (p4-depot-added-face ((t (:foreground "blue"))))
     (p4-depot-deleted-face ((t (:foreground "red"))))
     (p4-depot-unmapped-face ((t (:foreground "grey30"))))
     (p4-diff-change-face ((t (:foreground "dark green"))))
     (p4-diff-del-face ((t (:foreground "red"))))
     (p4-diff-file-face ((t (:background "gray90"))))
     (p4-diff-head-face ((t (:background "gray95"))))
     (p4-diff-ins-face ((t (:foreground "blue"))))
     (pointer ((t (nil))))
     (primary-selection ((t (:background "blue"))))
     (red ((t (:foreground "red"))))
     (region ((t (:background "blue"))))
     (right-margin ((t (nil))))
     (secondary-selection ((t (:background "darkslateblue"))))
     (show-paren-match-face ((t (:background "Aquamarine" :foreground "SlateBlue"))))
     (show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (vertical-divider ((t (nil))))
     (widget ((t (nil))))
     (widget-button-face ((t (:bold t))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "lime green"))))
     (widget-field-face ((t (:background "dim gray"))))
     (widget-inactive-face ((t (:foreground "light gray"))))
     (widget-single-line-field-face ((t (:background "dim gray"))))
     (woman-bold-face ((t (:bold t))))
     (woman-italic-face ((t (:foreground "beige"))))
     (woman-unknown-face ((t (:foreground "LightSalmon"))))
     (yellow ((t (:foreground "yellow"))))
     (zmacs-region ((t (:background "snow" :foreground "blue")))))))

(color-theme-arjen/sharad t)

;; info
;; get color name by `list-colors-display'

;; color-theme-is-cumulative
;; color-theme-is-cumulative is a variable defined in `color-theme.el'.
;; Its value is t
;; Documentation:
;; *Determines wether new color themes are installed on top of each other.
;; If non-nil, installing a color theme will undo all settings made by
;; previous color themes.

(provide 'color-config)
