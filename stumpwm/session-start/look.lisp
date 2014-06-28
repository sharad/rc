
(in-package :stumpwm)

;;{{{ Color
;; colors
;; 0 = black, 1 = red, 2 = green, 3 = yellow,
;; 4 = blue, 5 = magenta, 6 = cyan, 7 = white
;;}}}



(set-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
(set-frame-outline-width 1)
(set-focus-color "Green")
(set-unfocus-color "White")
(set-bg-color "gray25")
;; (set-bg-color "coral")


;;{{{ Customization:  change default parameters

(setf
 *frame-indicator-text* " Current frame "
 *message-window-gravity* :top-right
 *new-window-preferred-frame* '(:empty :focused)
 *menu-maximum-height* 50
 *menu-scrolling-step* 1
 *suppress-frame-indicator* nil
 *normal-border-width* 1
 *window-border-style* :thight
 ;; mode-line
 *mode-line-position* :top
)

(setf *group-format* "%t [%s]")

;;}}}
