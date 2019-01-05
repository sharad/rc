
(in-package :stumpwm)

;;{{{ Color
;; colors
;; 0 = black, 1 = red, 2 = green, 3 = yellow,
;; 4 = blue, 5 = magenta, 6 = cyan, 7 = white
;;}}}



(stumpwm:set-font "-Misc-Fixed-Medium-R-SemiCondensed--13-120-75-75-C-60-ISO8859-1")
(stumpwm:set-frame-outline-width 1)
(stumpwm:set-focus-color "Green")
(stumpwm:set-unfocus-color "White")
(stumpwm:set-bg-color "gray25")
;; (set-bg-color "coral")


;;{{{ Customization:  change default parameters

(setf
 stumpwm:*frame-indicator-text* " Current frame "
 stumpwm:*message-window-gravity* :top-right
 stumpwm:*new-window-preferred-frame* '(:empty :focused)
 stumpwm:*menu-maximum-height* 50
 stumpwm:*menu-scrolling-step* 1
 stumpwm:*suppress-frame-indicator* nil
 stumpwm:*normal-border-width* 1
 stumpwm:*window-border-style* :thight
 ;; stumpwm:mode-line
 stumpwm:*mode-line-position* :top
)

(setf *group-format* "%t [%s]")

;;}}}
