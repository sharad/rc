;; -*- truncate-lines: t; -*-

(defun increase-font-size (size)
  (custom-set-faces
   '(default
      ((t
        (:inherit
         nil :stipple
         nil :background "black" :foreground "White" :inverse-video
         nil :box nil :strike-through nil :overline nil :underline
         nil :slant normal :weight normal :height 108 :width
         normal :foundry "monotype" :family "Courier New"))))))




(when nil
  (face-list)
  (face-background 'default)
  (face-foreground 'default))

;; check http://delicious.com/sh4r4d/complementry+invert+opposite+hsl+hsv

(defun color-code (color)
  (apply 'format "#%02x%02x%02x"
         (mapcar (lambda (c) (lsh c -8))
                 (color-values color))))

(when nil
  (color-code "black")
  (color-code "white"))

(defun color-invert (code)
  ())


(deh-section "face size"
  ;; 98
  ;; default was 98 or 100
  (set-face-attribute 'default nil :height 86)

  (face-attribute 'default :height))

;; (list-colors-display)
;; http://www.emacswiki.org/emacs/CustomizingFaces
;; http://www.emacswiki.org/emacs/font-lock-color-test.el
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Colors.html
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
;; http://david.rothlis.net/emacs/customize_colors.html
;; http://david.rothlis.net/emacs/customize_colors.html



(provide 'faces-config)

