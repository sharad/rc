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
         (mapcar (lambda (c)
                   (lsh c -8))
                 (color-values color))))

(when nil
  (color-code "black")
  (color-code "white"))

(defun color-invert (code)
  ())


(when nil
  ;; (deh-section "face size"

  ;; http://stackoverflow.com/questions/3514299/how-do-i-find-the-display-size-of-my-system-in-emacs
  ;; http://stackoverflow.com/questions/2151449/can-i-detect-the-display-size-resolution-in-emacs
  ;; (x-display-pixel-width)
  ;; (/ (x-display-pixel-height) 12)

  ;; 98
  ;; default was 98 or 100
  ;; (set-face-attribute 'default nil :height 80)

  ;; (when nil
  ;;   (defun l100 (v x y)
  ;;     (log (expt 1.1 x) y))

  ;;   (l100 768)
  ;;   (l100 1200)

  ;;   (= (expt y 77) (expt 768 x))
  ;;   (= (expt y 100) (expt 1200 x)))

  ;; (defun cal1 (y)
  ;;   (- (* (log (expt y 100) 768) (log (expt y 33) 768) (expt 10 6))
  ;;      (* (log 1200 768) (expt 10 6))))


  ;; (cal1 1)
  ;; (cal1 1.0188111111)

  ;; log7(x):= log(x)/log(768);
  ;; f(y) := (log7(y / 1000  ^ 100) * log7(y / 1000 ^ 33) - log7(1200)) * 10 ^ 6;
  ;; f(y) := log7(y / 1000  ^ 100) * log7(y / 1000 ^ 33) - log7(1200)
  ;; plot2d(f(y), [y, -10, 10]);
  ;; plot2d(f(y), [y, -1, 100]);
  ;; plot2d(f(y), [y, -1, 1000]);
  ;; plot2d(f(y), [y, -1, 10000000000000000000000]);
  ;; plot2d(f(y), [y, 1.12690488499, 1.12690488509]);




  ;; (if (x-display-pixel-height)
  ;;     (set-face-attribute 'default nil :height (/ (x-display-pixel-height) 10)))

  ;; (face-attribute 'default :height)
  ;; (face-attribute 'default :width))

;; (list-colors-display)
;; http://www.emacswiki.org/emacs/CustomizingFaces
;; http://www.emacswiki.org/emacs/font-lock-color-test.el
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Colors.html
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
;; http://david.rothlis.net/emacs/customize_colors.html
;; http://david.rothlis.net/emacs/customize_colors.html
  )

  (deh-section "face size"

    (defun set-default-face-height-by-resolution (&optional height)
      (interactive
       (list (read-number "Face height: "
                          (if (and (featurep 'x)
                                    window-system
                                    (x-display-pixel-height))
                              (/ (x-display-pixel-height) 10)
                              (face-attribute 'default :height)))))
      (if (and (featurep 'x) window-system)
          (if (x-display-pixel-height)
              (set-face-attribute 'default nil :height (/ (x-display-pixel-height) 10))
              (message "(x-display-pixel-height) return nil"))
          (message "Not in Graphical Window system.")))

    (add-hook 'sharad/enable-startup-inperrupting-feature-hook 'set-default-face-height-by-resolution))


(provide 'faces-config)

