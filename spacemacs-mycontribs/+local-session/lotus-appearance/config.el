;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
;; Keywords: convenience

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
  ;; (progn ;; "face size"

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

  (progn ;; "face size"

    (defun maxmin-optimized-value (val scale div &optional max min)
      (let ((opt (/ (* val scale) div)))
        (if (and max
                 (> max 0)
                 (> opt max))
            max
            (if (and min
                     (> min 0)
                     (< opt min))
                min
                opt))))


    ;; set attributes
    (defun mycustom-face-set ()
      "thisandthat."
      (interactive)
      (set-face-attribute 'default nil ;(/ (* (x-display-mm-width) 121) 600)
                          ;; (x-display-pixel-height)
                          :height (maxmin-optimized-value (x-display-mm-height) 110 600 120 75)
                          :width  'normal))


    ;; (mycustom-face-set)
    ;;:font FONT)
    ;; get attributes
    ;; (face-attribute 'default :font)
    ;; (face-attribute 'default :height)

    (defvar *custom-xface-factor* 7)

    (defun set-default-face-height-by-resolution (&optional height)
      (interactive
       (list (read-number "Face height: "
                          (if (and (featurep 'x)
                                    window-system
                                    (x-display-mm-height))
                              (maxmin-optimized-value (x-display-mm-height) 110 600 120 75)
                              (face-attribute 'default :height)))))
      (if (and (featurep 'x) window-system)
          (if (x-display-mm-height)
              (if (any-frame-opened-p)
               (set-face-attribute 'default nil :height (maxmin-optimized-value (x-display-mm-height) 110 600 120 75)))
              (message "(x-display-pixel-height) return nil"))
          (message "set-default-face-height-by-resolution: Not in Graphical Window system.")))

    (add-hook 'sharad/enable-startup-interrupting-feature-hook
              '(lambda ()
                (run-at-time-or-now 3 '(lambda () (set-default-face-height-by-resolution))))))

(progn ;; "face help"
  ;; http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
  (defun what-face (pos)
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos))))


  ;; what-cursor-position with a prefix argument shows the face under point, among other information.
  ;; Keyboard shortcut is C-u C-x =


  ;; C-u M-x describe-face

  )


;;

;; (progn ;; "Font"
;;   (set-face-attribute 'default nil :font "KacstNaskh")

;;   (set-default-font "Mono-10")
;;   (set-default-font "KacstNaskh")
;;   (set-face-attribute 'default nil :font "KacstNaskh")
;;   (set-face-attribute 'default nil :font "Mono-10")

;; (custom-set-faces '(family "DejaVu Sans Mono"))

;; (custom-set-faces '(family "KacstNaskh"))


;;   )


;; (length (font-family-list))



(progn ;; "Fonts"
  (progn ;; "gist"
    (defun font-is-mono-p (font-family)
      ;; with-selected-window
      (let ((wind (selected-window))
            m-width l-width)
        ;; (with-current-buffer (get-buffer-create "asdf")
        (with-temp-buffer
          (set-window-buffer (selected-window) (current-buffer))
          (text-scale-set 4)
          (insert (propertize "l l l l l" 'face `((:family ,font-family))))
          (goto-char (line-end-position))
          (setq l-width (car (posn-x-y (posn-at-point))))
          (newline)
          (forward-line)
          (insert (propertize "m m m m m" 'face `((:family ,font-family) italic)))
          (goto-char (line-end-position))
          (setq m-width (car (posn-x-y (posn-at-point))))
          (eq l-width m-width))))

    (defun compare-monospace-fonts (&optioanal mono)
      "Display a list of all monospace font faces."
      (interactive)
      (pop-to-buffer "*Monospace Fonts*")
      (erase-buffer)
      (dolist (font-family (font-family-list))
        (when (or mono (font-is-mono-p font-family))
          (let ((str font-family))
            (newline)
            (insert
             (propertize (concat "The quick brown fox jumps over the lazy dog 1 l; 0 O o ("
                                 font-family ")\n") 'face `((:family ,font-family)))
             (propertize (concat "The quick brown fox jumps over the lazy dog 1 l; 0 O o ("
                                 font-family ")\n") 'face `((:family ,font-family) italic)))))))
    ))

;;; config.el ends here
