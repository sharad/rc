;;; config.el --- config                             -*- lexical-binding: t; truncate-lines: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

(defun percent (num percent &optional base)
  (let ((base (or base 100)))
    (/ (* num percent) base)))

(defvar lotus-mode-line-reduce-percent 75)

(defun face-applied-attribute (face attrib)
  (let ((value (face-attribute face attrib)))
    (if (eq 'unspecified value)
        (let* ((inherit-face (face-attribute face :inherit))
               (inherit-face (when inherit-face
                               (if (eq 'unspecified inherit-face)
                                   'default
                                 inherit-face))))
          (when inherit-face
            (face-applied-attribute inherit-face attrib)))
      value)))

(defun lotus-mode-line-reduce (percent)
  (interactive "Npercent: ")
  (if (= (face-applied-attribute 'mode-line :height)
         (face-applied-attribute 'default :height))
      (let* ((percent (or percent lotus-mode-line-reduce-percent))
             (height (or
                      (face-applied-attribute 'default :height)
                      (face-applied-attribute 'mode-line :height)))
             (percent-height (percent height percent)))
        (when (and percent-height
                   (not (eq 'unspecified percent-height))
                   (not (= height 0))
                   (not (= percent-height 0)))
          (set-face-attribute 'mode-line nil :height percent-height)
          (set-face-attribute 'mode-line nil :width 'normal)))
    (lwarn 'lotus-spacemacs :warning "Already modeline is reduced, not doing anything")))

(defun lotus-mode-line-reset ()
  (interactive)
  ;; (set-face-attribute 'mode-line nil :height 'unspecifed)
  (if (= (face-applied-attribute 'mode-line :height)
         (face-applied-attribute 'default :height))
      (lwarn 'lotus-spacemacs :warning "Modeline is not modified, so not reseting it.")
    (set-face-attribute 'mode-line nil
                        :height (face-attribute 'default :height))))


(defun lotus-powerline-setup-text-scale-factor ()
  (let ((text-scale-factor (/ (* (float (face-attribute 'mode-line :height)) 1.0002)
                              (float (face-attribute 'default :height)))))
    (when (numberp text-scale-factor)
      (setq
       powerline-text-scale-factor text-scale-factor))))

(defun lotus-powerline-attrib-setup ()
  (interactive)
  (when (boundp 'powerline-scale)
    (unless (boundp 'powerline-scale-old)
      (setq powerline-scale-old nil))
    (unless powerline-scale-old
      (setq
       powerline-scale-old              powerline-scale
       powerline-height-old             powerline-height
       powerline-text-scale-factor-old  powerline-text-scale-factor
       powerline-default-separator-old  powerline-default-separator)
      (setq
       powerline-scale              0.8
       powerline-height             10
       powerline-text-scale-factor  nil ;; (lotus-powerline-attrib-setup-text-scale-factor)
       powerline-default-separator 'alternate))))

(defun lotus-powerline-attrib-reset ()
  (interactive)
  (when (boundp 'powerline-scale)
    (unless (boundp 'powerline-scale-old)
      (setq powerline-scale-old nil))
    (when powerline-scale-old
      (setq
       powerline-scale              powerline-scale-old
       powerline-height             powerline-height-old
       powerline-text-scale-factor  powerline-text-scale-factor-old
       powerline-default-separator  powerline-default-separator-old)
      (setq
       powerline-scale-old              nil
       powerline-height-old             nil
       powerline-text-scale-factor-old  nil
       powerline-default-separator-old  nil))))


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

;; (face-attribute 'default :width nil)




;; "face size"
(defvar face-scale-div-max-min '(110 600 120 80))

(setq face-scale-div-max-min '(110 224 120 92))

;; (apply 'maxmin-optimized-value (x-display-mm-height) face-scale-div-max-min)

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



;; (mycustom-face-set)
;;:font FONT)
;; get attributes
;; (face-attribute 'default :font)
;; (face-attribute 'default :height)



(defvar face-size-display-matrix
  '(
    ((1080 1920 285 508) (:height 87 :width normal :machine "lispm"))
    ((900  2966 238 784) (:height 71 :width normal :machine "think530-spratap"))
    ((900  2966 237 781) (:height 71 :width normal :machine "think530-spratap"))
    ((900  2966 238 784) (:height 72 :width normal :machine "think530-spratap"))
    ((900  2966 237 781) (:height 72 :width normal :machine "think530-spratap"))
    ((1080 3286 285 868) (:height 71 :width normal :machine "latitude5480-spratap"))
    ((1080 1920 285 508) (:height 68 :width normal :machine "latitude5480-spratap"))
    ((1080 3286 285 867) (:height 68 :width normal :machine "latitude5480-spratap"))
    ((1080 3286 285 869) (:height 71 :width normal :machine "latitude5480-spratap"))
    )
  "Enter here all machine details of

   ((pixel-height pixel-width mm-height mm-width) . (height width))")

(defun assoc-attribs-in-matrix ()
  (let ((phy-attribs
         (list
          (x-display-pixel-height)
          (x-display-pixel-width)
          (x-display-mm-height)
          (x-display-mm-width))))
    (cadr (assoc phy-attribs face-size-display-matrix))))

(defun get-current-attribes-matrix-row ()
  (interactive)
  (let ((disp-attribs (cons
                       (list
                        (x-display-pixel-height)
                        (x-display-pixel-width)
                        (x-display-mm-height)
                        (x-display-mm-width))
                       (list
                        (list
                         :height (face-attribute 'default :height nil)
                         :width (face-attribute 'default :width nil)
                         :machine (system-name))))))
    (if (interactive-p)
        (progn
          (message "copies %s to kill-ring"
                   (prin1-to-string disp-attribs))
          (kill-new (prin1-to-string disp-attribs))))
    disp-attribs))

(defvar *custom-xface-factor* 7)

(defun set-default-face-height-by-resolution (&optional height width)
  (interactive
   (let* ((disp-attrib (assoc-attribs-in-matrix))
          (height
           (read-number "Face height: "
                        (or
                         (plist-get disp-attrib :height)
                         (if (and (featurep 'x)
                                  window-system
                                  (x-display-mm-height))
                             (apply 'maxmin-optimized-value (x-display-mm-height) face-scale-div-max-min)
                           (face-attribute 'default :height)))))
          (width
           (read
            (prin1-to-string
             (read-minibuffer "Face width: "
                              (prin1-to-string (or
                                                (plist-get disp-attrib :width)
                                                (face-attribute 'default :width))))))))
     (list height width)))
  (if (and (featurep 'x) window-system)
      (let* ((disp-attrib (assoc-attribs-in-matrix))
             (height
              (or
               height
               (plist-get disp-attrib :height)
               (if (and (featurep 'x)
                        window-system
                        (x-display-mm-height))
                   (apply 'maxmin-optimized-value (x-display-mm-height) face-scale-div-max-min)
                 (face-attribute 'default :height))))
             (width
              (or
               width
               (plist-get disp-attrib :width)
               (face-attribute 'default :width))))
        (if (x-display-mm-height)
            (if (any-frame-opened-p)
                (progn
                  (spacemacs/set-default-font dotspacemacs-default-font)
                  (when height (set-face-attribute 'default nil :height height))
                  (when width  (set-face-attribute 'default nil :width width)))
              (message "no frame is open now."))
          (message "(x-display-pixel-height) return nil")))
    (message
     "set-default-face-height-by-resolution: Not in Graphical Window system, window-system %s" window-system)))

(when (fboundp 'set-default-face-height-by-resolution)
  (defalias 'mycustom-face-set #'set-default-face-height-by-resolution))
;; (use-package startup-hooks
;;              :defer t
;;              :config
;;              (add-to-enable-startup-interrupting-feature-hook
;;               '(lambda ()
;;                  (run-at-time-or-now 3 '(lambda () (set-default-face-height-by-resolution))))))




;; "face help"
;; http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face
        (message "Face: %s" face)
      (message "No face at %d" pos))))


;; what-cursor-position with a prefix argument shows the face under point, among other information.
;; Keyboard shortcut is C-u C-x =


;; C-u M-x describe-face



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






;; "Fonts"
;; "gist"
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



(setq lotus-dotspacemacs-default-font-list
      '(("DejaVu Sans Mono:style=Book:antialias=true" :size 10 :weight normal :width normal :powerline-scale 0.8 :powerline-text-scale-factor 0.5 :powerline-default-separator 'curve)
        ("DejaVu Sans Mono:size=8:antialias=true" :size 9 :weight normal :width normal :powerline-scale 0.8 :powerline-text-scale-factor 0.5 :powerline-default-separator 'curve)
        ("DejaVu Sans Mono" :size 9 :weight normal :width normal :powerline-scale 1.1)
        ("Source Code Pro:antialias=true" :size 9 :weight normal :width normal :powerline-scale 1.1)
        ("Source Code Pro" :size 9 :weight normal :width normal :powerline-scale 1.1)))
(setq
 dotspacemacs-default-font (car lotus-dotspacemacs-default-font-list))
(spacemacs/set-default-font dotspacemacs-default-font)

(defun lotus-set-default-face (font)
  (interactive
   (let* ((font-name (completing-read "Fonts: " lotus-dotspacemacs-default-font-list))
          (font      (assoc font-name lotus-dotspacemacs-default-font-list)))
     (list font)))
  (message "Setting font: %s" font)
  (setq dotspacemacs-default-font font)
  (spacemacs/set-default-font font))


(defun lotus-appearance-setup ()
  (interactive)
  (setq
   dotspacemacs-default-font (car lotus-dotspacemacs-default-font-list))
  (spacemacs/set-default-font dotspacemacs-default-font)
  (lotus-mode-line-reduce lotus-mode-line-reduce-percent)
  (lotus-powerline-attrib-setup)
  (run-with-idle-timer 3 nil #'set-default-face-height-by-resolution)
  (set-default-face-height-by-resolution))

(defun lotus-appearance-reset ()
  (interactive)
  (lotus-mode-line-reset)
  (lotus-powerline-attrib-reset))


(add-hook 'after-make-frame-functions
          #'(lambda (frame)
              ;; (set-default-face-height-by-resolution)
              (lotus-appearance-reset)
              (lotus-appearance-setup))
          t)

;;; config.el ends here
