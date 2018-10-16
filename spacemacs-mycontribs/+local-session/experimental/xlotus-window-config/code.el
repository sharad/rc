;;; window-config.el --- Window Config

;; Copyright (C) 2014  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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




(defun make-buffer-small-window ()
  (cond
        ((not (get-buffer-window calculator-buffer))
         (let ((window-min-height 2))
           ;; maybe leave two lines for our window because of the normal
           ;; `raised' modeline in Emacs 21
           (select-window
            (split-window-vertically
             ;; If the modeline might interfere with the calculator buffer,
             ;; use 3 lines instead.
             (if (and (fboundp 'face-attr-construct)
                      (let* ((dh (plist-get (face-attr-construct 'default) :height))
                             (mf (face-attr-construct 'modeline))
                             (mh (plist-get mf :height)))
                        ;; If the modeline is shorter than the default,
                        ;; stick with 2 lines.  (It may be necessary to
                        ;; check how much shorter.)
                        (and
                         (not
                          (or (and (integerp dh)
                                   (integerp mh)
                                   (< mh dh))
                              (and (numberp mh)
                                   (not (integerp mh))
                                   (< mh 1))))
                         (or
                          ;; If the modeline is taller than the default,
                          ;; use 3 lines.
                          (and (integerp dh)
                               (integerp mh)
                               (> mh dh))
                          (and (numberp mh)
                               (not (integerp mh))
                               (> mh 1))
                          ;; If the modeline has a box with non-negative line-width,
                          ;; use 3 lines.
                          (let* ((bx (plist-get mf :box))
                                 (lh (plist-get bx :line-width)))
                            (and bx
                                 (or
                                  (not lh)
                                  (> lh 0))))
                          ;; If the modeline has an overline, use 3 lines.
                          (plist-get (face-attr-construct 'modeline) :overline)))))
               -3 -2)))
           (switch-to-buffer calculator-buffer)))
        ((not (eq (current-buffer) calculator-buffer))
         (select-window (get-buffer-window calculator-buffer)))))

(provide 'window-config)
;;; window-config.el ends here
