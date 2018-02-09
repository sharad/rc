;;; lotus-misc-utils.el --- copy config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@merunetworks.com>
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
(defun lotus-new-lower-win-size ()
  ;; TODO: improve it.
  ;; If the mode line might interfere with the calculator
  ;; buffer, use 3 lines instead.
  (if (and
       (fboundp 'face-attr-construct)
       (let* ((dh (plist-get (face-attr-construct 'default) :height))
              (mf (face-attr-construct 'mode-line))
              (mh (plist-get mf :height)))
         ;; If the mode line is shorter than the default,
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
           ;; If the mode line is taller than the default,
           ;; use 3 lines.
           (and (integerp dh)
                (integerp mh)
                (> mh dh))
           (and (numberp mh)
                (not (integerp mh))
                (> mh 1))
           ;; If the mode line has a box with non-negative line-width,
           ;; use 3 lines.
           (let* ((bx (plist-get mf :box))
                  (lh (plist-get bx :line-width)))
             (and bx
                  (or
                   (not lh)
                   (> lh 0))))
           ;; If the mode line has an overline, use 3 lines.
           (plist-get (face-attr-construct 'mode-line) :overline)))))
      -12 -10))

;; create smaller and proper sized window
(defun lotus-make-new-win ()
  (let ((size (lotus-new-lower-win-size))
        (window-min-height 7))
    (prog1
        (split-window-below size)
      (message "size %d" size))))


(defmacro lotus-with-new-win (newwin &rest body)
  `(let ()
     (lexical-let* ((,newwin (lotus-make-new-win)))
       ;; maybe leave two lines for our window because of the
       ;; normal `raised' mode line
       (select-window ,newwin 'norecord)
       (progn
         ,@body))))
(put 'lotus-with-new-win 'lisp-indent-function 1)

(defmacro lotus-with-timed-new-win (timeout timer cleanupfn-newwin cleanupfn-local newwin &rest body)
  (lexical-let ((temp-win-config (make-symbol "test-lotus-with-timed-new-win-config")))
    `(lexical-let* ((,temp-win-config (current-window-configuration))
                    (,cleanupfn-newwin #'(lambda (w localfn)
                                           ;; (message "cleaning up newwin and triggered timer for newwin %s" w)
                                           (when localfn (funcall localfn))
                                           ;; (when (active-minibuffer-window) ;not required here. it is just creating timed new-win
                                           ;;   (abort-recursive-edit))
                                           (when (and w (windowp w) (window-valid-p w))
                                             (delete-window w))
                                           (when ,temp-win-config
                                             (set-window-configuration ,temp-win-config)
                                             (setq ,temp-win-config nil)))))
       (lotus-with-new-win ,newwin
         (lexical-let* ((,timer (run-with-idle-timer ,timeout nil
                                                     ,cleanupfn-newwin
                                                     ,newwin
                                                     ,cleanupfn-local)))
           (condition-case err
               (progn
                 ,@body)
             ((quit)
              (funcall ,cleanupfn-newwin ,newwin ,cleanupfn-local))))))))
(put 'lotus-with-timed-new-win 'lisp-indent-function 1)


(provide 'lotus-misc-utils)
;;; lotus-misc-utils.el ends here
