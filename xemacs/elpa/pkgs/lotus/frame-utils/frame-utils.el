;;; frame-utils.el --- Anything Config  -*- lexical-binding: t; -*-

;; Copyright (C) 2011  Sharad Pratap

;; Author:
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



;; org-donot-try-to-clock-in
;; *frame-session-restore*

(require 'elscreen)

(eval-when-compile
  (defvar frame-utils-notify nil)
  (when (null frame-utils-notify)
    (setq frame-utils-notify
          (lambda (title fmt &rest args)
            (concat title ": "
                    (apply 'message fmt args))))))

;; (defun elscreen--set-alist (symbol key value)
;;   "Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE."
;;   (or (boundp symbol)
;;       (set symbol nil))
;;   (set symbol (elscreen--put-alist key value (symbol-value symbol))))

(defun get-alist (key alist)
  (cdr (assoc key alist)))

(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame  (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
      (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

(defun elscreen--del-alist (key alist)
  "Delete an element whose car equals KEY from ALIST.
Return the modified ALIST."
  (let ((pair (assoc key alist)))
    (if pair
        (delq pair alist)
      alist)))


;; deh-require-maybe elscreen
;; toggle-ibuffer-group
(require 'buffer-utils)

(defvar frame-utils-select-frame-fn #'select-frame-set-input-focus "startup-select-frame-fn alternate value is select-frame")

(defun frame-launcher-internal (ignore-err name args &optional fun)
  (let ((*frame-session-restore* nil)  ;not need to restore elsession for frames
        (org-donot-try-to-clock-in t)) ;no clock require to be clocked-in.
    (let ((f (make-frame (list (cons 'name name))))
          (screennum 0)
          (first-screen t))
      (funcall frame-utils-select-frame-fn f)
      (dolist (a args)
        (when (or first-screen
                  (setq screennum (elscreen-create)))
          (setq first-screen nil)
          (if ignore-err
              (condition-case e
                  (progn
                    (if (if fun
                            (funcall fun a)
                          (funcall a))
                        (sticky-buffer-mode t))
                    (launcher-set-elscreen-altname (format "%s" a) f screennum))
                ('quit  (funcall frame-utils-notify "frame-launcher" "Not able to start %s error %s" a e))
                ('error (funcall frame-utils-notify "frame-launcher" "Not able to start %s error %s" a e)))
            (progn
              (if (if fun
                      (funcall fun a)
                    (funcall a))
                  (sticky-buffer-mode t))
              (launcher-set-elscreen-altname (format "%s" a) f screennum))))))))

  ;;;###autoload
(defun frame-launcher (name args &optional fun)
  (if (progn
        (ignore-errors
          (select-frame-by-name name))
        (equal (get-frame-name) name))
      (funcall frame-utils-notify "frame-launcher" "frame-launcher frame already exists, so not creating another frame.")
    (if nil
        (condition-case e
            (frame-launcher-internal t name args fun)
          ('error (funcall frame-utils-notify "frame-launcher" "Error in creating frame %s" e)))
      (frame-launcher-internal nil name args fun))))

;; (frame-parameter (selected-frame) 'altscreen)
  ;;;###autoload
(defun launcher-set-elscreen-altname (name &optional frame screennum)
  (interactive "sname:")
  (let* ((frame (or frame (selected-frame)))
         (screennum (or screennum (elscreen-get-current-screen)))
         (place (get-alist 'altscreen (frame-parameters frame))))
    (unless (frame-parameter frame 'altscreen)
      (set-frame-parameter frame 'altscreen nil))
    (set-frame-parameter frame 'altscreen
                         (put-alist screennum
                                    name
                                    place))))
  ;;;###autoload
(defun launcher-del-elscreen-altname (&optional frame screennum)
  (interactive "sname: ")
  (let* ((frame (or frame (selected-frame)))
         (screennum (or screennum (elscreen-get-current-screen)))
         (place (get-alist 'altscreen (frame-parameters frame))))
    (unless (frame-parameter frame 'altscreen)
      (set-frame-parameter frame 'altscreen nil))
    (set-frame-parameter frame 'altscreen
                         (elscreen--del-alist screennum place))))
  ;;;###autoload
(defun launcher-get-elscreen-altname (&optional frame screennum)
  (interactive)
  (let* ((frame (or frame (selected-frame)))
         (screennum (or screennum (elscreen-get-current-screen)))
         (altscreen (frame-parameter frame 'altscreen)))
    (if altscreen
        (if (called-interactively-p 'any)
            (message "altcreen: %s" (get-alist screennum altscreen))
          (get-alist screennum altscreen)))))

;; (add-hook 'elscreen-kill-hook #'launcher-del-elscreen-altname)

;; advise (elscreen-kill-internal screen)

(defadvice elscreen-kill-internal (after lanucher-del-altname (screen) activate)
  ;; (message "elscreen-kill-internal: in advise %d" screen)
  (launcher-del-elscreen-altname (selected-frame) screen))

;; (launcher-set-elscreen-altname "test" (selected-frame))
;; (launcher-get-elscreen-altname)
;; (launcher-del-elscreen-altname)


;; deh-section "test"
;;;###autoload
(defun x-wm-hints (frame &optional source)
  (mapcar #'(lambda (field)
              (if (consp field)
                  (+ (lsh (car field) 16) (cdr field))
                field))
          (x-window-property
           "WM_HINTS" frame "WM_HINTS"
           (if source
               source
             (string-to-number (frame-parameter frame 'outer-window-id)))
           nil t)))

;;;###autoload
(defun x-urgency-hinthf (frame arg)
  (let* ((wm-hints (x-wm-hints frame))
         (flags (car wm-hints)))
    (setcar wm-hints (if arg
                         (logior flags #x00000100)
                       (logand flags #xFFFFFEFF)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))


;; deh-section "misc"
;; http://draketo.de/book/export/html/41


;; urgency hint

;; Make Emacs announce itself in the tray.

;; let emacs blink when something interesting happens.
;; in KDE this marks the active Emacs icon in the tray.
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg:

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to make the urgency setting disappear (at least in KDE)."
  (let* ((wm-hints (append (x-window-property
                            "WM_HINTS" frame "WM_HINTS"
                            source nil t) nil))
         (flags (car wm-hints)))
                                        ; (message flags)
    (setcar wm-hints
            (if arg
                (logior flags #x00000100)
              (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

  ;;;###autoload
(defun x-urgent (&optional arg)
  "Mark the current emacs frame as requiring urgent attention.

With a prefix argument which does not equal a boolean value of nil, remove the urgency flag (which might or might not change display, depending on the window manager)."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
    (x-urgency-hint frame (not arg))))


;; frame-to-front

;; Get the current Emacs frame to the front. You can for example call this via emacsclient and set it as a keyboard shortcut in your desktop (for me it is F12):

;; emacsclient -e "(show-frame)"

;; This sounds much easier than it proves to be in the end… but luckily you only have to solve it once, then you can google it anywhere…

  ;;;###autoload
(defun show-frame (&optional frame)
  "Show the current Emacs frame or the FRAME given as argument.

And make sure that it really shows up!"
  (raise-frame)
  ; yes, you have to call this twice. Don’t ask me why…
  ; select-frame-set-input-focus calls x-focus-frame and does a bit of
  ; additional magic.
  (select-frame-set-input-focus (selected-frame))
  (select-frame-set-input-focus (selected-frame)))


(provide 'frame-utils)
;;; frame-utils.el ends here
