;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

;; http://www.emacswiki.org/emacs/SwitchingBuffers
;; Flipping Buffers in Two Frames
;; In Emacs you can do many things at once in multiple
;; frames (outside Emacs frames are sometimes called “windows”). For
;; instance: If you have multiple screens on your machine you can
;; open individual Emacs frames for each screen. Each frame contains
;; its own buffers and of course each frame can be split into Emacs
;; windows.
;; I wrote the following bit of Emacs lisp in order to switch the
;; contents of two open frames. This is very useful for me at work
;; since I use a two monitor setup. Sometimes I want to edit
;; whatever is on the other monitor on my “main” monitor and
;; vise-versa.
(defun switch-buffers-between-frames ()
  "switch-buffers-between-frames switches the buffers between the two last frames"
  (interactive)
  (let ((this-frame-buffer nil)
	(other-frame-buffer nil))
    (setq this-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (other-frame 1)
    (setq other-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (switch-to-buffer this-frame-buffer)
    (other-frame 1)
    (switch-to-buffer other-frame-buffer)))
;; http://www.emacswiki.org/emacs/SwitchingBuffers
;; Transposing Two Buffers
;; If you have a window split showing two buffers, you can transpose
;; the two buffers:
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))


;;{{  No need
;; The following function changes the way you switch buffers. You are
;; presented with a list of buffers that shrinks as you type the name,
;; only the matching buffers are shown, making buffer switching much
;; easier.
;; iswitchb
;; (when (functionp 'iswitchb-default-keybindings)
;; ;;  (iswitchb-mode nil)
;; ;;  (iswitchb-default-keybindings))
;; )
;;}}




(progn ;; "Enable recursive minibuffer"
  (defun status-recursive-minibuffers ()
      (if enable-recursive-minibuffers
        (message "recursive minibuffers enabled")
        (message "recursive minibuffers disabled")))
  (defun enable-recursive-minibuffer ()
    (interactive)
    (setq enable-recursive-minibuffers t)
    (status-recursive-minibuffers))
  (defun disable-recursive-minibuffer ()
    (interactive)
    (setq enable-recursive-minibuffers t)
    (status-recursive-minibuffers))
  (defun toggle-recursive-minibuffer ()
    (interactive)
    (setq enable-recursive-minibuffers (not enable-recursive-minibuffers))
    (status-recursive-minibuffers)))

;;; config.el ends here
