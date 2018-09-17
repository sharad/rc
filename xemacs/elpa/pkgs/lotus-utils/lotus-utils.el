;;; lotus-utils.el --- copy config  -*- lexical-binding: t; -*-

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

;; will not require any library, only will provide utils
(provide 'lotus-utils)


(defun touch-file (file)
  ;; https://stackoverflow.com/questions/2592095/how-do-i-create-an-empty-file-in-emacs/2592558#2592558
  (unless (file-exists-p file)
    (make-directory
     (dirname-of-file file) t)
    (with-temp-buffer
      (write-file file)))
  file)

(defun cleanup-tty-process ()
  (interactive)
  (let ((tty-processes
         (remove-if-not
          'process-tty-name
          (process-list))))
    (dolist (tp tty-processes)
      (kill-process tp))))

(defun elscreen-keymap-setup ()
  (progn ;; "Keybinding: Elscreen"
    (when (featurep 'elscreen)
      ;;{{ elscreen
      ;; https://github.com/syl20bnr/spacemacs/issues/7372
      (define-key evil-emacs-state-map (kbd "C-z") nil)
      (global-unset-key [C-z])
      ;; (global-set-key [C-z c] 'elscreen-create)
      (funcall
       #'(lambda (symbol value)
           (when (boundp 'elscreen-map)
             (elscreen-set-prefix-key value))
           (custom-set-default symbol value))
       'elscreen-prefix-key "\C-z")
      (global-set-key [s-right] 'elscreen-next)
      (global-set-key [s-left]  'elscreen-previous)
      (global-set-key [H-right] 'elscreen-move-right)
      (global-set-key [H-left]  'elscreen-move-left)
      (global-set-key [M-H-right]    'elscreen-swap)
      ;; (global-set-key-if-unbind [H-down]  'elscreen-previous)
      ;;}}
      )))

(defun resolveip (host)
  (= 0 (call-process "~/bin/resolveip" nil nil nil host)))

(defun host-accessable-p (&optional host)
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                     (if host host "www.google.com"))))

;;; lotus-utils.el ends here
