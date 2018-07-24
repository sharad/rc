;;; buff-trans.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d@gmail.com>
;; Keywords: data

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

;; This package meant to log, analyze and report all emacs activity of
;; user which could further utilized to visualize activity of user
;; during period of time or editing session.

;; Enable Activity for the current buffer by invokingi
;; `activity-mode'. If you wish to activate it globally, use
;; `global-activity-mode'.

;; Set variable `activity-api-key' to your API key. Point
;; `activity-cli-path' to the absolute path of the CLI script
;; (activity-cli.py).

;; See http://nullprogram.com/blog/2013/04/07/ for help
;; add example code directly here for quick reference.

;;; Code:

(require 'activity-base)

(provide 'buff-trans)

(defsubclass-gen@ @transition-dectector-class :gen-buffer-trans (&optional note)
  (def@ @@ :make-event ()
    "Make buffer change event."
    (let ((curr (current-buffer)))
      (message "running :make-event")
      (unless (eql
               @:prev
               curr)
        (@! (@! @:tran :new) :send @:prev curr)
        (setf @:prev curr))))

  (def@ @@ :dispatch (&optional note)
    (setf @:prev (current-buffer))
    (setf @:tran
          (defsubobj@ @transition-class "buffer transition"

            (def@ @@ :send (prev next)
              (@! @:note :send "switched from buffer %s to %s on %s"
                  prev next (@:occuredon)))

            (def@ @@ :dispatch (&optional note)
              (@:init)
              (setf @:note
                    (or note
                        (@! @note-class :gen-format-msg "test"))))

            (@:dispatch note))))
  (@:dispatch note))

(setf @buff-transition-detector
      (@! @transition-dectector-class :gen-buffer-trans "test"))

(defun make-event ()
  (message "running make-event")
  (@! @buff-transition-detector :make-event))

(defun buff-transition-detector-install ()
  (add-hook 'buffer-list-update-hook     'make-event)
  (add-hook 'elscreen-screen-update-hook 'make-event)
  (add-hook 'elscreen-goto-hook          'make-event))

(defun buff-transition-detector-uninstall ()
  (remove-hook 'buffer-list-update-hook     'make-event)
  (remove-hook 'elscreen-screen-update-hook 'make-event)
  (remove-hook 'elscreen-goto-hook          'make-event))


;;; buff-trans.el ends here
