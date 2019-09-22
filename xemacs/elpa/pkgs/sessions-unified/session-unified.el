;;; session-unified.el --- Session unified           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <spratap@merunetworks.com>
;; Keywords: convenience, tools, internal

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

(provide 'session-unified)



(require 'sessions-unified)


;;For Session
;; (with-eval-after-load "session" ;;

;; (setq desktop-path '("~/.emacs.d/"))
;; (setq desktop-dirname "~/.emacs.d/")
;; (setq desktop-base-file-name
;;       (concat
;;        "emacs-desktop"
;;        (if (boundp 'server-name)
;;            (concat "-" server-name))))

;; (defvar *desktop-save-filename* (expand-file-name desktop-base-file-name desktop-dirname))
(setq session-save-file (auto-config-file "session/session.el"))

(defun lotus-session-saved-session ()
  (if (file-exists-p session-save-file) session-save-file))

(defun session-vc-save-session ()
  (if (lotus-session-saved-session)
      (put-file-in-rcs session-save-file))
  (session-save-session))

(defun session-vc-restore-session ()
  (unless (lotus-session-saved-session)
    (message "lotus-session-vc-session-restore: %s not found so trying to checkout it." session-save-file)
    (vc-checkout-file session-save-file))

  (or session-successful-p
      (setq session-successful-p
            (and session-save-file
                 (condition-case nil
                     (progn
                       ;; load might fail with coding-system = emacs-mule
                       (load session-save-file t nil t)
                       (run-hooks 'session-after-load-save-file-hook)
                       t)
                   (error nil))))))



(add-hook 'after-init-hook
          #'(lambda ()
              (setq session-initialize t)
              (session-initialize)
              (remove-hook 'kill-emacs-hook
                           ;; done in save-all-sessions-auto-save
                           'session-save-session)))
;; (add-hook 'kill-emacs-hook 'session-vc-save-session)


(setq session-initialize t)

;;{{ http://www.emacswiki.org/emacs/EmacsSession

;; There is a function in session that’s not really persistence
;; related – ‘session-jump-to-last-change’ <C-x C-/>. This is the
;; singular most useful function of any Emacs add-on to me. It moves
;; the point to the last modified location. Keep calling it and you
;; will visit all the locations you’ve made
;; modifications. Absolutely brilliant. Unobstrusive, unlike
;; highlight-changes-mode.

;; However, it doesn’t automatically reveal folded sections. Here is
;; the fix:


;; expanded folded secitons as required
(defun le::maybe-reveal ()
  (when (and (or (memq major-mode  '(org-mode outline-mode))
                 (and (boundp 'outline-minor-mode)
                      outline-minor-mode))
             (outline-invisible-p))
    (if (eq major-mode 'org-mode)
        (org-reveal)
      (show-subtree))))

(add-hook 'session-after-jump-to-last-change-hook
          #'le::maybe-reveal)
;;}}
;;  (session-initialize))
;; Something like this is recommended to get emacs to shut-up
;; and never ask you for a coding system. Otherwise this can
;; happen on *every* desktop-save triggered by the auto-save-hook:
(prefer-coding-system 'utf-8)

(add-hook 'delete-frame-functions
          #'(lambda (frame)
              (if (and
                   (< (length (frame-list)) 3)
                   (functionp 'session-save-sessoin))
                  (session-save-sessoin))))


;;; session-unified.el ends here
