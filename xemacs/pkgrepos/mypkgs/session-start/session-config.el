;;; session-config.el --- session setting

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


(deh-require-maybe savehist-20+
  )

;;For Session
(deh-require-maybe session ;;
  (add-hook 'after-init-hook 'session-initialize)
  (add-hook 'kill-emacs-hook 'session-save-session)
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
            'le::maybe-reveal)
  ;;}}
  )
;;  (session-initialize))


(provide 'session-config)
;;; session-config.el ends here
