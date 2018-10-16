;;
;; scm.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Tue Jan 25 10:49:28 2011 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;




(when (and (require 'host-info)
           sharad-in-office-with-perforce
           (xrequire 'vc-p4))
  )


(autoload 'commit-msg-mode "commit-msg-mode" "Major mode for editing commit messages." t)

(deh-require-maybe git-commit
  ;; http://petereisentraut.blogspot.in/2011/01/git-commit-mode.html
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))
  (add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . commit-msg-mode))
  (add-to-list 'auto-mode-alist '("hg-editor-.*$" . commit-msg-mode)))

(provide 'scm-config)
