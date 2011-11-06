;;
;; buffer.el
;; Login : <s@taj>
;; Started on  Tue Jul 27 00:19:15 2010 Sharad Pratap
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


;; Follow mode.
;; Dead useful for wider-than-tall-hires monitors.  Will let you do a
;; C-x 3, then use both windows as a single tall window, just broken in
;; half and displayed side by side.  Neat-o.
;; get it from: http://www.andersl.com/emacs/follow.html


(deh-require-maybe 'follow-mode
  ;; from http://www.dotemacs.de/dotfiles/DavidJolley.emacs.html
  (autoload 'follow-mode) ;; "follow.el" nil t)
  (autoload 'follow-delete-other-windows-and-split) ;; "follow.el" nil t)

;;
;; Some global keys so that I can activate Follow Mode fast.
;;
  (add-hook 'follow-mode-hook 'my-follow-mode-hook))





(deh-require-maybe 'ibuffer
  (global-set-key (kbd "C-x C-b") 'ibuffer) ;force
  ;; (autoload 'ibuffer "ibuffer" "List buffers." t)
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dired" (mode . dired-mode))
           ("perl" (mode . cperl-mode))
           ("erc" (mode . erc-mode))
           ("planner" (or
                       (name . "^\\*Calendar\\*$")
                       (name . "^diary$")
                       (mode . muse-mode)))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))
           ("gnus" (or
                    (mode . message-mode)
                    (mode . bbdb-mode)
                    (mode . mail-mode)
                    (mode . gnus-group-mode)
                    (mode . gnus-summary-mode)
                    (mode . gnus-article-mode)
                    (name . "^\\.bbdb$")
                    (name . "^\\.newsrc-dribble"))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))


(deh-require-maybe 'uniquify
  ;; from http://jasonmbaker.com/7-tools-for-working-with-python-in-emacs-and
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ; rename after killing uniquified
        ; don't muck with special buffers (or Gnus mail buffers
        uniquify-ignore-buffers-re "^\\*"))

(deh-section "Buffer"
  (unless (fboundp 'next-buffer)
    (defun next-buffer ()
      "Switch to the next buffer in cyclic order."
      (interactive)
      (let ((buffer (current-buffer)))
        (switch-to-buffer (other-buffer buffer))
        (bury-buffer buffer))))
  (unless (fboundp 'prev-buffer)
    (defun prev-buffer ()
      "Switch to the previous buffer in cyclic order."
      (interactive)
      (let ((list (nreverse (buffer-list)))
            found)
        (while (and (not found) list)
          (let ((buffer (car list)))
            (if (and (not (get-buffer-window buffer))
                     (not (string-match "\\` " (buffer-name buffer))))
                (setq found buffer)))
          (setq list (cdr list)))
        (switch-to-buffer found)))))

(user-provide 'buffer)

