;;
;; bindings.el
;; Login : <s@taj>
;; Started on  Sun Jan  9 23:54:46 2011 Sharad Pratap
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


;;{{ from: http://www.emacswiki.org/emacs/GnusAndPine
;; Now we activate the bindings:

(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (local-set-key (kbd "<tab>") 'gnus-summary-next-unread-article)
            (local-set-key "="  'toggle-article-window)
            ;; (local-set-key "n"  'gnus-summary-next-article)
            ;; (local-set-key "p"  'gnus-summary-prev-article)
            ;; (local-set-key "!"  'gnus-summary-put-mark-as-ticked-next)
            ;; (local-set-key "d"  'gnus-summary-put-mark-as-expirable-next)
            ;; (local-set-key "u"  'gnus-summary-clear-mark-forward)
            ;; (local-set-key "r"  'gnus-summary-dwim-reply)
            ;; (local-set-key "R"  'gnus-summary-dwim-reply-with-original)
            ;; ;; creating real problem
            ;; ;; (local-set-key "x"  'gnus-summary-delete-article)
            ;; (local-set-key "g"  'gnus-summary-goto-group)
            ;; (local-set-key "?"  'gnus-info-find-node)
            ;; (local-set-key "l"  'gnus-summary-exit)
            ;; (local-set-key "s"  'gnus-summary-save-and-expire)
            ;; (local-set-key "v"  'gnus-article-view-part)
            ;; (local-set-key "c"  'gnus-summary-mail-other-window)
            ;; (local-set-key "$f" 'gnus-summary-sort-by-author)
            ;; (local-set-key "$a" 'gnus-summary-sort-by-original)
            ;; (local-set-key "$d" 'gnus-summary-sort-by-date)
            ;; (local-set-key "$s" 'gnus-summary-sort-by-subject)
            ;; (local-set-key "$z" 'gnus-summary-sort-by-chars)
            ;; (local-set-key "$e" 'gnus-summary-sort-by-score)
            (if (gnus-news-group-p gnus-newsgroup-name)
                (local-set-key "f"  'gnus-summary-followup)
                (local-set-key "f"  'gnus-summary-mail-forward))))

;;}}

(provide 'bindings-config)
