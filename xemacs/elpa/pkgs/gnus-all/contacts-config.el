;;; contacts-config.el --- Contacts

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

(deh-require-maybe bbdb


  (setq bbdb-file (expand-file-name "bbdb/bbdb" "~/.emacs.d/.cache/autoconfig"))

  (defun bbdb/gnus-pop-up-bbdb-buffer-for-some-time ()
    (if (functionp 'bbdb/gnus-pop-up-bbdb-buffer)
        (progn
          (bbdb/gnus-pop-up-bbdb-buffer)
          ;; (with-selected-window (get-buffer-window gnus-article-buffer)
          ;;   (gnus-summary-goto-subject (cdr gnus-article-current)))
          (let ((win-bbdb (get-buffer-window "*BBDB*")))
            (when win-bbdb
              ;; (run-at-time "4 sec" nil #'delete-window w))))
              (run-at-time "4 sec" nil #'(lambda (w)
                                           (if (and
                                                (windowp w)
                                                (window-valid-p w))
                                               ;; (old-delete-window w)
                                               (progn
                                                 (delete-window w)
                                                 (message "deleted %s window" w))))
                           win-bbdb))))
      (message "#'bbdb/gnus-pop-up-bbdb-buffer is not present in bbdb3 use some other function for popup.")))

  (define-key gnus-summary-mode-map (kbd "s-c s-v")  'bbdb/gnus-pop-up-bbdb-buffer)

  (setq bbdb-use-pop-up t
        bbdb-save-db-timeout 0) ;; I want it
  (remove-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer)
  (add-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer-for-some-time)

  (defun toggle-bbdb-use-pop-up ()
    (interactive)
    (setq
     bbdb-use-pop-up (not bbdb-use-pop-up))))

;;

(provide 'contacts-config)


;;; contacts-config.el ends here

;; (run-at-time "4 sec" nil #'message "Hello")
;; (cancel-function-timers #'message)
