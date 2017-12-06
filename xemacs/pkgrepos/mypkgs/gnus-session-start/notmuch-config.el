;;; notmuch-config.el --- sadsa

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

(deh-require-maybe notmuch


  (deh-require-maybe notmuch-address
    ;; http://notmuchmail.org/emacstips/
    (setq notmuch-address-command (expand-file-name "notmuch-addrlookup" "~/bin"))
    (if (file-exists-p notmuch-address-command)
        (notmuch-address-message-insinuate)))

  (defun lotus-gnu-notmuch-current-message-id ()
    (interactive)
    (let ((id (with-current-buffer gnus-original-article-buffer
                (nnheader-narrow-to-headers)
                (message-fetch-field "message-id"))))
      (if (and (eql (aref id 0) ?<)
               (eql (aref id (- (length id) 1)) ?>))
          (subseq id 1 (- (length id) 1))
          id)))

  (defun notmuch-select-tag-with-completion (prompt &rest search-terms)
    (let ((tag-list
           (with-output-to-string
               (with-current-buffer standard-output
                 (apply 'call-process notmuch-command nil t nil "search-tags" search-terms)))))
      (completing-read prompt (split-string tag-list "\n+" t) nil nil nil)))

  (defun notmuch-message-tags ()
    (let ((tag-list
           (with-output-to-string
               (with-current-buffer standard-output
                 (apply 'call-process notmuch-command nil t nil
                        "search --format=text --output=tags"
                        (concat "id:" search-id-string)))))
          tag-list)))

  (defun lotus-notmuch-add-jobapply-tag (tag)
    (interactive
     (list (notmuch-select-tag-with-completion "Tag to add: ")))
    (let ((search-id-string (lotus-gnu-notmuch-current-message-id)))
      (notmuch-call-notmuch-process "tag" (concat "+" "jobapply") (concat "id:" search-id-string)))))


(provide 'notmuch-config)
;;; notmuch-config.el ends here

(testing
 (setq x "<asfddsafdsf>")
 (aref x (- (length x ) 1)))
