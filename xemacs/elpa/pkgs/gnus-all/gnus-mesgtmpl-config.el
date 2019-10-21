;;; gnus-mesgtmpl-config.el --- sdf

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords:

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

(defvar message-template-directory "~/.xemacs/gnustmpls/")

(defun gnus-insert-temple ()
  (interactive)
  (let
      ((file (concat
              message-template-directory "/"
              (ido-completing-read "template: "
                                   (mapcar
                                    #'file-name-nondirectory
                                    (directory-files message-template-directory t "[a-zA-Z]+"))))))
    ;; (message-change-subject NEW-SUBJECT)
    (message-goto-body)
    (insert
     (with-temp-buffer
       ;; (template-expand-template file)
      (template-new-file-0 file)
      (buffer-string)))))


(defun gnus-create-temple ()
  (interactive)
  (let (startbody endbody)

    (save-excursion
      (message-goto-body)
      (setq startbody (point))
      (message-goto-signature)
      (previous-line)
      )


    (template-simple-expand-template file)))



(provide 'gnus-mesgtmpl-config)
;;; gnus-mesgtmpl-config.el ends here
