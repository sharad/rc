;;; vc-config.el --- Vsrsion Control

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad>
;; Keywords: files

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


(require 'general-testing)

(deh-section  "vc check rcs-backup-mode also"
 (setq
  vc-follow-symlinks
  ;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
  t)


 (defvar vc-donot-follow-symlinks '() "")

 (defadvice vc-find-file-hook (before disable-vc-follow-symlinks () activate)
   ;; (message "disable-vc-follow-symlinks %s" vc-follow-symlinks)
   (if (and
        buffer-file-name
        (string-match "\\.gpg$" buffer-file-name))
       (set (make-local-variable 'vc-follow-symlinks) nil))))


(unless vc-follow-symlinks
  (add-hook 'find-file-hook
            #'(lambda ()
                (let ((file-truename (file-truename buffer-file-name))
                      (vc-backend (vc-backend file-truename)))
                  (when (and
                         (not vc-follow-symlinks)
                         (or
                          (file-symlink-p buffer-file-name)
                          (not (string-equal file-truename buffer-file-name))))
                    (vc-mode-line file-truename vc-backend))))))

(testing
 ;; TODO: check
 ;; http://www.emacswiki.org/emacs/log-edit-fill
 ;; http://www.emacswiki.org/emacs/VcAddLogEntries

)


(provide 'vc-config)
;;; vc-config.el ends here
