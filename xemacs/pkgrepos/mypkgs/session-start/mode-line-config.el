;;; mode-line-config.el --- session setting

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

(deh-section "ModeLineDirtrack"
  ;; emacswiki: http://www.emacswiki.org/emacs/ModeLineDirtrack
  (defun add-mode-line-dirtrack ()
    (add-to-list 'mode-line-buffer-identification
                 '(:propertize (" " default-directory " ") face dired-directory)))
  (add-hook 'shell-mode-hook 'add-mode-line-dirtrack))

;; http://stackoverflow.com/questions/778508/emacs-add-hostname-to-mode-line
(let ((pos (cddr (memq 'mode-line-modes mode-line-format))))
  (setcdr pos
          (cons
           '(:eval
             (if (frame-parameter (selected-frame) 'frame-spec-id)
                 (concat
                  (file-name-nondirectory (frame-parameter (selected-frame) 'frame-spec-id))
                  " ")))
            (cons
             '(:eval
               (if (car sidebrain-current-stack)
                   (concat
                    (car sidebrain-current-stack)
                    " ")))
             (cdr pos)))))


(deh-require 'scroll-mode-line-mode
  )

;; (testing
;;  (let* ((x '(a b c d e f))
;;         (pos (memq 'e x)))
;;    (setcdr pos
;;            (cons
;;             'n
;;             (cons
;;              'l
;;              (cdr pos))))
;;    x))


(provide 'mode-line-config)
;;; mode-line-config.el ends here


