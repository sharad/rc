;;; winconfig.el --- Window Configurations

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



(deh-section "Toggle Article Window"
  ;; from http://www.emacswiki.org/emacs/GnusAndPine
  ;; from http://www.emacswiki.org/emacs/GnusAndPine#toc4
  ;;{{ http://cvlab.epfl.ch/~tola/files/code/dotgnus
  ;; ;; layout of the gnus layout display ; 3-pane format
  (gnus-add-configuration
   '(article
     (horizontal 1.0
      (vertical 25
       (group 1.0))
      (vertical 1.0
       (summary 0.25 point)
       (article 1.0)))))

  (gnus-add-configuration
   '(summary
     (horizontal 1.0
      (vertical 25
       (group 1.0))
      (vertical 1.0
       (summary 1.0 point)))))

  ;; Add info configuration also for function `gnus-info-find-node'

  ;; gnus-buffer-configuration

;;}}


  (defun toggle-article-window ()
    (interactive)
    (let
        ((article-buffer (car
                         (remove-if-not '(lambda (bn)
                                          (string-match "*Article" bn 0)) (mapcar #'buffer-name (buffer-list))))))
      (if (and article-buffer
               (get-buffer-window article-buffer nil))
          (gnus-configure-windows 'summary 'force)
          (gnus-configure-windows 'article 'force)))))

(provide 'winconfig-config)
;;; winconfig.el ends here
