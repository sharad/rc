;;; dirs-config.el --- Dirs related config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords: lisp, convenience

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


;; from http://www.emacswiki.org/emacs/DirectoryVariables

;; Emacs (23.2.1) will not attempt to apply directory local
;; variables to buffers visiting remote files (via tramp).

;; We can advise ‘hack-dir-local-variables’ to work around this, if
;; you are willing to incur the associated performance cost (which
;; can be relatively minimal in some situations).

;; Enable directory local variables with remote files. This facilitates both
;; the (dir-locals-set-class-variables ...)(dir-locals-set-directory-class ...)
;; and the dir-locals.el approaches.



(require 'dir-locals)

(defadvice hack-dir-local-variables (around my-remote-dir-local-variables)
  "Allow directory local variables with remote files, by temporarily redefining
     `file-remote-p' to return nil unconditionally."
  (flet ((file-remote-p (&rest) nil))
    ad-do-it))

(ad-activate 'hack-dir-local-variables)


;; As of Emacs 24.3, you can set the variable
;; ‘enable-remote-dir-locals’ to apply such values on remote files. If
;; you’re using an earlier version of Emacs, read on:

(setq enable-remote-dir-locals t)

(dir-locals-set-class-variables
 'plone-core
 '((nil . ((buffer-read-only . t)))))

(dir-locals-set-class-variables
 'plone-instance
 '((nil . ((indent-tabs-mode . nil)
           (fill-column . 80)))
   ;; (python-mode . (()))
   ;; (nxhtml-mode . (()))
   ))





(provide 'dirs-config)
;;; dirs-config.el ends here
