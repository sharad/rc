;;; note.el --- sdfgsdfg

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d@gmail.com>
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


(when (xrequire 'evernote-mode)
  ;; http://emacs-evernote-mode.googlecode.com/svn-history/r190/trunk/doc/readme_en.html#sec-7
  ;; (setq evernote-username "") ; optional: you can use this username as default.
  (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")
        evernote-password-cache t) ; option

  ;; (global-set-key "\C-cec" 'evernote-create-note)
  ;; (global-set-key "\C-ceo" 'evernote-open-note)
  ;; (global-set-key "\C-ces" 'evernote-search-notes)
  ;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
  ;; (global-set-key "\C-cew" 'evernote-write-note)
  ;; (global-set-key "\C-cep" 'evernote-post-region)
  ;; (global-set-key "\C-ceb" 'evernote-browser)

  (add-to-list 'anything-sources anything-c-source-evernote-title))

(provide 'note-config)
;;; note.el ends here
