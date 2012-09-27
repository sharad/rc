;;; flymake-config.el --- Flymake Config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d@gmail.com>
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

(deh-require-maybe flymake
  (setq
   ;; http://stackoverflow.com/questions/2571436/emacs-annoying-flymake-dialog-box
   ;; flymake-gui-warnings-enabled nil       ;need to know.
   flymake-gui-warnings-enabled t       ;need to know.
   flymake-run-in-place
   ;; https://github.com/illusori/emacs-flymake/issues/1
   t))


(provide 'flymake-config)
;;; flymake.el ends here
