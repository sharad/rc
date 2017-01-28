;;; pa.el --- PA

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad at home>
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






(require 'pa-planner)
(require 'planner-interface)


;;{{ start: http://www.emacswiki.org/emacs/.emacs-thierry.el
;; (add-hook 'after-init-hook #'(lambda ()
;;                                (server-start)
;;                                (setq server-raise-frame t)))

;; emacsclient-and-stumpish (to ".emacsclient-and-stumpish")
;; When using emacsclient from external programs, raise emacs and come back
;; to external program when finish
(if window-system
    (add-hook 'server-done-hook
              (lambda ()
                (shell-command "stumpish 'eval (stumpwm::return-es-called-win stumpwm::*es-win*)'"))))
;; end
;;}}




(provide 'pa-config)
;;; pa.el ends here
