;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Author: sharad <sh4r4d _at_ _G-mail_>
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


;; (when (configuration-layer/package-usedp 'planner)
;;   (defun spacemacs/planner-enable ()
;;     (progn ;;
;;       (use-package startup-hooks
;;           :defer t
;;           :config
;;           (progn
;;             (add-hook 'lotus-enable-startup-interrupting-feature-hook ;; '*lotus-after-init-hook*
;;                       '(lambda ()
;;                         (with-eval-after-load "planner-registry"
;;                           (progn
;;                             (setq planner-registry-file "~/.emacs.d/autoconfig/planner/planner-registry.el")
;;                             (save-excursion
;;                               (save-window-excursion
;;                                 (plan 2)
;;                                 (planner-registry-insinuate)))))))))))

;;   (defun spacemacs/planner-disable ()
;;     (progn
;;       ))

;;   (spacemacs/planner-enable))



;; (provide 'config)
;;; config.el ends here
