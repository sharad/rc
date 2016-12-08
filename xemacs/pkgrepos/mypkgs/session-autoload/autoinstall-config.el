;;
;; autoinstall.el
;; Login : <s@taj>
;; Started on  Thu Sep 16 14:23:43 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;






(defun configuration|common|autoinstall-config|auto-install|config ()
  ;; from: http://www.emacswiki.org/emacs/AutoInstall
  (setq auto-install-use-wget t
        auto-install-directory "~/.xemacs/pkgrepos/autoinstalled/auto-install/")
                                        ; (auto-install-update-emacswiki-package-name t)
  (use-package anything-auto-install
      :defer t
      :config
      (setq anything-sources
          (list
           anything-c-source-auto-install-from-emacswiki
           anything-c-source-auto-install-from-library))))

;;;###autoload
(defun configuration|common|autoinstall-config|auto-install|init ()
    (use-package auto-install
      :defer t
      :config
      (configuration|common|autoinstall-config|auto-install|config)))

;;;###autoload
(defun configuration|common|autoinstall-config|packages ()
  '(auto-install))





















(defvar configuration|common|FILE|package-list nil)


;;;###autoload
(defun configuration|common|FILE|LIB|config ()

  )

;;;###autoload
(defun configuration|common|FILE|LIB|init ()
    (use-package LIB
      :defer t
      :config
      (configuration|common|FILE|LIB|config)))
(push 'LIB configuration|common|FILE|package-list)





;;;###autoload
(defun configuration|common|FILE|LIB|config ()

  )

;;;###autoload
(defun configuration|common|FILE|LIB|init ()
    (use-package LIB
      :defer t
      :config
      (configuration|common|FILE|LIB|config)))
(push 'LIB configuration|common|FILE|package-list)



;;;###autoload
(defun configuration|common|FILE|config ()

  )
;;;###autoload
(defun configuration|common|FILE|init ()
  (configuration|common|FILE|config))

;;;###autoload
(defun configuration|common|FILE|packages ()
  configuration|common|FILE|package-list)

















(provide 'autoinstall-config)
