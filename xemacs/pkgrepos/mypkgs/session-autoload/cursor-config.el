;;
;; cursor.el
;; Login : <s@taj>
;; Started on  Wed Sep 15 00:21:24 2010 Sharad Pratap
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

(defvar configuration|common|cursor-config|package-list nil)

;;;###autoload
(defun configuration|common|cursor-config|hl-line|config ()
  ;; for the current buffer
  (hl-line-mode)
  ;; for all buffers
  (global-hl-line-mode))

;;;###autoload
(defun configuration|common|cursor-config|hl-line|init ()
    (use-package hl-line
      :defer t
      :config
      (configuration|common|cursor-config|hl-line|config)))
(push 'hl-line configuration|common|cursor-config|package-list)


;;;###autoload
(defun configuration|common|cursor-config|hl-line+|config ()
  ;;{{ from: http://www-sop.inria.fr/everest/Clement.Hurlin/linux.shtml
  ;; 05/08/07, emacs: I find useful to highlight the line (snapshot)
  ;; where the cursor points because it avoids getting lost when
  ;; switching between numerous windows. You can enable this with:
  ;; (hl-line-mode) ;; for the current buffer
  ;; (global-hl-line-mode) ;; for all buffers

  ;; Load this file (it will load `hl-line.el')
  ;;
  ;;  To turn on `global-hl-line-mode' only when Emacs is idle, by
  ;;  default, add this line also to your init file:
  ;;
  ;; Highlight only when idle
  (hl-line-toggle-when-idle 1)
  (hl-line-when-idle-interval 70))

;;;###autoload
(defun configuration|common|cursor-config|hl-line+|init ()
    (use-package hl-line+
      :defer t
      :config
      (configuration|common|cursor-config|hl-line+|config)))
(push 'hl-line+ configuration|common|cursor-config|package-list)




;;;###autoload
(defun configuration|common|cursor-config|config ()
  )
;;;###autoload
(defun configuration|common|cursor-config|init ()
  (configuration|common|cursor-config|config))

;;;###autoload
(defun configuration|common|cursor-config|packages ()
  configuration|common|cursor-config|package-list)



(provide 'cursor-config)
