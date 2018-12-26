;;; emacs-panel.el --- http://www.emacswiki.org/emacs/AngryFruitSalad

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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

;; http://lists.gnu.org/archive/html/emacs-devel/2011-06/msg00340.html

;;; Code:



  ;; (x-window-property "_NET_CURRENT_DESKTOP" nil nil 0 nil nil)
  ;; (x-window-property "_NET_NUMBER_OF_DESKTOPS" nil nil 0 nil nil)
  ;; (x-window-property "_NET_DESKTOP_NAMES" nil nil 0 nil nil)
  ;; (x-window-property "_NET_WM_NAME" nil nil 0 nil t)
  ;; (x-window-property "STUMPWM_WCLI" nil nil 0 nil t)

  ;; (fmsession-read-location-internal)

  ;; (x-window-property "_NET_DESKTOP_NAMES" nil nil 0 nil nil)


(progn ;;
  "test"
  ;; http://lists.gnu.org/archive/html/emacs-devel/2011-06/msg00340.html
  (require 'bindat)

  (defmacro emacs-panel-x-property (prop window &optional type vec)
    `(x-window-property ,prop nil ,(or type "AnyPropertyType") ,window nil ,vec))

  (defmacro emacs-panel-x-property-nullsepstringarray (prop window &optional type)
    `(split-string (emacs-panel-x-property ,prop ,window ,type) "\0" t))

  (defmacro emacs-panel-x-property-u32r (prop window &optional type)
    `(let ((spec '((:v u32r)))
           (bin (emacs-panel-x-property ,prop ,window ,type)))
       (cdr-safe (assq :v (bindat-unpack spec bin)))))

  (defmacro emacs-panel-x-property-u32r (prop window &optional type)
    `(let ((spec '((:v u32r)))
           (bin (emacs-panel-x-property ,prop ,window ,type t)))
       bin))


  (defun emacs-panel-wm-hints ()
    ;; ask the root window what window to query for the WM name
    (let* ((nqid (emacs-panel-x-property-u32r "_NET_SUPPORTING_WM_CHECK" 0))
           (name (when nqid (emacs-panel-x-property "_NET_WM_NAME" nqid))))
      `((name ,name)
        (desktop-names
         ,@(emacs-panel-x-property-nullsepstringarray "_NET_DESKTOP_NAMES" 0))
        (active-window
         ,(emacs-panel-x-property-u32r "_NET_ACTIVE_WINDOW" 0))
        (desktop-count
         ,(emacs-panel-x-property-u32r "_NET_NUMBER_OF_DESKTOPS" 0))
        (current-desktop
         ,(emacs-panel-x-property-u32r "_NET_CURRENT_DESKTOP" 0))))))




(when nil
(progn
  ;; (emacs-panel-x-property-u32r "_NET_SUPPORTING_WM_CHECK" 0)
  ;; (x-window-property "_NET_SUPPORTING_WM_CHECK" nil "AnyPropertyType" 0 nil nil)
  ;; (x-window-property PROP &optional FRAME TYPE SOURCE DELETE-P VECTOR-RET-P)
  ;; (x-window-property "_NET_SUPPORTING_WM_CHECK" (selected-frame) "AnyPropertyType" 0 nil nil)
  ;; (x-window-property "_NET_SUPPORTING_WM_CHECK" (selected-frame) "AnyPropertyType" 0 nil nil)

  ;; (bindat-unpack '((:v u32r)) "0x194")

  (emacs-panel-wm-hints)

  ;; (x-window-property "_NET_SUPPORTING_WM_CHECK" nil "AnyPropertyType" 0 nil nil)

  (cdr-safe (assq :v (bindat-unpack '((:v u32r)) "x194")))

  (emacs-panel-x-property "_NET_WM_NAME" 404)

  (emacs-panel-x-property-nullsepstringarray "_NET_DESKTOP_NAMES" 0)

  (string-to-number (frame-parameter nil 'outer-window-id))

  8388680

  (emacs-panel-x-property-u32r "_NET_ACTIVE_WINDOW" 0)

  (emacs-panel-x-property-u64r "_NET_NUMBER_OF_DESKTOPS" 0)

  (x-window-property "_NET_ACTIVE_WINDOW" nil "AnyPropertyType" 0 nil t)

  (x-window-property "_NET_WM_NAME" nil "AnyPropertyType" 0 nil t)

  (x-window-property "_NET_SUPPORTING_WM_CHECK" nil "AnyPropertyType" 0 nil t)


  (x-window-property "_NET_SUPPORTING_WM_CHECK" nil "AnyPropertyType" 0 nil t)

  (assq :v (bindat-unpack '((:v byte)) ""))

  (assq :v (bindat-unpack '((:v byte)) "H"))
  ))

(provide 'emacs-panel)
;;; emacs-panel.el ends here
