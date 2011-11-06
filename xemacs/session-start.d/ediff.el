;;
;; ediff.el
;; Login : <s@taj>
;; Started on  Sun Jun  6 16:08:10 2010 Sharad Pratap
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


(defun ediff-write-merge-buffer ()
  (let ((file ediff-merge-store-file))
    (set-buffer ediff-buffer-C)
    (write-region (point-min) (point-max) file)
    (message "Merge buffer saved in: %s" file)
    (set-buffer-modified-p nil)
    (sit-for 1)))

;; (setq ;; ediff-quit-hook 'kill-emacs
;;  ediff-quit-merge-hook 'ediff-write-merge-buffer)

;; (ediff-merge-files-with-ancestor \\\"$LOCAL\\\" \\\"$REMOTE\\\"
;;                                  \\\"$BASE\\\" nil \\\"$MERGED\\\")\"



(deh-require-maybe 'ediff

  (defvar ediff-after-quit-hooks nil
    "* Hooks to run after ediff or emerge is quit.")

  (defadvice ediff-quit (after edit-after-quit-hooks activate)
    (run-hooks 'ediff-after-quit-hooks))

  (setq git-mergetool-emacsclient-ediff-active nil)

  (defun local-ediff-frame-maximize ()
    (let* ((bounds (display-usable-bounds))
           (x (nth 0 bounds))
           (y (nth 1 bounds))
           (width (/ (nth 2 bounds) (frame-char-width)))
           (height (/ (nth 3 bounds) (frame-char-height))))
      (set-frame-width (selected-frame) width)
      (set-frame-height (selected-frame) height)
      (set-frame-position (selected-frame) x y)))

  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)

  (defun local-ediff-before-setup-hook ()
    (setq local-ediff-saved-frame-configuration (current-frame-configuration))
    (setq local-ediff-saved-window-configuration (current-window-configuration))
    ;; (local-ediff-frame-maximize)
    (if git-mergetool-emacsclient-ediff-active
        (raise-frame)))

  (defun local-ediff-quit-hook ()
    (set-frame-configuration local-ediff-saved-frame-configuration)
    (set-window-configuration local-ediff-saved-window-configuration))

  (defun local-ediff-suspend-hook ()
    (set-frame-configuration local-ediff-saved-frame-configuration)
    (set-window-configuration local-ediff-saved-window-configuration))

  (add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
  (add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
  (add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

  ;; Useful for ediff merge from emacsclient.
  (defun git-mergetool-emacsclient-ediff (local remote base merged)
    (setq git-mergetool-emacsclient-ediff-active t)
    (if (file-readable-p base)
        (ediff-merge-files-with-ancestor local remote base nil merged)
        (ediff-merge-files local remote nil merged))
    (recursive-edit))

  (defun git-mergetool-emacsclient-ediff-after-quit-hook ()
    (exit-recursive-edit))

  (add-hook 'ediff-after-quit-hooks 'git-mergetool-emacsclient-ediff-after-quit-hook 'append)
  )

(user-provide 'ediff)

