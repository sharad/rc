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



(deh-require-maybe ediff

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


(deh-section "binary file hexal mode"
  ;; http://trey-jackson.blogspot.in/2010/10/emacs-tip-38-automatically-diff-binary.html
  (defvar ediff-do-hexl-diff nil
    "variable used to store trigger for doing diff in hexl-mode")
  (defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
    "catch the condition when the binary files differ

the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
    (let ((file-A (ad-get-arg 0))
          (file-B (ad-get-arg 1))
          ediff-do-hexl-diff)
      (condition-case err
          (progn
            ad-do-it)
        (error
         (if ediff-do-hexl-diff
             (let ((buf-A (find-file-noselect file-A))
                   (buf-B (find-file-noselect file-B)))
               (with-current-buffer buf-A
                 (hexl-mode 1))
               (with-current-buffer buf-B
                 (hexl-mode 1))
               (ediff-buffers buf-A buf-B))
             (error (error-message-string err)))))))

  (defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
    "when binary files differ, set the variable "
    (condition-case err
        (progn
          ad-do-it)
      (error
       (setq ediff-do-hexl-diff
             (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                  (error-message-string err))
                  (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))
                  (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
       (error (error-message-string err))))))

(provide 'ediff-config)

