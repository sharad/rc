;;; recent-config.el --- recent

;; Copyright (C) 2011  Sharad Pratap

;; Author:
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



(deh-require-maybe recentf
  (setq
   recentf-save-file (auto-config-file "recentf/recentf")
   recentf-exclude (list (regexp-opt '(".org$" ".rem$")))
   recentf-max-saved-items 99
   ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2007-07/msg00007.html
   ;; I do not want to clean up especially trampfiles.
   recentf-auto-cleanup 'never

   ;; http://stackoverflow.com/a/2069425/341107
   ;; I had problem with recentf and remote file when the remote host was gone.
   recentf-keep '(file-remote-p file-readable-p)
   ;; May solve your problem (remote file will be kept without testing if they still exists).
   )
  ; Add to the "File" menu a list of recently opened files.
  (if (not running-xemacs)
      ;;displays this menu in a buffer
      (recentf-mode 1))

  (deh-section "Entries for files that were never displayed"
    ;; http://www.emacswiki.org/RecentFiles#toc16
    ;; Entries for files that were never displayed

    ;; If you, for example, use CEDET, the recentf package may be fairly
    ;; useless by default. The problem is that CEDET can scan lots of
    ;; source files and make files in the process of building a tags
    ;; database or managing the build system. The recentf list is then
    ;; saturated with files you haven’t displayed or edited on screen.

    ;; This is fixed in the CVS version of CEDET.

    ;; The following is a workaround.

    (defsubst file-was-visible-p (file)
      "Return non-nil if FILE's buffer exists and has been displayed."
      (let ((buf (find-buffer-visiting file)))
        (if buf
            (let ((display-count (buffer-local-value 'buffer-display-count buf)))
              (if (> display-count 0) display-count nil)))))

    (defsubst keep-default-and-visible-recentf-p (file)
      "Return non-nil if recentf would, by default, keep FILE, and
FILE has been displayed."
      (if (recentf-keep-default-predicate file)
          (file-was-visible-p file)))

    ;; When a buffer is closed, remove the associated file from the recentf
    ;; list if (1) recentf would have, by default, removed the file, or
    ;; (2) the buffer was never displayed.  This is useful because, for
    ;; example, CEDET opens a lot of files in the background to generate
    ;; its tags database, etc.
    (setq recentf-keep '(keep-default-and-visible-recentf-p)))

  (deh-section "Undo kill buffer"
    ;; http://www.emacswiki.org/RecentFiles#toc17
    ;; Undo kill buffer

    ;; The Opera web browser has a surprisingly useful feature called the
    ;; “trash can”. After you close a tab you can “undo” the close with
    ;; C-z. As it turns out when I want a tab reopened it’s usually one of
    ;; the last few I closed, so I almost never end up looking in the
    ;; history. I missed this feature in Emacs, so I implemented it on top
    ;; of recentf.

    (defun undo-kill-buffer (arg)
      "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
      (interactive "p")
      (let ((recently-killed-list (copy-sequence recentf-list))
            (buffer-files-list
             (delq nil (mapcar (lambda (buf)
                                 (when (buffer-file-name buf)
                                   (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
        (mapc
         (lambda (buf-file)
           (setq recently-killed-list
                 (delq buf-file recently-killed-list)))
         buffer-files-list)
        (find-file
         (if arg (nth arg recently-killed-list)
             (car recently-killed-list)))))))

(when recentf-save-file
  (deh-require-maybe recentf-ext))


(deh-require-maybe recentf-buffer)


(provide 'recent-config)
;;; recent-config.el ends here
