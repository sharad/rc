;;; file-utils.el --- files

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad @>
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

(defun dirname-of-file (file &optional final-slash)
  ;; (ido-no-final-slash
  (if final-slash
      (expand-file-name
       (file-name-directory file))
      (directory-file-name
       (expand-file-name
        (file-name-directory file)))))

(defun dir-final-slash (dir &optional noerror)
  (if dir
      (expand-file-name (concat dir "/"))
    (unless noerror
      (error "dir is nil"))))

(defun find-truefile (&optional file)
  "Useful in case of when vc-follow-symlink is nil
to do VC operation."
  (interactive)
  (if buffer-file-name
      (if (not (string-equal buffer-file-name (file-truename buffer-file-name)))
          (find-alternate-file (file-truename buffer-file-name))
        (message "file %s is true file, not doing anything." buffer-file-name))
    (message "No file is associated with buffer.")))

(when nil ;; progn ;; "recursive"
      ;; url: http://turingmachine.org/bl/2013-05-29-recursively-listing-directories-in-elisp.html
      ;;;
      ;;; Recursively list files in a given directory
      ;;;
      ;;; Author:    daniel m german dmg at uvic dot ca
      ;;; Copyright: daniel m german
      ;;; License:   Same as Emacs
      ;;;

      ;; e.g.
      ;; (directory-files-recursive "/home/dmg/git.dmg/projects" "\\.org$" 2 "\\(rip\\|stage\\)")

      (defun directory-files-recursive (directory match maxdepth &optional ignore include-dir)
        "List files in DIRECTORY and in its sub-directories.
   Return files that match the regular expression MATCH but ignore
   files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only
   to depth MAXDEPTH. If zero or negative, then do not recurse"
        (let* ((files-list '())
               (current-directory-list
                (directory-files directory t)))
          ;; while we are in the current directory
          (while current-directory-list
            (let ((f (car current-directory-list)))
              (cond
               ((and
                 ignore ;; make sure it is not nil
                 (string-match ignore f))
                ; ignore
                nil)
                
               ((and
                 (file-regular-p f)
                 (file-readable-p f)
                 (string-match match f))
                (setq files-list (cons f files-list)))
               ((and
                 (file-directory-p f)
                 (file-readable-p f)
                 (not (string-equal ".." (substring f -2)))
                 (not (string-equal "." (substring f -1)))
                 (> maxdepth 0))
                ;; recurse only if necessary
                (setq
                 files-list (append
                             files-list
                             (directory-files-recursive f match (1- maxdepth) ignore include-dir)))
                (when (and
                       include-dir
                       (string-match match f))
                  (setq
                   files-list (cons f files-list))))
               (t)))
            (setq current-directory-list (cdr current-directory-list)))
          files-list)))

;; (defun dirname-of-file (file &optional final-slash)
;;   ;; (ido-no-final-slash
;;   (if final-slash
;;       (expand-file-name
;;        (file-name-directory file))
;;     (directory-file-name
;;      (expand-file-name
;;       (file-name-directory file)))))


;; (defun dir-final-slash (dir &optional noerror)
;;   (if dir
;;       (expand-file-name (concat dir "/"))
;;       (unless noerror
;;           (error "dir is nil"))))

;; (defun dirname-of-file (dir)
;;   (concat
;;    (directory-file-name
;;    (if (file-directory-p dir)
;;        dir
;;        (file-name-directory dir)))
;;    "/"))

(defun call-times (count fn arg)
  (if (eq count 0)
      arg
      (call-times (1- count)
                  fn (funcall fn arg))))

;; (dirname-of-file "/")
;; (call-times 0 'dirname-of-file "/www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html")
;; (call-times 1 'dirname-of-file buffer-file-name)
;; (call-times 2 'dirname-of-file "/")

(defun find-truefile (&optional file)
  "Useful in case of when vc-follow-symlink is nil
to do VC operation."
  (interactive)
  (if buffer-file-name
      (if (not (string-equal buffer-file-name (file-truename buffer-file-name)))
          (find-alternate-file (file-truename buffer-file-name))
          (message "file %s is true file, not doing anything." buffer-file-name))
      (message "No file is associated with buffer.")))





(defun testpa (c x)
  (interactive "PP")
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html
  (message "prefix c %d"
           (if c
               (prefix-numeric-value c)
               0)))



(progn ;; "Tramp read-only file problem"
  (defun set-file-mode-to-truename (file)
    (interactive
     (list (ido-read-file-name "file: " default-directory buffer-file-name)))
    (when (file-remote-p file)
      (let ((tfile (file-truename file)))
        (when (not (string-equal file tfile))
          (let ((fileattr (tramp-get-file-property (tramp-file-connection file) file "file-attributes-integer" 'undef))
                (tfileattr (tramp-get-file-property (tramp-file-connection tfile) tfile "file-attributes-integer" 'undef)))
            ;; (message "XXXX file: %s, fileattr: %s tfile %s, tfileattr: %s" file fileattr tfile tfileattr)
            (if (eq fileattr 'undef)
                (unless (eq tfileattr 'undef)
                  (tramp-set-file-property (tramp-file-connection tfile) tfile "file-attributes-integer" 'undef))
                (unless (string-equal (nth 8 fileattr) (nth 8 tfileattr))
                  (setf (nth 8 tfileattr) (nth 8 fileattr))
                  (tramp-set-file-property (tramp-file-connection tfile) tfile "file-attributes-integer" tfileattr))))))))


  (defadvice set-file-modes (after set-file-mode-to-truename (filename mode) activate)
    (set-file-mode-to-truename filename))

  (when nil
    (ad-disable-advice 'set-file-modes 'after 'set-file-mode-to-truename)
    (ad-update 'set-file-modes)))

;; (deh-require-maybe rcs-backup
;;   check C-h C-f save-buffer
;;   (defun save-buffer-with-rcs ()
;;     ))


(when nil ;; progn ;; "recursive"
      ;; url: http://turingmachine.org/bl/2013-05-29-recursively-listing-directories-in-elisp.html
      ;;;
      ;;; Recursively list files in a given directory
      ;;;
      ;;; Author:    daniel m german dmg at uvic dot ca
      ;;; Copyright: daniel m german
      ;;; License:   Same as Emacs
      ;;;

      ;; e.g.
      ;; (directory-files-recursive "/home/dmg/git.dmg/projects" "\\.org$" 2 "\\(rip\\|stage\\)")

      (defun directory-files-recursive (directory match maxdepth &optional ignore include-dir)
        "List files in DIRECTORY and in its sub-directories.
   Return files that match the regular expression MATCH but ignore
   files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only
   to depth MAXDEPTH. If zero or negative, then do not recurse"
        (let* ((files-list '())
               (current-directory-list
                (directory-files directory t)))
          ;; while we are in the current directory
          (while current-directory-list
            (let ((f (car current-directory-list)))
              (cond
               ((and
                 ignore ;; make sure it is not nil
                 (string-match ignore f))
                ; ignore
                nil)
                
               ((and
                 (file-regular-p f)
                 (file-readable-p f)
                 (string-match match f))
                (setq files-list (cons f files-list)))
               ((and
                 (file-directory-p f)
                 (file-readable-p f)
                 (not (string-equal ".." (substring f -2)))
                 (not (string-equal "." (substring f -1)))
                 (> maxdepth 0))
                ;; recurse only if necessary
                (setq
                 files-list (append
                             files-list
                             (directory-files-recursive f match (1- maxdepth) ignore include-dir)))
                (when (and
                       include-dir
                       (string-match match f))
                  (setq
                   files-list (cons f files-list))))
               (t)))
            (setq current-directory-list (cdr current-directory-list)))
          files-list)))

(when nil
  (directory-files-recursive
   (expand-file-name "~/Documents/CreatedContent/contents/org/tasks/meru")
   "\\.org$" 2 "\\(rip\\|stage\\)"))



(provide 'file-utils)
;;; files-config.el ends here
