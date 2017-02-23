;;; ff-relativedir.el --- files

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

(defun call-times (count fn arg)
  (if (eq count 0)
      arg
      (call-times (1- count)
                  fn (funcall fn arg))))

;; (dirname-of-file "/")
;; (call-times 0 'dirname-of-file "/www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html")
;; (call-times 1 'dirname-of-file buffer-file-name)
;; (call-times 2 'dirname-of-file "/")


(defun get-subdirs (dir pos filename)
  (let* ((tpos 0)
         (epos 0)
         (spos (dotimes (c (1+ (cdr dirnu)) tpos)
                 (string-match (car dirnu) filename tpos)
                 (setq tpos (match-beginning 0)
                       epos (match-end 0))))
         (prefix-filename (substring filename 0 spos))
         (suffix-filename (substring filename epos))
         (subdirs (remove
                   (car dirnu)
                   (remove-if-not
                    #'(lambda (d)
                        (and
                         (file-directory-p (concat prefix-filename d))
                         (not
                          (or
                           (string-equal (concat  ".") d)
                           (string-equal (concat  "..") d)))))
                    (directory-files prefix-filename))))
         (existing-subdirs (remove-if-not
                            #'(lambda (sd)
                                (file-exists-p (concat prefix-filename sd suffix-filename)))
                            subdirs)))
    (message "existing-subdirs %s" existing-subdirs)
    (list existing-subdirs prefix-filename suffix-filename)))

(defun find-same-file-in-relative-dir (&optional updircount)
  (interactive "P")
  (if buffer-file-name
    (let* ((filename (call-times (cond
                                   ((and (consp updircount) (eq (car wprif) 4)) 1)
                                   ((null updircount) 0)
                                   (t (prefix-numeric-value updircount)))
                                 'dirname-of-file buffer-file-name))
           (dircomponents
            (let ((count 0))
              (mapcar
               (lambda (c)
                 (prog1
                     (cons c count)
                   (incf count)))
               (split-string (if (file-directory-p filename)
                                 filename
                                 (dirname-of-file filename)) "/" t))))
           (matchd (ido-completing-read "dir: " (mapcar #'car dircomponents)))
           (matcheddircomponents
            (remove-if-not
             #'(lambda (e)
                 (string-equal (car e) matchd))
             dircomponents))
           (dirnu
            (if (= (length matcheddircomponents) 1)
                (car matcheddircomponents)
                (rassoc
                 (ido-completing-read "which one: " (mapcar #'cdr matcheddircomponents))
                 matcheddircomponents))))
      (let* ((results (get-subdirs (car dirnu) (cdr dirnu) filename))
             (existing-subdirs (car results))
             (prefix-filename (nth 1 results))
             (suffix-filename (nth 2 results))
             (select-subdir (if existing-subdirs
                                (ido-completing-read "select subdir: " existing-subdirs)))
             (selected-file-name (if select-subdir
                                     (concat prefix-filename select-subdir suffix-filename))))
        ;; (testing
        ;;  (message "%s %s %s %s %s %s %s" tpos spos dirnu prefix-filename suffix-filename subdirs existing-subdirs))
        (if selected-file-name
            (progn
              (if (file-directory-p selected-file-name)
                  (let ((default-directory selected-file-name))
                    (call-interactively
                     (or (command-remapping 'find-file)
                         'find-file)))
                  (find-file selected-file-name))
              t)
            (message "No match present."))))
    (message "buffer <%s> is not associate to any file." (current-buffer))))



(provide 'ff-relativedir)
;;; ff-relativedir.el ends here
