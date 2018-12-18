;;; paths-mapper.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sharad

;; Author: Sharad <sh4r4d at _Gmail_ >
;; Keywords: abbrev

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

(require 's)                            ;https://github.com/magnars/s.el

(defvar paths-mapper-map nil)

(setq paths-mapper-map nil)

(defun paths-mapper-filter-path (path)
  (when path
    (let* ((matched-paths (mapcar
                           #'(lambda (p)
                               (when (s-starts-with? (car p) path)
                                 (if (string-equal (car p) path)
                                     (cdr p)
                                   (when (file-name-directory path)
                                    (expand-file-name
                                     (s-chop-prefix (concat (car p) "/") path) ;fixit
                                     (cdr p))))))
                           paths-mapper-map))
           (existing-matched-paths (remove-if-not #'file-exists-p (remove nil matched-paths))))
      (message "filering [%s] for %s" existing-matched-paths path)
      (if existing-matched-paths
          (car existing-matched-paths)
        path))))

(defun rl-string-len-compare (s1 s2)
  (> (length (car s1)) (length (car s2))))

(defun paths-mapper-add-replacement (path replacement)
  ;; remove common suffix
  (let* ((suffix       (s-shared-end path replacement))
         (spath        (s-chop-suffix suffix path))
         (sreplacement (s-chop-suffix suffix replacement)))
    (push (cons spath sreplacement) paths-mapper-map)
    (setq paths-mapper-map
          (sort paths-mapper #'rl-string-len-compare))))

(defun paths-mapper-read-replacement (path &optional again)
  (let ((modpath (read-file-name
                  (format
                   (if again
                       "again replacement for %s: "
                       "replacement for %s: ")
                   path)
                  (dirname-of-file path))))
    (if (and
         (file-name-directory path)
         (string-equal
          (file-name-nondirectory path)
          (file-name-nondirectory modpath)))
        path
      (progn
        (message "wrong %s read for %s, read again" modpath path)
        (paths-mapper-read-replacement path t)))))

(defun find-missing-file-paths-mapper-params (marker filename directory &rest formats)
  (let* ((path (if (stringp directory)
                  (expand-file-name filename directory)
                 filename))
         (filtered-path (paths-mapper-filter-path path)))
    (if (and filtered-path
             (stringp filtered-path)
             (file-exists-p filtered-path))
        (append
          (list marker
                (paths-mapper-filter-path filename)
                (paths-mapper-filter-path directory))
          formats)

      (progn
        (paths-mapper-add-replacement
         path
         (paths-mapper-read-replacement path))
        (find-missing-file-paths-mapper-params marker filename directory formats)))))

(defun compilation-find-file-fix-remote-local-path-map (orig-fun &rest args)
  (let* (;; (marker (nth 0 args))
         (filename (nth 1 args))
         (directory (nth 2 args))
         ;; (formats (nthcdr 3 args))
         (path (if (stringp directory)
                 (expand-file-name filename directory)
                filename)))

    (if (file-exists-p path)
        (let ((res (apply orig-fun args)))
           (message "display-buffer returned %S" res)
           res)
      (let ((mod-args (apply #'find-missing-file-paths-mapper-params args)))
        (message "org[%s] mod[%s]" args mod-args)
        (apply orig-fun mod-args)))))

;;;###autoloads
(defun paths-mapper-insinuate ()
  (interactive)
  (advice-add 'compilation-find-file :around #'compilation-find-file-fix-remote-local-path-map))

;;;###autoloads
(defun paths-mapper-uninsinuate ()
  (interactive)
  (advice-remove 'compilation-find-file #'compilation-find-file-fix-remote-local-path-map))

(provide 'paths-mapper)
;;; paths-mapper.el ends here
