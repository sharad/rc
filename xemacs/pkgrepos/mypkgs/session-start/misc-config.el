;;; misc-config.el --- Auto Loads

;; Copyright (C) 2015  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Started on  Thu Mar  3 16:12:21 2011 Sharad Pratap
;; $Id$
;; Keywords:convenience

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

;; misc config

;;; Code:

(require 'misc-utils)








(defmacro deh-require-maybe (feature &rest forms)
  "Dot Emacs require Maybe"
  (declare (indent 1))
  (labels ((refine (feature)
             (if (consp feature)
                 (cond
                   ((or (equal (car feature) 'or)
                        (equal (car feature) 'and)
                        (equal (car feature) 'progn))
                    `(,(car feature) ,@(mapcar #'refine (cdr feature))))
                   (t feature))
                 `(require ',feature nil t))))
    `(progn
       (when ,(refine feature)
         ,@(if (stringp (car forms))
               (cdr forms)
               forms)))))

(defmacro deh-require-maybe (feature &rest forms)
  (let ((file-name (file-name-nondirectory (buffer-file-name)))
        (file-name-sym
         (if (string-match "\\(.\+\\)\.el" file-name)
             (intern (match-string 1 lib))))

        (let ((package-list-name (intern (format "configuration|common|%s|packages|list" file-name-sym)) ))
          (if (boundp )))

        `(progn
           (progn)
           (defun configuration|common|FILE|packages ()
             )))))


(progn
  (defun insert-with-newline (fmt &rest args)
    (insert (apply #'format fmt args))
    (newline-and-indent))

  (defun generate-defvar (name)
    (insert-with-newline "(defvar %s nil)" name))

  (defun generate-use-package (pkg)
    (insert-with-newline "(use-package %s" pkg)
    (insert-with-newline ":defer t")
    (insert-with-newline ":config")
    (insert ")"))

  (defun generate-fun (fun &optional fn arg)
    (insert-with-newline generate-autoload-cookie)
    (insert-with-newline "(defun %s ()" fun)
    (if fn (funcall fn arg))
    (insert-with-newline ")")
    (insert-with-newline ";; (defun %s () ...)" fun)
    (newline))

  (defun generate-push (filename-sym lib)
    (interactive
     (let* ((filename (file-name-nondirectory (buffer-file-name)))
            (filename-sym
             (if (string-match "\\(.\+\\)\.el" filename)
                 (intern (match-string 1 filename))))
            (lib (read-from-minibuffer "lib name: "))
            (lib (if (string-equal lib "") "LIB" lib)))
       (list filename-sym lib)))

    (let ((package-list (format "configuration|common|%s|package-list" filename-sym)))
      (insert-with-newline "(push '%s %s)" lib package-list)))

  (defun generate-lib (filename-sym lib)
    (interactive
     (let* ((filename (file-name-nondirectory (buffer-file-name)))
            (filename-sym
             (if (string-match "\\(.\+\\)\.el" filename)
                 (intern (match-string 1 filename))))
            (lib (read-from-minibuffer "lib name: "))
            (lib (if (string-equal lib "") "LIB" lib)))
       (list filename-sym lib)))
    (newline)
    (let ()
      (message "filename-sym %s" filename-sym)
      (if filename-sym
          (let
              ((config (format "configuration|common|%s|%s|config" filename-sym lib))
               (init (format "configuration|common|%s|%s|init" filename-sym lib)))
            (generate-fun config
                         (lambda (arg)
                           (generate-use-package arg))
                         lib)
            (generate-fun init
                         (lambda (arg)
                           ;; (insert-with-newline "(%s)" arg)
                           (insert (format "(%s)" arg)))
                         config)
            (generate-push filename-sym lib)))))



  (defun generate-file (filename)
    (interactive
     (let* ((filename (file-name-nondirectory (buffer-file-name))))
       (list filename)))

    (let ((filename-sym
           (if (string-match "\\(.\+\\)\.el" filename)
               (intern (match-string 1 filename)))))

      (if filename-sym
          (let
              ((package-list (format "configuration|common|%s|package-list" filename-sym))
               (config (format "configuration|common|%s|config" filename-sym))
               (init (format "configuration|common|%s|init" filename-sym))
               (packages (format "configuration|common|%s|packages" filename-sym)))

            (dotimes (x 17) (newline))

            (generate-defvar package-list)

            (dotimes (x 3) (newline))

            (generate-lib filename-sym "LIB")

            (dotimes (x 3) (newline))

            (generate-fun config)
            (dotimes (x 2) (newline))

            (generate-fun init
                         (lambda (arg)
                           ;; (insert-with-newline "(%s)" arg)
                           (insert (format "(%s)" arg)))
                         config)

            (generate-push filename-sym "LIB1")
            (generate-fun packages
                         (lambda (arg)
                           ;; (insert-with-newline "%s" arg)
                           (insert (format "%s" arg)))
                         package-list))))))

(provide 'misc-config)
;;; misc-config.el ends here
