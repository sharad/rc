;;; occ-util-common.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

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

(provide 'occ-util-common)


(defvar occ-verbose 0)

(defvar occ-org-clock-persist nil "Control org-clock-persist at time of occ clock-in")
(defvar occ-org-clock-auto-clock-resolution nil "Control occ-org-clock-auto-clock-resolution at time of occ clock-in")


(defvar occ-debug nil "Debug occ")

(defun occ-message (&rest args)
  (apply #'message args))

(defun occ-debug (level &rest args)
  (when occ-debug
    (when (car args)
      (apply #'format args)
      (when (member level '(:emergency :error :warning :debug))
        ;; (apply #'lwarn 'occ level args)
        (apply #'lwarn 'occ level args))
      (unless (eq level :nodisplay)
        (apply #'message args)))))

(defun sym2key (sym)
  (if (keywordp sym)
      sym
    (intern-soft (concat ":" (symbol-name sym)))))
(defun key2sym (sym)
  (if (keywordp sym)
      (intern-soft (substring (symbol-name sym) 1))
    sym))


(defun occ-chgable-p ()
  "Stay with a clock at least 2 mins."
  (if org-clock-start-time
      (let ((clock-duration
             (if (and
                  (stringp org-clock-start-time)
                  (string-equal "" org-clock-start-time))
                 0
               (float-time (time-since org-clock-start-time)))))
        (or
         (< clock-duration 60)
         (> clock-duration 120)))
    t))

;;;###autoload
(defun occ-straight-org-clock-clock-in (clock &optional resume start-time)
  (let ((org-log-note-clock-out nil))
    (progn
     (lotus-org-clock-load-only)
     (prog1
         (let ((org-clock-persist               occ-org-clock-persist)
               (org-clock-auto-clock-resolution occ-org-clock-auto-clock-resolution))
           (org-clock-clock-in clock resume start-time)
           t)
       (setq org-clock-loaded t)))))


(defun occ-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  (let ((helm-always-two-windows nil))
    (completing-read prompt
                     collection
                     predicate
                     require-match
                     initial-input
                     hist
                     def
                     inherit-input-method)))

(cl-defmethod ignore-p ((buff buffer))
  nil)



(defmacro run-unobtrusively (&rest body)
  `(while-no-input
    (redisplay)
    ,@body))


(defmacro run-unobtrusively (&rest body)
  `(let ((retval (while-no-input
                   (redisplay)
                   ,@body)))
     (when (eq retval t)
       (occ-debug :debug "user input %s retval %s" last-input-event retval))
     retval))


(defmacro condition-case-control (enable var bodyform &rest handlers)
  (if enable
      `(condition-case ,var
           ,bodyform
         ,@handlers)
    bodyform))
(put 'condition-case-control 'lisp-indent-function 2)



;; testing verification
(defun occ-files-with-null-regex ()
  (interactive)
  (let ((files
         (remove-if
          #'(lambda (f)
              (with-current-buffer (find-file-noselect f)
                org-complex-heading-regexp))
          (occ-files))))
    (occ-message :debug "files with null regex %s" files)))

;; testing verification;; testing verification
(defun occ-files-not-in-org-mode ()
  (interactive)
  (let ((files
         (remove-if
          #'(lambda (f)
              (with-current-buffer (find-file-noselect f)
                (eq major-mode 'org-mode)))
          (occ-files))))
    (occ-message :debug "files not in org-mode %s" files)))


;;;###autoload
(defun occ-after-save-hook-fun ()
  (let ((file (buffer-file-name)))
    (when (and
           file
           (eq major-mode 'org-mode))
      (if (member*
           file
           (occ-files)
           :test #'(lambda (f1 f2)
                     (string-equal
                      (file-truename f1)
                      (file-truename f2))))
          ;; TODO workaround do complete nil, later change it to optimized.
          ;; TODO update existing occ-collection.tree or occ-collection.list
          (occ-reset-global-tsk-collection)
        (occ-debug :debug "file %s not resetting global-tsk-collection" file)))))


(defvar occ-select-clock-in-operate-label  'occ-operate "should not be null")
(defvar occ-select-clock-in-true-label     'occ-true "should not be null")
(defvar occ-select-clock-in-false-label    'occ-false "should not be null")

(cl-assert occ-select-clock-in-true-label)
(cl-assert occ-select-clock-in-false-label)

(defun occ-select-clock-in-tranform (action)
  "Will make all action except first to return occ-select-clock-in-label."
  (cons
   (cons
    "Select"
    #'(lambda (candidate)
        (cons occ-select-clock-in-operate-label candidate)))
   (mapcar #'(lambda (a)
               (if (consp a)
                   (cons (car a)
                         #'(lambda (candidate)
                             (let ((retval
                                    (funcall (cdr a) candidate)))
                               (cons
                                (if retval
                                    occ-select-clock-in-true-label
                                  occ-select-clock-in-false-label)
                                retval))))
                 #'(lambda (candidate)
                     (funcall a candidate)
                     occ-select-clock-in-label)))
           action)))

(defun occ-select-clock-in-tranformer-fun-transform (tranformer-fun)
  "Will make transformer fun to change action except first to return occ-select-clock-in-label."
  #'(lambda (action
             candidate)
      (occ-select-clock-in-tranform
       (funcall tranformer-fun action candidate))))


;;;###autoload
(defun occ-run-with-global-tsk-collection (fn)
  (if occ-global-tsk-collection
      (funcall fn)
    (add-hook
     'occ-global-tsk-collection-change-hook
     fn)))


;;; occ-util-common.el ends here
