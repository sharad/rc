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
(defvar occ-debug-uncond nil "occ-debug-uncond")


;;;###autoload
(defun occ-enable-debug ()
  (interactive)
  (setq occ-debug t))
;;;###autoload
(defun occ-disable-debug ()
  (interactive)
  (setq occ-debug nil))


(defun occ-debug-uncond (&rest args)
  (when occ-debug-uncond
    (apply #'message args)))


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


(defvar occ-condition-case-control-debug nil)

;;;###autoload
(defun occ-enable-condition-case-control-debug ()
  (interactive)
  (setq occ-condition-case-control-debug t))

;;;###autoload
(defun occ-disable-condition-case-control-debug ()
  (interactive)
  (setq occ-condition-case-control-debug nil))

(defmacro condition-case-control (var bodyform &rest handlers)
  (if (not occ-condition-case-control-debug)
      `(condition-case ,var
           ,bodyform
         ,@handlers)
    bodyform))
(put 'condition-case-control 'lisp-indent-function 1)


(defun downcase-sym (sym)
  (let ((symname (downcase (symbol-name sym))))
    (or
     (intern-soft symname)
     (intern symname))))
(defun upcase-sym (sym)
  (let ((symname (upcase (symbol-name sym))))
    (or
     (intern-soft symname)
     (intern symname))))
(defun sym2key (sym)
  (if (keywordp sym)
      sym
    (or
     (intern-soft (concat ":" (symbol-name sym)))
     (intern (concat ":" (symbol-name sym))))))
(defun key2sym (sym)
  (if (keywordp sym)
      (or
       (intern-soft (substring (symbol-name sym) 1))
       (intern (substring (symbol-name sym) 1)))
    sym))


(defun occ-valid-marker (marker)
  (when (and
         marker
         (marker-buffer marker))
    marker))


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
  ;; lotus-org-with-safe-modification
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
    (occ-debug-uncond "occ-completing-read: prompt %s collection %s"
                      prompt collection)
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
  `(if (called-interactively-p 'any)
       (progn
         ,@body)
     (while-no-input
      (redisplay)
      ,@body)))

(defmacro run-unobtrusively (&rest body)
  `(if (called-interactively-p 'any)
       (progn ,@body)
    (let ((retval (while-no-input
                   (redisplay)
                   ,@body)))
      (when (eq retval t)
        (occ-debug :debug "user input %s retval %s" last-input-event retval))
      retval)))


(defun occ-helm-buffer-p (buffer)
  (string-match "^*helm" (buffer-name buffer)))


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


;;;###autoload
(defun occ-run-with-global-tsk-collection (fn)
  (if occ-global-tsk-collection
      (funcall fn)
    (add-hook
     'occ-global-tsk-collection-change-hook
     fn)))


;;; occ-util-common.el ends here
