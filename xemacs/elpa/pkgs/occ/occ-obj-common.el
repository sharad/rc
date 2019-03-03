;;; occ-obj-common.el --- occ-api               -*- lexical-binding: t; -*-
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

(provide 'occ-obj-common)


(require 'cl-macs)
(require 'cl-generic)



;; TODO org-base-buffer

;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode

;; "org tsks accss common api"
;; (defvar org-)
(defvar occ-verbose 0)

(defvar occ-org-clock-persist nil "Control org-clock-persist at time of occ clock-in")
(defvar occ-org-clock-auto-clock-resolution nil "Control occ-org-clock-auto-clock-resolution at time of occ clock-in")

(defun occ-debug (level &rest args)
  (when (car args)
    (apply #'format args)
    (when (member level '(:emergency :error :warning :debug))
      (apply #'lwarn 'occ level args)))
  (unless (eq level :nodisplay)
   (apply #'message args)))

(when nil ;; https://curiousprogrammer.wordpress.com/2010/07/19/emacs-defstruct-vs-other-languages/

  (defun cl-get-field (object field)
    (cl-struct-slot-value (cl-classname object) field object))

  (defun cl-set-field (object field value)
    (setf (cl-struct-slot-value (cl-classname object) field object) value))

  (get-field dave 'name)
  (set-field dave 'name "Simon Smith"))

(defun sym2key (sym)
  (if (keywordp sym)
      sym
    (intern-soft (concat ":" (symbol-name sym)))))
(defun key2sym (sym)
  (if (keywordp sym)
      (intern-soft (substring (symbol-name sym) 1))
    sym))
(defun cl-classname (inst)
  (intern
   (substring
    (symbol-name (aref inst 0))
    (length "cl-struct-"))))
(defun cl-get-field (object field)
  (cl-struct-slot-value (cl-classname object) field object))
(defun cl-set-field (object field value)
  (setf (cl-struct-slot-value (cl-classname object) field object) value))
(defun cl-class-slots (class)
  (mapcar
   #'(lambda (slot) (aref slot 1))
   (cl--struct-class-slots
    (cl--struct-get-class class))))
;; (defun cl-class-slot-value (obj slot)
;;   (when (member slot (cl-class-slots (cl-classname obj)))
;;     (cl-struct-slot-value (cl-classname obj) slot obj)))
(defun cl-class-obj-slot-value (class slot obj)
  (when (member slot (cl-class-slots class))
    (cl-struct-slot-value class slot obj)))
(defun cl-obj-slot-value (obj slot)
  (cl-class-obj-slot-value (cl-classname obj) slot obj))
(defun cl-obj-plist-value (obj)
  (cl-obj-slot-value obj 'plist))

(defmacro cl-method-param-case (method param-spec val)
  `(let ((methods (cl--generic ,method)))
     (remove
      nil
      (mapcar
       #'(lambda (fspec)
           (pcase (aref fspec 1)
             (,param-spec ,val)
             (_ nil)))
       (when methods (aref methods 3))))))

(defun cl-method-first-arg (method)
  (let ((methods (cl--generic method)))
    (mapcar
     #'(lambda (fspec) (cadar (aref fspec 1)))
     (when methods (aref methods 3)))))
(defun cl-method-first-arg-with-value (method obj)
  (let ((methods (cl--generic method)))
    (mapcar
     #'(lambda (fspec)
         (let ((first-arg (cadar (aref fspec 1))))
           (when (funcall method (cons first-arg obj)) first-arg)))
     (when methods (aref methods 3)))))


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
  (progn
    (lotus-org-clock-load-only)
    (prog1
        (let ((org-clock-persist               occ-org-clock-persist)
              (org-clock-auto-clock-resolution occ-org-clock-auto-clock-resolution))
          (org-clock-clock-in clock resume start-time))
      (setq org-clock-loaded t))))

(defmacro occ-find-library-dir (library)
  `(file-name-directory
    (or
     "~/.xemacs/elpa/pkgs/occ/occ.el"
     (locate-library ,library)
     "")))

(defun occ-version (&optional here full message)
  "Show the Org version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive (list current-prefix-arg t (not current-prefix-arg)))
  (let ((occ-dir (ignore-errors (occ-find-library-dir "occ")))
        (save-load-suffixes (when (boundp 'load-suffixes) load-suffixes))
        (load-suffixes (list ".el"))
        (occ-install-dir
         (ignore-errors (occ-find-library-dir "occ-loaddefs"))))
    (unless (and
             (fboundp 'occ-release)
             (fboundp 'occ-git-version))
      (org-load-noerror-mustsuffix (concat occ-dir "occ-version")))
    (let* ((load-suffixes save-load-suffixes)
           (release (occ-release))
           (git-version (occ-git-version))
           (version (format "Org mode version %s (%s @ %s)"
                            release
                            git-version
                            (if occ-install-dir
                                (if (string= occ-dir occ-install-dir)
                                    occ-install-dir
                                  (concat "mixed installation! "
                                          occ-install-dir
                                          " and "
                                          occ-dir))
                              "org-loaddefs.el can not be found!")))
           (version1 (if full version release)))
      (when here (insert version1))
      (when message (message "%s" version1))
      version1)))

(defun occ-reload (&optional uncompiled)
  "Reload all Org Lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions."
  (interactive "P")
  (require 'loadhist)
  (let* ((occ-dir     (occ-find-library-dir "occ"))
         ;; (contrib-dir (or (occ-find-library-dir "org-contribdir") occ-dir))
         ;; (feature-re "^\\(org\\|ob\\|ox\\)\\(-.*\\)?")
         (feature-re "^\\(occ\\|okk\\)\\(-.*\\)?")
         (remove-re (format "\\`%s\\'"
                            (regexp-opt '("org" "org-loaddefs" "occ-version"))))
         (feats (delete-dups
                 (mapcar 'file-name-sans-extension
                         (mapcar 'file-name-nondirectory
                                 (delq nil
                                       (mapcar 'feature-file
                                               features))))))
         (lfeat (append
                 (sort
                  (setq feats
                        (delq nil (mapcar
                                   (lambda (f)
                                     (if (and (string-match feature-re f)
                                              (not (string-match remove-re f)))
                                         f nil))
                                   feats)))
                  'string-lessp)
                 (list "occ-version" "occ")))
         (load-suffixes (when (boundp 'load-suffixes) load-suffixes))
         (load-suffixes (if uncompiled (reverse load-suffixes) load-suffixes))
         load-uncore load-misses)
    (setq load-misses
          (delq 't
                (mapcar (lambda (f)
                          (or (org-load-noerror-mustsuffix (concat occ-dir f))
                              ;; (and (string= occ-dir contrib-dir)
                              ;;      (org-load-noerror-mustsuffix (concat contrib-dir f)))
                              (and (org-load-noerror-mustsuffix (concat (occ-find-library-dir f) f))
                                   (add-to-list 'load-uncore f 'append)
                                   't)
                              f))
                        lfeat)))
    (when load-uncore
      (message "The following feature%s found in load-path, please check if that's correct:\n%s"
               (if (> (length load-uncore) 1) "s were" " was") load-uncore))
    (if load-misses
        (message "Some error occurred while reloading Org feature%s\n%s\nPlease check *Messages*!\n%s"
                 (if (> (length load-misses) 1) "s" "") load-misses (occ-version nil 'full))
      (message "Successfully reloaded Org\n%s" (occ-version nil 'full)))))



(when nil
  (defmacro cl-method-first-arg-x (method param-spec val)
    `(let ((methods (cl--generic ,method)))
       (mapcar
        #'(lambda (fspec)
            (pcase (aref fspec 1)
              (,param-spec ,val)
              (_ nil)))
        (when methods (aref methods 3)))))

  (macroexpand-1
   '(cl-method-first-arg-x 'occ-readprop `((head ,val) occ-ctx) val))


  (let ((methods (cl--generic (quote occ-readprop))))
    (mapcar
     (function (lambda (fspec) (pcase fspec ((\` ((head (\, val)) occ-ctx)) val) (_ nil))))
     (when methods (aref methods 3))))


  (let ((methods (cl--generic (quote occ-readprop))))
    (mapcar
     (function
      (lambda (fspec)
        (pcase (aref fspec 1)
          (`((head ,val) occ-ctx) val)
          (_ nil))))
     (when methods
       (aref methods 3))))



  (cl-method-param-case 'occ-readprop `((head ,val) occ-ctx) val)

  '( `(x))







  (cl-method-first-arg-x 'occ-readprop `((head ,val) occ-ctx) 'val)



  (setq xxnaaa (aref (cl--generic 'occ-readprop) 3)))


;; (occ-reload)
;;; occ-obj-common.el ends here
