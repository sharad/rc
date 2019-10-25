;;; autoinsert+.el --- automatic mode-dependent insertion of text into new files

;; Copyright (C) 1985-1987, 1994-1995, 1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Author: Charlie Martin <crm@cs.duke.edu>
;; Adapted-By: Daniel Pfeiffer <occitan@esperanto.org>
;; Keywords: convenience
;; Maintainer: FSF

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  The following defines an association list for text to be
;;  automatically inserted when a new file is created, and a function
;;  which automatically inserts these files; the idea is to insert
;;  default text much as the mode is automatically set using
;;  auto-mode-alist.
;;
;;  To use:
;;     (add-hook 'find-file-hook 'auto-insert+)
;;     setq auto-insert+-directory to an appropriate slash-terminated value
;;
;;  You can also customize the variable `auto-insert+-mode' to load the
;;  package.  Alternatively, add the following to your .emacs file:
;;  (auto-insert+-mode 1)
;;
;;  Author:  Charlie Martin
;;           Department of Computer Science and
;;           National Biomedical Simulation Resource
;;           Box 3709
;;           Duke University Medical Center
;;           Durham, NC 27710
;;	      (crm@cs.duke.edu,mcnc!duke!crm)

;;; Code:

(provide 'autoinsert+)


(require 'autoinsert)


(defgroup auto-insert+ nil
  "Automatic mode-dependent insertion of text into new files."
  :prefix "auto-insert+-"
  :group 'files
  :group 'convenience
  :link '(custom-manual "(autotype) Autoinsert+ing"))


(defcustom auto-insert+ 'not-modified
  "Controls automatic insertion into newly found empty files.
Possible values:
	nil	do nothing
	t	insert if possible
	other	insert if possible, but mark as unmodified.
Insertion is possible when something appropriate is found in
`auto-insert+-alist'.  When the insertion is marked as unmodified, you can
save it with  \\[write-file] RET.
This variable is used when the function `auto-insert+' is called, e.g.
when you do (add-hook 'find-file-hook 'auto-insert+).
With \\[auto-insert+], this is always treated as if it were t."
  :type '(choice (const :tag "Insert if possible" t)
                 (const :tag "Do nothing" nil)
                 (other :tag "insert if possible, mark as unmodified."
                        not-modified))
  :group 'auto-insert+)

(defcustom auto-insert+-query 'function
  "Non-nil means ask user before auto-insert+ing.
When this is `function', only ask when called non-interactively."
  :type '(choice (const :tag "Don't ask" nil)
                 (const :tag "Ask if called non-interactively" function)
                 (other :tag "Ask" t))
  :group 'auto-insert+)

(defcustom auto-insert+-prompt "Perform %s auto-insert+ion? "
  "Prompt to use when querying whether to auto-insert+.
If this contains a %s, that will be replaced by the matching rule."
  :type 'string
  :group 'auto-insert+)

;; Establish a default value for auto-insert+-directory
(defcustom auto-insert+-directory "~/insert/"
  "Directory from which auto-insert+ed files are taken.
The value must be an absolute directory name;
thus, on a GNU or Unix system, it must end in a slash."
  :type 'directory
  :group 'auto-insert+)


(defvar auto-insert+-alist nil)
(defvar auto-noinsert+-alist nil)
(defvar auto-insert+action-handlers nil "autoinsert+ handlers")


;;(setq auto-insert+-alist nil auto-noinsert+-alist nil)
;;;###autoload
(defun add-auto-insert+action-handler (key handler)
  "add autoinsert+ handler"
  (setq auto-insert+action-handlers
        (plist-put
         auto-insert+action-handlers
         key handler)))

;;;###autoload
(defun auto-insert+choose-action (alist)
  (let ((name (completing-read "Which? " alist)))
    (cdr (assoc name alist))))

;;;###autoload
(defun auto-insert+run-action (action)
  ;; TODO: Add support function with arguments
  (save-window-excursion
    ;; make buffer visible before skeleton or function
    ;; which might ask the user for something
    (switch-to-buffer (current-buffer))
    (mapc
     (lambda (act)
       (let* ((type    (car act))
              (ac      (cdr act))
              (handler (plist-get
                        auto-insert+action-handlers type)))
         (if handler
             (funcall handler ac))))
     action)))


;;;###autoload
(defun auto-insert+-insinuate ()
  (add-auto-insert+action-handler
   :skeleton
   '(lambda (ac) (skeleton-insert ac)))

  (add-auto-insert+action-handler
   :func
   '(lambda (ac) (funcall ac)))

  (add-auto-insert+action-handler
   :plain-file
   #'(lambda (ac)
       (if (file-readable-p ac)
           (insert-file-contents ac)
         (if (file-readable-p (expand-file-name ac auto-insert+-directory))
             (insert-file-contents (expand-file-name ac auto-insert+-directory))
           (error "error: %s file not exists" ac))))))

;;;###autoload
(auto-insert+-insinuate)


;;;###autoload
(defun char-isalpha-p (thechar)
  "Check to see if thechar is a letter"
  (and (or (and (>= thechar ?a)
                (<= thechar ?z))
           (and (>= thechar ?A)
                (<= thechar ?Z)))))

;;;###autoload
(defun char-isnum-p (thechar)
  "Check to see if thechar is a number"
  (and (>= thechar ?0)
       (<= thechar ?9)))

;;;###autoload
(defun char-isalnum-p (thechar)
  (or (char-isalpha-p thechar)
      (char-isnum-p thechar)))

;; (mapcar (lambda (s)
;;           (remove-if-not 'char-isalpha-p s))
;;         (mapcar 'car auto-mode-alist))

;; ? How to unify mode (cdr auto-mode-alist) and file names


;;;###autoload
(defun auto-mode-alist-get-modes-from-file (file) ;return list
  )

;;;###autoload
(defun auto-mode-alist-get-fileregexs-from-mode (file) ;return list
  )

;;;###autoload
(defun regex-equal (r1 r2)
  (string-equal
   (remove-if-not 'char-isalpha-p r1)
   (remove-if-not 'char-isalpha-p r2)))


(when nil
  (regex-equal
   "\\.el\\'"
   (car (find 'emacs-lisp-mode auto-mode-alist :key 'cdr :test 'eq))))
;; thos fileregex o mode o auto-mode-alist matchs files regx o
;; template o auto-insert-alist than that mode also need also cause
;; that template to be used.


;;;###autoload
(defun auto-mode-alist-get-from-moderegex (inmode tmplregex) ;return list
  ;; inmode: given file mode
  ;; inregx: template file regex
  (let ((mode-regexs (remove-if-not (lambda (e)
                                      (eq e inmode)) auto-mode-alist :key 'cdr)))
    (remove-if-not '(lambda (e)
                      (regex-equal (car e) tmplregex))
                   mode-regexs)))


;; (defun auto-mode-alist-get-modes-from-moderegex-p (inmode tmplregex) ;return list
;;   ;; inmode: given file mode
;;   ;; inregx: template file regex
;;   (let ((mode-regexs (find inmode auto-mode-alist :key 'cdr :test 'eq)))
;;     (member* tmplregex
;;               mode-regexs
;;               :test '(lambda (e)
;;                       (regex-equal (car e) tmplregex)))))


;;;###autoload
(defun auto-mode-alist-get-from-strmode (infile tmplmode) ;return list
  ;; inmode: given file mode
  ;; inregx: template file regex
  (let ((mode-regexs (remove-if-not (lambda (regex)
                                      (string-match regex infile))
                                    auto-mode-alist :key 'car)))
    (member* tmplmode mode-regexs :key 'cdr)))

;; TODO: checkout pcre2el.el
;; TODO: check major mode with reverse auto-mode-alist file pattern.


;;;###autoload
(defun auto-insert+ (&optional force)
  "Insert default contents into new files if variable `auto-insert+' is non-nil.
Matches the visited file name against the elements of `auto-insert+-alist'."
  (interactive "P")
  ;; (message "in auto-insert+")
  (when
      (and
       (not buffer-read-only)
       (or (eq this-command 'auto-insert+)
           (and auto-insert+
                (or force (and (bobp) (eobp)))))
       ;; no action test
       (let ((alist auto-noinsert+-alist)
             noaction-alist cond)
         (while alist
           (setq cond (caar alist))
           (if (if (symbolp cond)
                   (let ((cond-major-mode cond))
                     (eq cond-major-mode major-mode))
                 (let ((file-regex-cond cond))
                   (and buffer-file-name
                        (string-match file-regex-cond buffer-file-name))))
               (setq
                noaction-alist (append noaction-alist (caar alist))
                alist nil))
           (setq alist (cdr alist)))
         ;; (message "noaction-alist %s" noaction-alist)
         (not noaction-alist)))

    ;; all actions test and accumulation
    (let ((alist auto-insert+-alist)
          case-fold-search
          cond
          desc
          action-alist)
      (goto-char 1)
      ;; find all matching alist entry
      (while alist
        (let* ((element (car alist))
               (cond    (car element))
               (newdesc (plist-get (cdr element) :desc)))
          ;; (message "cond %s newdesc %s" cond newdesc)
          (if (some (lambda (c)
                      (if (symbolp c)
                          (let ((cond-major-mode c))
                            (or (eq cond major-mode)
                                (auto-mode-alist-get-from-strmode buffer-file-name cond-major-mode)))
                        (let ((file-regex-cond c))
                          (or
                           (and buffer-file-name
                                (string-match cond buffer-file-name))
                           (auto-mode-alist-get-from-moderegex major-mode file-regex-cond)))))
                    (if (consp cond) cond (list cond)))
              (setq action-alist (append action-alist (plist-get (cdr element) :action-alist))
                    desc (concat
                          desc
                          (if newdesc
                              (format (concat (if desc " or") " %s") newdesc))))))
        (setq alist (cdr alist)))
      ;; (message "action-alist %s" action-alist)
      ;; Now, if we found something, do it
      (if (and action-alist
               (or (not auto-insert+-query)
                   (if (eq auto-insert+-query 'function)
                       (eq this-command 'auto-insert+))
                   (y-or-n-p (format auto-insert+-prompt desc))))
          (auto-insert+run-action (auto-insert+choose-action action-alist)))

      (and (buffer-modified-p)
           (not (eq this-command 'auto-insert+))
           (set-buffer-modified-p (eq auto-insert+ t)))))
  ;; Return nil so that it could be used in
  ;; `find-file-not-found-hooks', though that's probably inadvisable.
  nil)


;;;###autoload
(defun define-auto-insert+ (pattern-mode desc name actkey action &optional after)
  "Associate CONDITION with (additional) ACTION in `auto-insert+-alist'.
Optional AFTER means to insert action after all existing actions for CONDITION,
or if CONDITION had no actions, after all other CONDITIONs."
  (unless (stringp name)
    (error "name arg not string"))
  (unless action
    (error "action arg can not be nil"))
  (unless (assoc pattern-mode auto-insert+-alist)
    (push (cons pattern-mode nil) auto-insert+-alist))
  (let* ((elt (assoc pattern-mode auto-insert+-alist)))
    (when elt
      (setcdr elt (plist-put (cdr elt) :desc desc))
      (let* ((action-alist (plist-get (cdr  elt)
                                      :action-alist)))
        (unless (assoc name action-alist)
          (push (list name) action-alist))
        (let ((name-action (assoc name action-alist)))
          (if name-action
              (setcdr name-action
                      (if after
                          (append (cdr name-action) (list (cons actkey action)))
                        (push (cons actkey action) (cdr name-action))))))
        (setcdr elt
                (plist-put (cdr  elt)
                           :action-alist
                           action-alist))))))


;;;###autoload
(defun set-auto-insert+noaction (condition)
  "Set pattern where auto-insert+ should not be performed."
  (unless (member condition auto-noinsert+-alist)
    (push condition auto-noinsert+-alist)))


;;;###autoload
(with-eval-after-load 'autoinsert
  ;; (require 'autoinsert)
  (defun action-type-old-autoinsert-alist (action)
    ;; TODO: Add support function with arguments
    (if action
        (cond
         ((and
           (stringp action)
           (or
            (file-readable-p action)
            (file-readable-p
             (expand-file-name
              action auto-insert+-directory))))
          :plain-file)
         ((and (consp action)
               (not (eq (car action) 'lambda)))
          :skeleton)
         ((or
           (symbolp action)
           (and
            (consp action)
            (eq (car action) 'lambda)))
          :func)
         (t ;; (error "action is recognizable.")
          :unknown))
      (error "action is nil")))

  (defun add-from-autoinsert-alist (name alist)
    (dolist (elt alist)
      (let ((pattern-mode (if (consp (car elt))
                              (caar elt)
                            (car elt)))
            (desc         (if (consp (car elt))
                              (cdar elt)
                            (if (symbolp (car elt))
                                (symbol-name (car elt))
                              (car elt))))
            (action       (cdr elt))
            (type         (action-type-old-autoinsert-alist (cdr elt))))
        (if (vectorp (cdr elt))
            (dolist (a (append (cdr elt) nil))
              (define-auto-insert+ pattern-mode desc name type a t))
          (define-auto-insert+ pattern-mode desc   name type action t)))))

  (add-from-autoinsert-alist "old" auto-insert-alist))


;;;###autoload
(defvar tmpledir-alist
  '(("yastemp" "~/emacs.d/template.d/autoinsert" )
    ("autoinsert" "~/emacs.d/template.d/autoinsert")
    ("template" "~/emacs.d/template.d/template")))

;;;###autoload
(defun template-buffer ()
  (tmpldir )
  ())


;; auto-insert+-alist

;;;###autoload
(define-minor-mode auto-insert+-mode
  "Toggle auto-insert+ mode, a global minor mode.
With a prefix argument ARG, enable auto-insert+ mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When auto-insert+ mode is enabled, when new files are created you can
insert a template for the file depending on the mode of the buffer."
  :global t :group 'auto-insert+
  :init-value nil
  (if (and arg
           (if (> (prefix-numeric-value arg) 0)
               auto-insert+-mode (not auto-insert+-mode)))

      (add-hook 'find-file-hook 'auto-insert+)
    (remove-hook 'find-file-hook 'auto-insert+)))

;;; autoinsert.el ends here
