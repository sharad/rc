;;
;; utils.el
;; Login : <s@taj>
;; Started on  Thu Sep  2 02:33:29 2010 Sharad Pratap
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

(defun global-set-key-if-unbind (key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (let ((bindedcmd (key-binding key t)))
    (if bindedcmd
        (when (or (not *emacs-in-init*) (not reloading-libraries))
          (message "key %s already have binded with command %s, can't bind to %s."
                   key bindedcmd cmd))
        (global-set-key key cmd))))

(defun global-set-key-warn-if-bind (key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (let ((bindedcmd (key-binding key t)))
    (if bindedcmd
        (when (or (not *emacs-in-init*) (not reloading-libraries))
          (message "key %s already have binded with command %s, but binding to %s."
                   key bindedcmd cmd)))
        (global-set-key key cmd)))

(defun keymap-set-key-if-unbind (map key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (let ((bindedcmd (key-binding key t)))
    (if bindedcmd
        (when (or (not *emacs-in-init*) (not reloading-libraries))
          (message "key %s already have binded with command %s, can't bind to %s."
                   key bindedcmd cmd))
        (define-key map key cmd))))



(defun fprint (dir)
  "Print the current buffer with same file name."
  (interactive "DDirqectory to put: ")
  (let* ((fname (file-name-nondirectory
                (buffer-file-name)))
         (pname (concat (or dir default-directory) fname ".ps")))
    (ps-print-in-file pname)))



;;{{{ define xrequire

(defun xrequire (feature)
  (unless (member feature exclude-lib)
      (if (not running-xemacs)
          (require feature nil t)
          (require feature nil))))

(defun irequire (feature)
  (ignore-errors
    (unless (member feature exclude-lib)
      (if (not running-xemacs)
          (require feature nil t)
	(require feature nil)))))


;;}}}

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))


(defun afind-if (fun list) ;; anaphoric
  (let ((result
         (funcall fun (car list))))
    (if result
        result
        (if list (afind-if fun (cdr list))))))




(defun run-each-hooks (hook)
  (dolist (f (symbol-value hook))
    (condition-case e
        (funcall f)
      (error (message "run-each-hooks Error: function %s error %s" f e)))))

(defun run-each-debug-hooks (hook)
  (dolist (f (symbol-value hook))
      (error (message "run-each-hooks Error: function %s error %s" f e))))




(when nil
  (defun toignore ()
    (message "asdfds"))


  (defalias 'toignore 'ignore)

  (fset 'alttoignore (symbol-function 'toignore))
  (fset 'ignore (symbol-function 'alttoignore))

  (toignore)
  (alttoignore))

(defvar *undefine-function-alist* nil "undefine-function-alist")

(defun undefine-function-remember (fnsym)
  "Use (redefine-function-remembered fnsym) to redefine."
  (unless (eq fnsym 'ignore)
   (push (cons fnsym (symbol-function fnsym))
         *undefine-function-alist*)
   (defalias fnsym 'ignore)))

(defun redefine-function-remembered (fnsym)
  "Use (undefine-function-remember fnsym) to undefine."
  (let ((fdef (assoc fnsym *undefine-function-alist*)))
    (if fdef
        (progn
          (fset fnsym (cdr fdef))
          (setq *undefine-function-alist*
                (del-alist (car fdef) *undefine-function-alist*)))
        (message "def for %s function is not available." fnsym))))



;; (add-hook 'aa-hook (lambda ()
;;                      (message "dsafds")))

;; (add-hook 'aa-hook (lambda ()
;;                      (message "errorQQ")
;;                      (error "Err")))


;; (run-each-hooks 'aa-hook)


;; (deh-section "dep"
(when nil
  (defadvice require (around compile-if-fail (feature &optional filename noerror) activate)
    (let ((ret (condition-case e
                   ad-do-it
                 ('error nil))))
      (if ret
          ret
          (let ((file (or
                       filename
                       (locate-library (symbol-name feature)))))

            (load-file file)
            (byte-compile-file file)
            ;; (require feature filename noerror)
            ))))

  (when nil
    (ad-disable-advice 'require 'around 'compile-if-fail)
    (ad-remove-advice 'require 'around 'compile-if-fail)
    (ad-update 'require)))
