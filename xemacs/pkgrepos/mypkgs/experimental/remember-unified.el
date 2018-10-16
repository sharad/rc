;;; remember-unified.el --- remember-unified               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

(require 'tree)
(require 'loadhist)

(require 'remember)
(require 'org)
(if (featurep 'org-remember)
    (require 'org-remember))
(require 'planner)
(require 'remember-planner)

;; (require 'read-file-name)
;; (require 'remember-blosxom)
;; (require 'remember-experimental) ;; will start mail at daemon startup time.


(require 'remember-autoloads)
;; (require 'remember-diary) ; merged in remember.el
(require 'remember-planner)
(require 'remember-bbdb)



;; (require 'remember-bibl) ; - check it
;; (require 'macs-wiki-journal)

(defvar lotus-remember-functions-alist nil "")

(defvar remember-organizer 'planner "")

(defvar ad-remember-mode-after-hook nil "")

(when nil ;;get interactive


  (nth 4 (indirect-function 'find-file))

  (nth 2 (aref  (indirect-function 'remember) 5))


  (nth 3  (indirect-function 'remember))



  (defun help-function-interactive (def)
    ;; Handle symbols aliased to other symbols.
    (if (and (symbolp def) (fboundp def)) (setq def (indirect-function def)))
    ;; If definition is a macro, find the function inside it.
    (if (eq (car-safe def) 'macro) (setq def (cdr def)))
    (cond
      ((byte-code-function-p def) (append (nth 2 (aref def 5)) nil))
      ((eq (car-safe def) 'lambda) (nth 3 def))
      ((and (eq (car-safe def) 'autoload) (not (eq (nth 4 def) 'keymap)))
       "[Arg list not available until function definition is loaded.]")
      (t t)))

  (append  [ 1 2 ] ())

  (help-function-interactive 'find-file)
  (help-function-interactive 'remember)


  )



;; (reduce #'list '(1 2 3 4)
;;         :initial-value 'foo)

;; (defun run-list-until-success (flist)
;;   (some 'funcall flist))



(defun remember-fun-set-orgnizer-advice (fun adname)
  (unless (ad-find-advice fun 'around adname)
    (eval
     `(defadvice ,fun (around ,adname ,(help-function-arglist fun) activate)
        ;; ,(help-function-interactive 'fun)
        (let ((remember-annotation-functions
               (tree-node  lotus-remember-functions-alist remember-organizer 'annotation :test 'equal))
              (remember-handler-functions
               (tree-node lotus-remember-functions-alist remember-organizer 'handler :test 'equal))
              (remember-mode-hook
               (tree-node lotus-remember-functions-alist remember-organizer 'hook :test 'equal)))
          ad-do-it))))
  (ad-enable-advice fun 'around adname)
  (ad-activate fun)
  (ad-update fun))

(defun remember-fun-unset-orgnizer-advice (fun adname)
  (when (ad-find-advice fun 'around adname)
    (ad-remove-advice fun 'around adname))
  (ad-activate fun)
  (ad-update fun))

(defun remember-fun-disable-orgnizer-advice (fun adname)
  (when (ad-find-advice fun 'around adname)
    (ad-disable-advice fun 'around adname))
  (ad-activate fun)
  (ad-update fun))

(defun remember-fun-enable-orgnizer-advice (fun adname)
  (when (ad-find-advice fun 'around adname)
    (ad-enable-advice fun 'around adname))
  (ad-activate fun)
  (ad-update fun))

(defun remember-set-orgnizer-advice ()
  (interactive)
  (setq remember-annotation-functions nil
        remember-handler-functions nil
        remember-mode-hook nil)
  (dolist (fun (mapcar 'cdr
                       (remove-if-not
                        '(lambda (e)
                          (and (consp e)
                           (eq 'defun (car e))))
                        (feature-symbols 'remember))))
    (remember-fun-set-orgnizer-advice fun 'Ad-organizer)))

(defun remember-manage-orgnizer-advice (mgrfn)
  (interactive
   (let*
       ((fnnames '("remember-fun-set-orgnizer-advice"
                   "remember-fun-unset-orgnizer-advice"
                   "remember-fun-enable-orgnizer-advice"
                   "remember-fun-disable-orgnizer-advice"))
        (fn (ido-completing-read "manager: " fnnames nil t)))
     (list (intern fn))))
  (setq remember-annotation-functions nil
        remember-handler-functions nil
        remember-mode-hook nil)
  (dolist (fun (mapcar 'cdr
                       (remove-if-not
                        '(lambda (e)
                          (and (consp e)
                           (eq 'defun (car e))))
                        (feature-symbols 'remember))))
    (funcall mgrfn fun 'Ad-organizer)))

(defun remember-unified-init ()

  (setf (tree-node* lotus-remember-functions-alist 'org 'annotation) '(org-remember-annotation))
  (setf (tree-node* lotus-remember-functions-alist 'planner 'annotation) planner-annotation-functions)
  (setf (tree-node* lotus-remember-functions-alist 'org 'handler) '(org-remember-handler))
  (setf (tree-node* lotus-remember-functions-alist 'planner 'handler) '(remember-planner-append))
  (setf (tree-node* lotus-remember-functions-alist 'org 'hook) '(org-remember-apply-template))
  (setf (tree-node* lotus-remember-functions-alist 'planner 'hook) nil)

  (remember-set-orgnizer-advice))


;; (unless (ad-find-advice 'ccm-first-start 'before 'reset-ccm-vpos)
;;   (defadvice ccm-first-start (before reset-ccm-vpos (animate) activate)
;;     (setq ccm-vpos nil) t))


(defun remember-change-orgnizer (&optional orgnizer)
  (interactive
   (list (when current-prefix-arg
           (buffer-substring (point) (mark)))))
  (let ((old-remember-organizer remember-organizer)
        (organizer
         (or
          orgnizer
          (intern
           (ido-completing-read "Organizer: "
                                (mapcar (lambda (e)
                                          (symbol-name (car e)))
                                        lotus-remember-functions-alist)
                                nil t)))))
    (setq remember-organizer organizer)))

;;;###autoload
(defun dontforgetme (&optional initial)
  (interactive
   (list (when current-prefix-arg
           (buffer-substring (point) (mark)))))
  (let ((old-remember-organizer remember-organizer)
        (organizer (remember-change-orgnizer)))
    (setq remember-organizer organizer)
    (remember initial)
    ;; will not work, think more or live with it.
    ;; (setq remember-organizer old-remember-organizer)
    ))

;; org-ignore-region

;;;###autoload
(defun lotus-remember-org (&optional initial)
  (interactive
   (list (when current-prefix-arg
           (buffer-substring (point) (mark)))))
  (let ((organizer 'org))
    (setq remember-organizer organizer)
    (remember initial)))

;;;###autoload
(defun lotus-remember-planner (&optional initial)
  (interactive
   (list (when current-prefix-arg
           (buffer-substring (point) (mark)))))
  (let ((organizer 'planner))
    (setq remember-organizer organizer)
    (remember initial)))

(progn
  (remember-unified-init)
  (defvar ad-remember-mode-after-hook nil "")
  (defadvice remember-buffer (after remember-mode-after-hook activate)
    (run-hooks  'ad-remember-mode-after-hook)))

(provide 'remember-unified)
;;; remember-unified.el ends here
