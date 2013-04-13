;;; find-file-config.el --- files

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <>
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



  (defun jcl-file-cache-ido-find-file ()
    "Open a file from the file cache.
First select a file from `file-cache-alist'.  If the file exist
in more than one directory one is asked to select which to open.
If you find out that the desired file is not present in the file
cache then you may want to fallback to normal ido find file with
C-f.
Bind this command to C-x C-f to get:

 C-x C-f         -> Open file in filecache.
 C-x C-f C-f     -> Open file with normal ido.
 C-x C-f C-f C-f -> Open file with vanilla find-file.
"
    (interactive)
    (let* (jcl-ido-text
           (file (let ((ido-setup-hook (cons (lambda ()
                                               (define-key ido-completion-map [(control ?f)]
                                                 (lambda (arg)
                                                   (interactive "P")
                                                   (if jcl-ido-text
                                                       (ido-magic-forward-char arg)
                                                       (setq jcl-ido-text ido-text
                                                             ido-text 'fallback-from-cache
                                                             ido-exit 'done)
                                                       (exit-minibuffer)))))
                                             ido-setup-hook)))
                   (ido-completing-read "Cached File: "
                                        (mapcar 'car file-cache-alist)))))
      (if (eq file 'fallback-from-cache)
          (progn
            (setq minibuffer-history (delete 'fallback-from-cache minibuffer-history))
            (ido-file-internal ido-default-file-method
                               nil
                               nil
                               "Ido Find File: "
                               nil
                               jcl-ido-text))

          ;; don't consider it.
          (let ((record (assoc file file-cache-alist)))
            (find-file
             (expand-file-name
              file
              (if (= (length record) 2)
                  (cadr record)
                  (ido-completing-read (format "Find %s in dir: " file)
                                       (cdr record)
                                       nil
                                       t)))))

          )))

(defvar *find-file-wizard-alist* nil "find-file-wizard-alist")

(defmacro find-file-wizard-add (name ff-fun map magic-key hook ret-variable fun)
  "hook where initial set could be defined\n
"
  `(let ((elt (cons ,name (list :ff-fun ',ff-fun :map ',map :key ',magic-key :hook ',hook :ret-variable ',ret-variable :fun ',fun))))
     (push elt *find-file-wizard-alist*)))


(find-file-wizard-add "ido1"
                      ido-find-file
                      ido-completion-map
                      (kbd "C-x") ;; [(control ?f)]
                      ido-setup-hook
                      ido-text
                      (progn
                        (setq initial-string ido-text
                              ido-text 'fallback
                              ido-exit 'done)
                        (exit-minibuffer)))

;; (setq *find-file-wizard-alist* nil)

;; *find-file-wizard-alist*

(plist-get (cdr (assoc "ido1" *find-file-wizard-alist*)) :map)

(defun find-file-wizard ()
  (interactive)
  (let ((wizard-alist *find-file-wizard-alist*)
        (plist (cdar wizard-alist))
        (file 'fallback)
        initial-string)
    (while (and
            wizard-alist
            (not (eq file 'fallback)))
      (let ((plist (cdar wizard-alist)))
        (setq file
              (let ((:hook (cons (lambda ()
                                   (define-key :map :key
                                     (lambda (arg)
                                       (interactive "P")
                                       :fun
                                       )))
                                 ido-setup-hook)))
                (:ff-fun initial-string))
              wizard-alist (cdr wizard-alist))))
    ;; (exit-minibuffer)
    ;; file
    ))

;; (defun find-file-wizard ()
;;   (let ((file))
;;     (dolist (elt *find-file-wizard-alist*)
;;       (setq file
;;             (let ()
;;               (funcall ))))))

;; (defun find-file-wizard ()
;;   (let ((file))
;;     (loop for (elt1 elt2) on *find-file-wizard-alist*
;;        do (let ()
;;             ))))


;; (loop for (a b) on '(1 2 3 4)
;;    do (message "%s" (cons a b)))

;; (progn
;;   (let ((ido-setup-hook (cons (lambda ()
;;                               (define-key ido-completion-map (kbd "C-x") 'exit-minibuffer))
;;                             ido-setup-hook)))
;;   (ido-completing-read "Cached File: "
;;                        (mapcar 'car file-cache-alist)))
;;   (lusty-file-explorer))



(let* (jcl-ido-text
       (file (let ((ido-setup-hook (cons (lambda ()
                                               (define-key ido-completion-map [(control ?f)]
                                                 (lambda (arg)
                                                   (interactive "P")
                                                   (if jcl-ido-text
                                                       (ido-magic-forward-char arg)
                                                       (setq jcl-ido-text ido-text
                                                             ido-text 'fallback-from-cache
                                                             ido-exit 'done)
                                                       (exit-minibuffer)))))
                                             ido-setup-hook)))
                   (ido-completing-read "Cached File: "
                                        (mapcar 'car file-cache-alist)))))
(provide 'find-file-config)
;;; find-file-config.el ends here
