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



(progn

  (defvar *find-file-wizard-alist* nil "find-file-wizard-alist")

  (defun find-file-wizard-add (name ff setup)
  "hook where initial set could be defined\n
"
  (if (assoc name *find-file-wizard-alist*)
      (setcdr
       (assoc name *find-file-wizard-alist*)
       (list :ff ff :setup setup))
      (let ((elt (cons name (list :ff ff :setup setup))))
        (push elt *find-file-wizard-alist*))))


  (setq *find-file-wizard-alist* nil)

  (find-file-wizard-add "lusty"
      ;; ido-find-file
      (lambda (initstr)
        (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
        (lusty-file-explorer))

      (lambda (ff initial-string)
        (let ((lusty-setup-hook
               (cons
                (lambda ()
                  (define-key lusty-mode-map
                      (kbd "s-f") ;; (plist-get plist :key)
                    (lambda (arg)
                      (interactive "P")
                      (setq initial-string ido-text
                            ;; ido-text 'fallback-wizard
                            ;; ido-exit 'done
                            )
                      ;; (exit-minibuffer)
                      (throw 'nextff (list 'next (list (cons :initial-string initial-string)))))))
                lusty-setup-hook)))
          (funcall ff initial-string))))

  (find-file-wizard-add "idoff"
      ;; ido-find-file
      (lambda (initstr)
        (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
        (ido-find-file))


      (lambda (ff initial-string)
        (let ((ido-setup-hook
               (cons
                (lambda ()
                  (define-key ido-completion-map ;; ido-mode-map
                      (kbd "s-f") ;; (plist-get plist :key)
                    (lambda (arg)
                      (interactive "P")
                      (setq initial-string ido-text
                            ido-text 'fallback-wizard
                            ido-exit 'done)
                      ;; (message "magic Alambda3: ido-text: %s initial-string: %s" ido-text initial-string)
                      ;; (exit-minibuffer)
                      (throw 'nextff (list 'next (list (cons :initial-string initial-string))))
                      ;; (message "magic Alambda3x: ido-text: %s initial-string: %s" ido-text initial-string)
                      )))
                ido-setup-hook)))
          (funcall ff initial-string))))



    (defun find-file-wizard ()
      (interactive)
      (let ((wizard-alist *find-file-wizard-alist*)
            (plist (cdar *find-file-wizard-alist*))
            ;; (file 'fallback-wizard)
            (retval '(next))
            initial-string)
        (while (and wizard-alist ;; (eq file 'fallback-wizard)
                    (and (consp retval) (eq 'next (car retval))))
          ;; (message "fileB: %s" file)
          (letf ((plist (cdar wizard-alist))
                 (initial-string (plist-get (cdr retval) :initial-string)))
            (setq retval
                  (catch 'nextff
                    (funcall (plist-get plist :setup) (plist-get plist :ff) initial-string))
                  wizard-alist (or (cdr wizard-alist)
                                   (setq wizard-alist *find-file-wizard-alist*)))))))


    (global-set-key-if-unbind (kbd "s-x s-f") 'find-file-wizard))



(require 'general-testing)

(testing
 (defmacro find-file-wizard-add (name ff-fun map magic-key hook ret-variable fun)
   "hook where initial set could be defined\n
"
   (declare (debug t) (indent 4))
   `(let ((elt (cons ,name (list :ff-fun ',ff-fun :map ',map :key ',magic-key :hook ',hook :ret-variable ',ret-variable :fun ',fun))))
      (push elt *find-file-wizard-alist*)))


 (find-file-wizard-add "ido1"
     ;; ido-find-file
     (ido-file-internal ido-default-file-method
                        nil
                        nil
                        "Ido Find File: "
                        nil
                        initial-string)
     ido-completion-map
     (kbd "C-f") ;; [(control ?f)]
   ido-setup-hook
   ido-text
   (progn
     (setq initial-string ido-text
           ido-text 'fallback
           ido-exit 'done)
     (exit-minibuffer)))

 ;; (setq *find-file-wizard-alist* nil)

 ;; *find-file-wizard-alist*

 ;; (plist-get (cdar *find-file-wizard-alist*) :map)

 (defun find-file-wizard ()
   (interactive)
   (let ((wizard-alist *find-file-wizard-alist*)
         ;; (plist (cdar wizard-alist))
         (file 'fallback)
         initial-string)
     (while (and wizard-alist (eq file 'fallback))
       (let ((plist (cdar wizard-alist)))
         (setq file
               (letf (((symbol-value (plist-get plist :hook))
                       (cons (lambda ()
                               (define-key (plist-get plist :map)
                                   (plist-get plist :key)
                                 (lambda (arg)
                                   (interactive "P")
                                   (plist-get plist :fun))))
                             (plist-get plist :hook))))
                 ((plist-get plist :ff-fun) initial-string))
               wizard-alist (cdr wizard-alist))))
     ;; (exit-minibuffer)
     ;; file
     ))





 (progn
   (setq *find-file-wizard-alist* nil)

   (find-file-wizard-add "lusty"
       ;; ido-find-file
       (lambda (initial-string)
         (message "finder initial-string 4: %s" initial-string)
         (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
         (prog1
             (lusty-file-explorer)
           (message "finder Ainitial-string 4: %s" initial-string)))
       ido-completion-map
       (kbd "C-f") ;; [(control ?f)]
     ido-setup-hook
     ido-text
     (lambda (arg)
       (interactive "P")
       (progn
         (message "magic lambda4: ido-text: %s initial-string: %s" ido-text initial-string)
         (setq initial-string ido-text
               ido-text 'fallback-wizard
               ido-exit 'done)
         (message "magic Alambda4: ido-text: %s initial-string: %s" ido-text initial-string)
         (exit-minibuffer)
         (message "magic Alambda4x: ido-text: %s initial-string: %s" ido-text initial-string))))

   (find-file-wizard-add "idoff"
       ;; ido-find-file
       (lambda (initial-string)
         (message "finder initial-string 3: initial-string: %s" initial-string)
         (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
         (prog1
             (ido-find-file)
           (message "finder Ainitial-string 3: initial-string: %s" initial-string)))
       ido-completion-map
       (kbd "C-f") ;; [(control ?f)]
     ido-setup-hook
     ido-text
     (lambda (arg)
       (interactive "P")
       (progn
         (message "magic lambda3: ido-text: %s initial-string: %s" ido-text initial-string)
         (setq initial-string ido-text
               ;; ido-text 'fallback-wizard
               ido-exit 'done)
         (message "magic Alambda3: ido-text: %s initial-string: %s" ido-text initial-string)
         (exit-minibuffer)
         (message "magic Alambda3x: ido-text: %s initial-string: %s" ido-text initial-string))))

   (find-file-wizard-add "ido2"
       ;; ido-find-file
       (lambda (initial-string)
         (message "finder initial-string 2: initial-string: %s" initial-string)
         (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
         (prog1
             (ido-completing-read "2. Cached File: "
                                  (mapcar 'car file-cache-alist))
           (message "finder Ainitial-string 2: initial-string: %s" initial-string)))
       ido-completion-map
       (kbd "C-f") ;; [(control ?f)]
     ido-setup-hook
     ido-text
     (lambda (arg)
       (interactive "P")
       (progn
         (message "magic lambda2: ido-text: %s initial-string: %s" ido-text initial-string)
         (setq initial-string ido-text
               ido-text 'fallback-wizard
               ido-exit 'done)
         (message "magic Alambda2: ido-text: %s initial-string: %s" ido-text initial-string)
         (exit-minibuffer)
         (message "magic Alambda2x: ido-text: %s initial-string" ido-text initial-string))))


   (find-file-wizard-add "ido1"
       ;; ido-find-file
       (lambda (initial-string)
         (message "finder initial-string 1: initial-string: %s" initial-string)
         (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
         (prog1
             (ido-completing-read "1. Cached File: "
                                  (mapcar 'car file-cache-alist))
           (message "finder Ainitial-string 1: initial-string: %s" initial-string)))
       ido-completion-map
       (kbd "C-f") ;; [(control ?f)]
     ido-setup-hook
     ido-text
     (lambda (arg)
       (interactive "P")
       (progn
         (message "magic lambda1: ido-text: %s initial-string: %s" ido-text initial-string)
         (setq initial-string ido-text
               ido-text 'fallback-wizard
               ido-exit 'done)
         (message "magic Alambda1: ido-text: %s initial-string: %s" ido-text initial-string)
         (exit-minibuffer)
         (message "magic Alambda1x: ido-text: %s initial-string: %s" ido-text initial-string))))

   (defvar initial-string "" nil)

   (defun find-file-wizard ()
     (interactive)
     (let ((wizard-alist *find-file-wizard-alist*)
           (plist (cdar *find-file-wizard-alist*))
           (file 'fallback-wizard)
           initial-string)
       (while (and wizard-alist (eq file 'fallback-wizard))
         (message "fileB: %s" file)
         (message "TESTB")
         (message "while initial-string: %s" initial-string)
         (let ((plist (cdar wizard-alist)))
           (setq file
                 (letf (((symbol-value (plist-get plist :hook))
                         (cons (lambda ()
                                 (define-key (symbol-value (plist-get plist :map))
                                     (kbd "C-f") ;; (plist-get plist :key)
                                   (plist-get plist :fun)))
                               (symbol-value (plist-get plist :hook)))))
                   (message "letf initial-string: %s" initial-string)
                   (funcall (plist-get plist :ff-fun) initial-string))
                 wizard-alist (or (cdr wizard-alist)
                                  (setq wizard-alist *find-file-wizard-alist*))
                 ))
         (message "TESTA")
         (message "fileA: %s QQ" file))
       ;; (exit-minibuffer)
       ;; file
       ))


   (defun find-file-wizard ()
     (interactive)
     (let ((wizard-alist *find-file-wizard-alist*)
           (plist (cdar *find-file-wizard-alist*))
           (file 'fallback-wizard)
           initial-string)
       (while (and wizard-alist (eq file 'fallback-wizard))
         (message "fileB: %s" file)
         (message "TESTB")
         (message "while initial-string: %s" initial-string)
         (let ((plist (cdar wizard-alist)))
           (setq file
                 (letf (((symbol-value (plist-get plist :hook))
                         (cons (plist-get plist :fun)
                               (symbol-value (plist-get plist :hook)))))
                   (message "letf initial-string: %s" initial-string)
                   (funcall (plist-get plist :ff-fun) initial-string))
                 wizard-alist (or (cdr wizard-alist)
                                  (setq wizard-alist *find-file-wizard-alist*))
                 ))
         (message "TESTA")
         (message "fileA: %s QQ" file))
       ;; (exit-minibuffer)
       ;; file
       ))



   )




 (progn
   (setq *find-file-wizard-alist* nil)

   (find-file-wizard-add "lusty"
       ;; ido-find-file
       (lambda (initstr)
         (message "finder initstr 4: %s" initstr)
         (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
         (prog1
             (lusty-file-explorer)
           (message "finder Ainitstr 4: %s" initstr)))
       ido-completion-map
       (kbd "C-f") ;; [(control ?f)]
     ido-setup-hook
     ido-text

     (lambda ()
       (define-key ido-completion-map
           (kbd "C-f") ;; (plist-get plist :key)
         (lambda (arg)
           (interactive "P")
           (progn
             (message "magic lambda4: ido-text: %s initial-string: %s" ido-text initial-string)
             (setq initial-string ido-text
                   ido-text 'fallback-wizard
                   ido-exit 'done)
             (message "magic Alambda4: ido-text: %s initial-string: %s" ido-text initial-string)
             (exit-minibuffer)
             (throw 'nextff initial-string)
             (message "magic Alambda4x: ido-text: %s initial-string: %s" ido-text initial-string))))))

   (find-file-wizard-add "idoff"
       ;; ido-find-file
       (lambda (initstr)
         (message "finder initstr 3: initstr: %s" initstr)
         (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
         (prog1
             (ido-find-file)
           (message "finder Ainitstr 3: initstr: %s" initstr)))
       ido-completion-map
       (kbd "C-f") ;; [(control ?f)]
     ido-setup-hook
     ido-text

     (lambda ()
       (define-key ido-completion-map
           (kbd "C-f") ;; (plist-get plist :key)
         (lambda (arg)
           (interactive "P")
           (progn
             (message "magic lambda3: ido-text: %s initial-string: %s" ido-text initial-string)
             (setq initial-string ido-text
                   ido-text 'fallback-wizard
                   ido-exit 'done)
             (message "magic Alambda3: ido-text: %s initial-string: %s" ido-text initial-string)
             ;; (exit-minibuffer)
             (throw 'nextff initial-string)
             (message "magic Alambda3x: ido-text: %s initial-string: %s" ido-text initial-string))))))

   (find-file-wizard-add "ido2"
       ;; ido-find-file
       (lambda (initstr)
         (message "finder initstr 2: initstr: %s" initstr)
         (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
         (prog1
             (ido-completing-read "2. Cached File: "
                                  (mapcar 'car file-cache-alist))
           (message "finder Ainitstr 2: initstr: %s" initstr)))
       ido-completion-map
       (kbd "C-f") ;; [(control ?f)]
     ido-setup-hook
     ido-text

     (lambda ()
       (define-key ido-completion-map
           (kbd "C-f") ;; (plist-get plist :key)
         (lambda (arg)
           (interactive "P")
           (progn
             (message "magic lambda2: ido-text: %s initial-string: %s" ido-text initial-string)
             (setq initial-string ido-text
                   ido-text 'fallback-wizard
                   ido-exit 'done)
             (message "magic Alambda2: ido-text: %s initial-string: %s" ido-text initial-string)
             (exit-minibuffer)
             (throw 'nextff initial-string)
             (message "magic Alambda2x: ido-text: %s initial-string" ido-text initial-string))))))


   (find-file-wizard-add "ido1"
       ;; ido-find-file
       (lambda (initstr)
         (message "finder initstr 1: initstr: %s" initstr)
         (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
         (prog1
             (ido-completing-read "1. Cached File: "
                                  (mapcar 'car file-cache-alist))
           (message "finder Ainitstr 1: initstr: %s" initstr)))
       ido-completion-map
       (kbd "C-f") ;; [(control ?f)]
     ido-setup-hook
     ido-text

     (lambda ()
       (define-key ido-completion-map
           (kbd "C-f") ;; (plist-get plist :key)
         (lambda (arg)
           (interactive "P")
           (progn
             (message "magic lambda1: ido-text: %s initial-string: %s" ido-text initial-string)
             (setq initial-string ido-text
                   ido-text 'fallback-wizard
                   ido-exit 'done)
             (message "magic Alambda1: ido-text: %s initial-string: %s" ido-text initial-string)
             ;; (exit-minibuffer)
             (throw 'nextff  initial-string)
             (message "magic Alambda1x: ido-text: %s initial-string: %s" ido-text initial-string))))))




   ;; (defvar initial-string "" nil)



   (defun find-file-wizard ()
     (interactive)
     (let ((wizard-alist *find-file-wizard-alist*)
           (plist (cdar *find-file-wizard-alist*))
           (file 'fallback-wizard)
           initial-string)
       (while (and wizard-alist (eq file 'fallback-wizard))
         (message "fileB: %s" file)
         (message "TESTB")
         (message "while initial-string: %s" initial-string)
         (letf ((plist (cdar wizard-alist)))
           (setq file
                 (letf (((symbol-value (plist-get plist :hook))
                         (cons (plist-get plist :fun)
                               (symbol-value (plist-get plist :hook)))))
                   (message "letf initial-string: %s" initial-string)
                   (funcall (plist-get plist :ff-fun) initial-string))
                 wizard-alist (or (cdr wizard-alist)
                                  (setq wizard-alist *find-file-wizard-alist*))
                 ))
         (message "TESTA")
         (message "fileA: %s QQ" file))
       ;; (exit-minibuffer)
       ;; file
       ))


   (defun find-file-wizard ()
     (interactive)
     (let ((wizard-alist *find-file-wizard-alist*)
           (plist (cdar *find-file-wizard-alist*))
           (file 'fallback-wizard)
           initial-string)
       (while (and wizard-alist ;; (eq file 'fallback-wizard)
                   )
         (message "fileB: %s" file)
         (message "TESTB")
         (message "while initial-string: %s" initial-string)
         (letf ((plist (cdar wizard-alist)))
           (setq file
                 (catch 'nextff
                   (letf (((symbol-value (plist-get plist :hook))
                           (cons (plist-get plist :fun)
                                 (symbol-value (plist-get plist :hook)))))
                     (message "letf initial-string: %s" initial-string)
                     (funcall (plist-get plist :ff-fun) initial-string)))
                 wizard-alist (or (cdr wizard-alist)
                                  (setq wizard-alist *find-file-wizard-alist*))
                 ))
         (message "TESTA")
         (message "fileA: %s QQ" file))
       ;; (exit-minibuffer)
       ;; file
       ))

   )





 ;; (global-set-key-if-unbind (kbd "s-x s-f") 'find-file-wizard)
 ;; ( find-file-wizard)


 (defmacro macmsg (msg)
   `(message ,msg))

 (dotimes (a 10)
   (macmsg (format "AAA %d" a)))



 (defmacro find-file-macro (mhook mmap mkey mfun mff-fun)
   `(let ((,mhook
           (cons (lambda ()
                   (define-key ,mmap
                       ,mkey
                     ,(funcall
                       (lambda ()
                         (lambda (arg)
                           (interactive "P")
                           mfun)))))
                 (funcall (lambda ()
                            (eval ,mhook))))))
      (message ",mmap %s ,mkey %s" ,mmap ,mkey)
      (message ",mff-fun %s" (lambda () ,mff-fun))
      (message ",mhook %s" ,mhook)
      (funcall
       (funcall (lambda ()
                  `(lambda ()
                     ,,mff-fun))))))


 (defun find-file-wizard ()
   (interactive)
   (let ((wizard-alist *find-file-wizard-alist*)
         (plist (cdar *find-file-wizard-alist*))
         (file 'fallback)
         initial-string)
     (while (and wizard-alist (eq file 'fallback))
       (let ((plist (cdar wizard-alist)))
         (setq file
               (let ((hook (plist-get plist :hook))
                     (map  (plist-get plist :map))
                     (key  (plist-get plist :key))
                     (fun  (plist-get plist :fun))
                     (ff-fun  (plist-get plist :ff-fun)))
                 (message "%s %s" hook map)
                 (find-file-macro hook map key fun ff-fun))
               wizard-alist
               (cdr wizard-alist))))
     ;; (exit-minibuffer)
     ;; file
     ))


 ;; ,mhook ((lambda nil (define-key map key (funcall (lambda nil (` (lambda (arg) (interactive P) (, fun))))))) . ido-setup-hook)
 ;; (find-file-wizard)



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

 ;;
 ;;
 ;; (let* (jcl-ido-text
 ;;        (file (let ((ido-setup-hook (cons (lambda ()
 ;;                                            (define-key ido-completion-map [(control ?f)]
 ;;                                              (lambda (arg)
 ;;                                                (interactive "P")
 ;;                                                (if jcl-ido-text
 ;;                                                    (ido-magic-forward-char arg)
 ;;                                                    (setq jcl-ido-text ido-text
 ;;                                                          ido-text 'fallback-from-cache
 ;;                                                          ido-exit 'done)
 ;;                                                    (exit-minibuffer)))))
 ;;                                          ido-setup-hook)))
 ;;                (ido-completing-read "Cached File: "
 ;;                                     (mapcar 'car file-cache-alist))))))


 (defun funA ()
   (message "%s" varA))

 (defun funB ()
   (message "%s" varB))

 ...

 (setq alist
       '((varA . funA)
         (varB . funB)
         ...
         ))

 (defun call-each-fun-of-alist ()
   (dolist (e alist)
     (let (( (car e)  (read-from-minibuffer "value: ") ))
       (funcall (cdr e)))))

 ;; (call-each-fun-of-alist)

 ;; (let ((varA (read-from-minibuffer "value: ")))
 ;;   (funcall (cdr (assoc 'varA alist))))
 ;;
 ;; (require 'cl)
 ;;
 ;; (defun call-each-fun-of-alist ()
 ;;   (dolist (e alist)
 ;;     (destructuring-bind (variable . function) e
 ;;       (letf (((symbol-value variable)
 ;;               (read-from-minibuffer
 ;;                (format "value for %s: "
 ;;                        variable))))
 ;;         (funcall function)))))

 )









(provide 'find-file-config)
;;; find-file-config.el ends here
