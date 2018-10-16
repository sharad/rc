;;; ffw.el --- Find file wizard

;; Copyright (C) 2013  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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


(progn ;; deh-section "find-file-wizard"

  (defvar *find-file-wizard-alist* nil "find-file-wizard-alist")
  (setq *find-file-wizard-alist* nil)

  (defun find-file-wizard-add (name ff &optional setup failval)
    "hook where initial set could be defined\n
"

    (unless setup
      (setq setup
            (lambda (ff initial-string)
              (let (oldbinding)
                (minibuffer-with-setup-hook
                    (lambda ()
                      ;; (key-binding key t)
                      (setq oldbinding (key-binding (kbd "s-f") t))
                      ;; (local-set-key key binding)
                      (local-set-key
                       ;; (define-key minibuffer-local-completion-map
                       (kbd "s-f") ;; (plist-get plist :key)
                       (lambda (arg)
                         (interactive "P")
                         (message "oldbinding %s" oldbinding)
                         (if oldbinding
                             (local-set-key (kbd "s-f") oldbinding)
                             (local-unset-key (kbd "s-f")))
                         (setq initial-string ido-text
                               ;; ido-text 'fallback-wizard
                               ;; ido-exit 'done
                               )
                         ;; (exit-minibuffer)
                         (throw 'nextff (list 'next (list (cons :initial-string initial-string)))))))
                  (funcall ff initial-string))))))

    (if (assoc name *find-file-wizard-alist*)
        (setcdr
         (assoc name *find-file-wizard-alist*)
         (list :ff ff :setup setup :failval failval))
        (let ((elt (cons name (list :ff ff :setup setup :failval failval))))
          (push elt *find-file-wizard-alist*))))


  ;;  (pop *find-file-wizard-alist*)
  ;; (define-key minibuffer-local-map (kbd "C-f") 'forward-char)

  (find-file-wizard-add "ffip"
      ;; ido-find-file
      (lambda (initstr)
        (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
        (ffip)))

  (find-file-wizard-add "contentswitch"
      ;; ido-find-file
      (lambda (initstr)
        (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
        (contentswitch)))

  (find-file-wizard-add "find-file"
      ;; ido-find-file
      (lambda (initstr)
        (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
        (find-file
         (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer))))

      (lambda (ff initial-string)
        (let (oldbinding)
         (minibuffer-with-setup-hook
            (lambda ()
              ;; (key-binding key t)
              (setq oldbinding (key-binding (kbd "s-f") t))
              ;; (local-set-key key binding)
              (local-set-key
              ;; (define-key minibuffer-local-completion-map
               (kbd "s-f") ;; (plist-get plist :key)
                (lambda (arg)
                  (interactive "P")
                  (message "oldbinding %s" oldbinding)
                  (if oldbinding
                      (local-set-key (kbd "s-f") oldbinding)
                      (local-unset-key (kbd "s-f")))
                  (setq initial-string ido-text
                        ;; ido-text 'fallback-wizard
                        ;; ido-exit 'done
                        )
                  ;; (exit-minibuffer)
                  (throw 'nextff (list 'next (list (cons :initial-string initial-string)))))))
          (funcall ff initial-string)))))

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


  (find-file-wizard-add "idorelative"
      ;; ido-find-file
      (lambda (initstr)
        (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
        (find-same-file-in-relative-dir))


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
            (let ((failval  (plist-get plist :failval))
                  (ffretval (catch 'nextff
                              (condition-case e
                                  (funcall (plist-get plist :setup) (plist-get plist :ff) initial-string)
                                (error '(next))))))
              (if (eq failval ffretval)
                  (setq retval '(next)))
              (setq wizard-alist (or (cdr wizard-alist)
                                     (setq wizard-alist *find-file-wizard-alist*))))))))


    (global-set-key-if-unbind (kbd "s-x s-f") 'find-file-wizard))


(provide 'ffw)
;;; ffw.el ends here
