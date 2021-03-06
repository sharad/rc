#!/usr/bin/emacs --script
;;
;; be.el
;; 
;; Made by s
;; Login   <s@taj>
;; 
;; Started on  Wed Feb  4 21:51:00 2009 s
;; Last update Wed Feb 25 18:47:34 2009 Sharad Pratap
(load "~/.xemacs/init.el")

(setq
 european-calendar-style nil
 diary-mail-days 7 
 diary-file "~/.Organize/emacs/diary/diary")

(org-batch-agenda "a"
                  european-calendar-style nil
                  diary-mail-days 12
                  org-agenda-ndays 20
                  diary-file "~/.Organize/emacs/diary/diary"
                  org-agenda-files
                  (file-expand-wildcards "~/.Organize/emacs/org/*/*.org"))

;;;      emacs -batch -l ~/.emacs -eval '(org-batch-agenda "t")' | lpr

;;; You may also modify parameters on the fly like this:

;;;      emacs -batch -l ~/.emacs                                      \
;;;         -eval '(org-batch-agenda "a"                               \
;;;                  org-agenda-ndays 300                              \
;;;                  org-agenda-include-diary nil                      \
;;;                  org-agenda-files (quote ("~/org/project.org")))'  \
;;;         | lpr

;;;     (org-batch-agenda "S")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from: http://www.mail-archive.com/emacs-orgmode@gnu.org/msg10679.html
;;emacs --batch --no-init-file -q  -l ~/.emacs.slim --eval "(progn
;;(org-agenda-list) (princ (buffer-string)))" 2> /dev/null
