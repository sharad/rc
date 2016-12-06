;;
;; mutt.el
;; 
;; Made by s
;; Login   <s@taj>
;; 
;; Started on  Sun Feb  1 17:19:15 2009 s
;; Last update Wed Mar  4 11:09:22 2009 Sharad Pratap

;; from: http://www.emacswiki.org/emacs/MuttInEmacs#toc3
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;; Further Configuration Since your Mutt mails are now in mail-mode,
;; you can use mail-mode-hook for further configurations. Here is an
;; example to add AutoFillMode:

(add-hook 'mail-mode-hook 'turn-on-auto-fill)

;; Replacing C-x # with C-x k This turns on auto-fill-mode,
;; abbrev-mode and sets C-x k to server-edit so that you donât have
;; to change from using C-x k to C-x # depending on the buffer

(defun my-mail-mode-hook ()
  (auto-fill-mode 1)
  (abbrev-mode 1))
  ;; (local-set-key "\C-Xk" 'server-edit))
(add-hook 'mail-mode-hook 'my-mail-mode-hook)


(provide 'mutt-config)
