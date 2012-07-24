

;; from: http://1010.co.uk/tech_notes.html
;;
;;
;; (global-set-key "\C-xa" 'upload-all)

;; (defun upload-all ()
;;   (interactive)
;;   (emacs-wiki-publish)
;;   (shell-command "/usr/bin/sitecopy -u site"))
;;
;;end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/share/emacs/site-lisp/muse-el/experimental")

(provide 'muse-config)
