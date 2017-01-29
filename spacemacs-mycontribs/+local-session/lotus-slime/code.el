
(require 'general-testing)





(when t
  )

(when t

)


;;{{
;; Donot compile
;; (byte-recompile-directory slime-path 0)

;; (expand-file-name slime-path)

;; (let ((package-dir (concat "/usr/share/"
;;                            (if (boundp 'flavor) (symbol-name flavor)
;; 			     "emacs")
;;                            "/site-lisp/slime")))
;;   (setq qload-path nil)
;;   (when (file-directory-p package-dir)
;;         (setq qload-path (cons package-dir
;; 	                      (cons (concat package-dir "/contrib")
;; 			            qload-path)))))

;; (setq pl '(a b c))

;; (cons 'x (cons 'z pl))

;; (add-to-list  'pl 'p)

;; pl

(when t
  ;; This is the problem point take care of it well.
  (setq load-path
        (cons (concat (pathname-delete-trailing-/ slime-path) "/contrib") load-path))
  ;; (add-to-list 'load-path (pathname-delete-trailing-/ slime-path))
  )




(deh-section "Slime"

  (when t
    (xrequire 'slime-autoloads))

  ;; (let ((slime-autoloads (concat slime-path "slime-autoloads.elc")))
  ;;   (if (file-exists-p slime-autoloads)
  ;;       (load slime-autoloads)
  ;;       (load "/usr/share/emacs/site-lisp/slime/slime-autoloads.el")))



  (deh-require-maybe slime-autoloads
    ;; (slime-setup '(slime-scratch slime-editing-commands))
    ;; (slime-setup '(slime-repl)) ; repl only
    ;; If you like what you see try this:
    ;; almost everything

    ;; producing error
    ;; (slime-setup '(slime-fancy))
    ;; (slime-setup '(slime-fancy slime-scratch slime-editing-commands slime-repl slime-asdf)))

    (when t
          (eval-after-load "slime"
            '))

    ;; (when (functionp 'slime-bind-editing-commands)
    ;;   (add-hook 'slime-mode-hook 'slime-bind-editing-commands))

    ))

;;}}








(load-slime)

(provide 'slime-config)
