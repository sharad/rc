



(deh-require-maybe muse-mode
  (defun muse-help ()
    (interactive)
    (find-file-other-window "/usr/share/doc/muse-el/examples/QuickStart.muse"))
  (define-key muse-mode-local-map (kbd "C-c C-.") 'muse-help))


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

;; lib muse-texinfo

(add-to-list
   'muse-project-alist
   '("WikiWriting"
     ("~/../paradise/Writing"   ;; Or wherever you want your planner files to be
      :default "index"
      :major-mode muse-mode
      :visit-link planner-visit-link)
     ;; This next part is for specifying where Planner pages
     ;; should be published and what Muse publishing style to
     ;; use. In this example, we will use the XHTML publishing
     ;; style.
     (:base "planner-xhtml"
            ;; where files are published to
            ;; (the value of 'planner-publishing-directory', if
            ;; if you have configuration for an older version
            ;; of Planner)
      :path "~/public_html/Writing")))



(deh-require-maybe markdown-mode
  ;; (autoload 'markdown-mode "markdown-mode"
  ;;   "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))


(deh-require-maybe (progn
                     oddmuse
                     yaoddmuse
                     org-oddmuse
                     wikirel
                     oddmuse-curl)
  ;; http://www.emacswiki.org/emacs/Yaoddmuse
  ;; http://www.emacswiki.org/emacs/Yaoddmuse#toc8
  )


(provide 'muse-config)
