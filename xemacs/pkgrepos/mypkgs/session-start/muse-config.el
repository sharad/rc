



(deh-require-maybe muse-mode
  (defun muse-help ()
    (interactive)
    (find-file-other-window "/usr/share/doc/muse-el/examples/QuickStart.muse"))
  (define-key muse-mode-local-map (kbd "C-c C-.") 'muse-help)

  (deh-require-maybe org

    ;; quick fix
    (setq muse-ignored-extensions-regexp
          (regexp-or muse-ignored-extensions-regexp "\\.\\(muse\\|bz2\\|gz\\|[Zz]\\|org\\)\\'"))


    ;; (setq html (org-export-region-as-html beg end t 'string))
    (defun org-export-string-as-html-string (text)
      (with-temp-buffer
        (insert text)
        (org-export-region-as-html 0 (point-max) t 'string)))

    ;; Hurdle
    ;; (string-match muse-explicit-link-regexp "[[/~s/tmp/xx.org][sdfds]]")
    ;; (string-match org-bracket-link-analytic-regexp++ "[[/~s/tmp/xx.org][sdfds]]")

    (add-to-list 'muse-publish-markup-regexps
                 '(4000 org-bracket-link-analytic-regexp++ 0 org-export-string-as-html-string))))


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
     ("~/../paradise/Writing"   ;; Or wherever you want your planner
                                ;; files to be
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
      ;; Not needed.
      ;; :path "~/public_html/Writing/html"
      )))

(add-to-list
   'muse-project-alist
   '("StartWiki"
     ("~/public_html"   ;; Or wherever you want your planner
                                ;; files to be
      :default "index"
      :major-mode muse-mode
      :visit-link planner-visit-link)
     ;; This next part is for specifying where Planner pages
     ;; should be published and what Muse publishing style to
     ;; use. In this example, we will use the XHTML publishing
     ;; style.
     (:base "xhtml"
            ;; where files are published to
            ;; (the value of 'planner-publishing-directory', if
            ;; if you have configuration for an older version
            ;; of Planner)
      ;; :path "~/public_html"
      )))



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


;; (deh-require-maybe oddmuse
;;   ;; http://www.emacswiki.org/emacs/OddmuseMode
;;   )


(deh-require-maybe yaoddmuse
  ;; excellent great
  ;; http://www.emacswiki.org/emacs/Yaoddmuse

  ;; Usage

  ;;     To edit your first EmacsWiki page:

  ;;         M-x yaoddmuse-edit-default RET, input page name you want edit.
  ;;         Yaoddmuse will download the current page content. Then edit page using Oddmuse Markup (See TextFormattingRules)
  ;;         Type “C-u C-c C-c” when your edits are complete.
  ;;         Write your summary for the modifications, typically something like “Update”.

  ;;     That’s all. Yaoddmuse will view your edited page after a successful post.

  ;;  Me: M-x yaoddmuse-edit-default RET Yaoddmuse RET C-s ==Usage==

  )


(provide 'muse-config)
