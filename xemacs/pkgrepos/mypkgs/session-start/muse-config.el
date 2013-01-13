

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


(provide 'muse-config)
