

(add-to-list 'load-path "/usr/share/emacs/site-lisp/muse-el/experimental")



(deh-require-maybe
    ;; from: http://mwolson.org/projects/emacs-config/muse-init.el.html
    ;; I use initsplit.el to separate customize settings on a per-project
    ;; basis.

    ;; In order to see the scripts that used to publish my website to a
    ;; remote webserver, check out
    ;; http://mwolson.org/projects/SiteScripts.html.
    (progn
      ;; Initialize
      outline       ; I like outline-style faces
      muse          ; load generic module
      muse-colors   ; load coloring/font-lock module
      muse-mode     ; load authoring mode
      muse-blosxom  ; load blosxom module
      muse-docbook  ; load DocBook publishing style
      muse-html     ; load (XHTML publishing style
      muse-ikiwiki  ; load Ikiwiki support
      htmlize-hack  ; work around htmlize bug with Emacs 23
      muse-latex    ; load LaTeX/PDF publishing styles
      muse-latex2png ; publish <latex> tags
      muse-project  ; load support for projects
      muse-texinfo  ; load Info publishing style
      muse-wiki     ; load Wiki support
      muse-xml      ; load XML support
      muse-message  ; load message support (experimental)
      )

  ;; Setup projects

  ;; Here is an example of making a customized version of your favorite
  ;; publisher.  All this does is run `my-muse-blosoxm-finalize' on the
  ;; published file immediately after saving it.
  (muse-derive-style "my-blosxom" "blosxom-xhtml"
                     :final 'my-muse-blosxom-finalize)

  ;; This turns relative links into absolute links
  (muse-derive-style "my-pdf" "pdf"
                     :before 'my-muse-pdf-prepare-buffer)

  ;; This uses a different header and footer than normal
  (muse-derive-style "my-xhtml" "xhtml"
                     :header "~/personal-site/muse/header.html"
                     :footer "~/personal-site/muse/footer.html")

  ;; Define a draft style which provides extra space between sections

  (defvar muse-latex-draft-markup-strings
    '((chapter      . "\\bigskip\n\\bigskip\n\\chapter{")
      (section      . "\\bigskip\n\\bigskip\n\\section{")
      (subsection   . "\\bigskip\n\\bigskip\n\\subsection{")
      (subsubsection . "\\bigskip\n\\bigskip\n\\subsubsection{"))
    "Strings used for marking up Latex draft text.")

  (muse-derive-style "latex-draft" "latex"
                     :strings 'muse-latex-draft-markup-strings)
  (muse-derive-style "pdf-draft" "latex-draft"
                     :final   'muse-latex-pdf-generate
                     :browser 'muse-latex-pdf-browse-file
                     :link-suffix 'muse-latex-pdf-extension
                     :osuffix 'muse-latex-pdf-extension)

  ;; Define a style with unnumbered titles

  (defvar muse-latex-uh-markup-strings
    '((chapter      . "\\chapter*{")
      (section      . "\\section*{")
      (subsection   . "\\subsection*{")
      (subsubsection . "\\subsubsection*{"))
    "Strings used for marking up Latex text with unnumbered headings.")

  (muse-derive-style "latex-uh" "latex"
                     :strings 'muse-latex-uh-markup-strings)
  (muse-derive-style "pdf-uh" "latex-uh"
                     :final   'muse-latex-pdf-generate
                     :browser 'muse-latex-pdf-browse-file
                     :link-suffix 'muse-latex-pdf-extension
                     :osuffix 'muse-latex-pdf-extension)

  (defvar *doc-root* "~/Documents")
  (defvar *created-content-dir* (concat *doc-root* "/CreatedContent"))
  (defvar *muse-top-dir* (concat *created-content-dir* "/contents/muse"))
  (defvar *generated-top-dir* (concat *created-content-dir* "/gen"))
  (defvar *website-address* "http://hello.org/")

  ;; Here is my master project listing.
  (when t

    (setq my-muse-project-alist
          `(
            ("Website" ( ,(concat *muse-top-dir* "/web/site/wiki/web")
                         ,(concat *muse-top-dir* "/web/site/wiki/web/testdir/")
                                           :force-publish ("WikiIndex")
                                           :default "WelcomePage")
                       (:base "my-xhtml"
                              :base-url ,(concat *website-address* "/web/")
                              :include "/web/[^/]+"
                              :path ,(concat *generated-top-dir* "/web/site/wiki/web"))
                       (:base "my-xhtml"
                              :base-url ,(concat *website-address* "/web/")
                              :include "/testdir/[^/]+"
                              :path ,(concat *generated-top-dir* "/web/site/wiki/web/testdir/"))
                       (:base "my-pdf"
                              :base-url ,(concat *website-address* "/web/")
                              :path ,(concat *generated-top-dir* "/doc/pdf/site/wiki/web")
                              :include "/\\(CurriculumVitae\\|BriefResume\\)[^/]*$"))

            ("Projects" (
                         ,(concat *muse-top-dir* "/web/site/wiki/projects")
                         :force-publish ("WikiIndex" "MuseQuickStart")
                         :default "WelcomePage")
                        (:base "my-xhtml"
                               :base-url ,(concat *website-address* "/projects/")
                               :path ,(concat *generated-top-dir* "/web/site/wiki/projects")))

            ("Blog" (,@(muse-project-alist-dirs (concat *muse-top-dir* "/web/site/blog"))
                       :default "index"
                       :publish-project #'ignore)
                    ;; Publish this directory and its subdirectories.  Arguments
                    ;; are as follows.  The above `muse-project-alist-dirs' part
                    ;; is also needed.
                    ;;   1. Source directory
                    ;;   2. Output directory
                    ;;   3. Publishing style
                    ;;   remainder: Other things to put in every generated style
                    ,@(muse-project-alist-styles
                       (concat *muse-top-dir* "/web/site/blog")
                       (concat *generated-top-dir* "/doc/pdf/site/blog")
                       "ikiwiki"
                       :base-url (concat *website-address* "/blog/"))) ;; "http://grepfind.hello.org/blog/"

            ("MyNotes" (,(concat *muse-top-dir* "/web/site/wiki/notes")
                        :force-publish ("index")
                        :default "index")
                       (:base "xhtml"
                              :base-url "http://mwolson.org/notes/"
                              :path ,(concat *generated-top-dir* "/web/site/wiki/notes"))
                       (:base "my-pdf"
                              :base-url "http://mwolson.org/notes/"
                              :path ,(concat *generated-top-dir* "/web/site/wiki/notes")))

            ("_Private" (,(concat *muse-top-dir* "/doc/priv"))
                        ,@(muse-project-alist-styles (concat *muse-top-dir* "/doc/priv")
                                                     (concat *generated-top-dir* "/doc/pdf/doc/priv")
                                                     "pdf"))

            ("_Classes" (,@(muse-project-alist-dirs (concat *muse-top-dir* "/web/site/wiki/classes"))
                           :default "index")
                        ,@(muse-project-alist-styles (concat *muse-top-dir* "/web/site/wiki/classes")
                                                     (concat *generated-top-dir* "/web/site/wiki/classes")
                                                     "xhtml"))

            ("MA366" (,(concat *muse-top-dir* "/doc/pdf/classes/ma366"))
                     (:base "pdf-uh"
                      :path ,(concat *generated-top-dir* "/doc/pdf/classes/ma366")))

            ("ENGL238" (,(concat *muse-top-dir* "/doc/pdf/classes/eng238"))
                       (:base "pdf-uh"
                        :path ,(concat *generated-top-dir* "/doc/pdf/classes/eng238")))

            ("CS426" (,(concat *muse-top-dir* "/web/site/wiki/classes/cs426"))
                     (:base "pdf-uh"
                      :path "~/proj/classes/cs426"))

            ("_Plans" (,(concat *muse-top-dir* "/web/site/wiki/plans")
                       :default "TaskPool"
                       :major-mode planner-mode
                       :visit-link planner-visit-link)
                      (:base "planner-xhtml"
                       :path ,(concat *generated-top-dir* "/web/site/wiki/plans")))
            ))


    (setq muse-project-alist (append my-muse-project-alist muse-project-alist)))

  ;; Wiki settings
  (setq muse-wiki-interwiki-alist
        '(("PlugWiki" . "http://wiki.purduelug.org/")
          ("EmacsWiki" . "http://www.emacswiki.org/cgi-bin/wiki/")
          ("ArchWiki" . "http://gnuarch.org/gnuarchwiki/")
          ("HCoopWiki" . "http://wiki.hcoop.net/")
          ;; abbreviations
          ("CERIAS" . "http://www.cerias.purdue.edu/")
          ("PlannerMode" . "http://www.emacswiki.org/cgi-bin/wiki/PlannerMode")
          ("RememberMode" . "http://www.emacswiki.org/cgi-bin/wiki/RememberMode")
          ("GP2X" . "http://www.gp2x.co.uk/")
          ("UbuntuLinux" . "http://ubuntulinux.org/")
          ("HCoop" . "http://hcoop.net/")
          ("PLUG" . "http://purduelug.org/")
          ("PAC" . "http://web.ics.purdue.edu/~pac/")))

;;; Functions

  ;; Turn relative links into absolute ones
  (defun my-muse-pdf-make-links-absolute (str &rest ignored)
    "Make relative links absolute."
    (when str
      (save-match-data
        (if (string-match "\\`[/.]+" str)
            (replace-match "http://mwolson.org/" nil t str)
            str))))

  ;; Make sure my interproject links become absolute when published in
  ;; PDFs
  (defun my-muse-pdf-prepare-buffer ()
    (set (make-local-variable 'muse-publish-url-transforms)
         (cons 'my-muse-pdf-make-links-absolute muse-publish-url-transforms)))

  ;; Switch to the given project and prompt for a file
  (defun my-muse-project-find-file (project)
    (interactive)
    (let ((muse-current-project (muse-project project)))
      (call-interactively 'muse-project-find-file)))

  (defun my-muse-blosxom-finalize (file output-path target)
    ;;  (my-muse-prepare-entry-for-xanga output-path)
    ;; For now, do nothing.
    )

  ;; Make the current file display correctly in Xanga
  ;; I call this using C-c p x now.
  (defun my-muse-prepare-entry-for-xanga (file)
    "Mangle FILE so that Xanga doesn't bug out, saving to X clipboard.

If FILE is not specified, use the published version of the current file."
    (interactive
     (list
      (expand-file-name (concat (muse-page-name) muse-blosxom-extension)
                        (muse-style-element
                         :path (car (muse-project-applicable-styles
                                     buffer-file-name
                                     (cddr (muse-project-of-file))))))))
    (save-match-data
      (muse-with-temp-buffer
        (muse-insert-file-contents file)
        ;; surround first line in <h3></h3>
        (goto-char (point-min))
        (insert "<h3>")
        (end-of-line)
        (insert "</h3>")
        ;; treat example regions properly
        (let (beg end)
          (while (re-search-forward "<pre[^>]*>" nil t)
            (setq beg (match-end 0))
            (setq end (if (re-search-forward "</pre>" nil 1)
                          (match-beginning 0)
                          (point)))
            (save-restriction
              (narrow-to-region beg end)
              ;; change initial spaces to &nbsp;
              (goto-char (point-min))
              (while (re-search-forward "^ +" nil t)
                (replace-match (apply 'concat (make-list
                                               (length (match-string 0))
                                               "&nbsp;"))))
              ;; change newline to <br />
              (goto-char (point-min))
              (while (re-search-forward "\n" nil t)
                (replace-match "<br />")))))
        ;; get rid of 2 spaces together and merge lines
        (goto-char (point-min))
        (while (re-search-forward (concat "[" muse-regexp-blank "\n]+") nil t)
          (replace-match " "))
        ;; remove trailing space
        (goto-char (point-min))
        (while (re-search-forward " *</p> *" nil t)
          (replace-match "</p>"))
        ;; make relative links work
        (goto-char (point-min))
        (while (re-search-forward "href=\"[/.]+" nil t)
          (replace-match "href=\"http://mwolson.org/" nil t))
        ;; copy entry to clipboard
        (clipboard-kill-ring-save (point-min) (point-max))
        (message "Copied blog entry to clipboard"))))

  ;; Turn a word or phrase into a clickable Wikipedia link
  (defun my-muse-dictize (beg end)
    (interactive "r")
    (let* ((text (buffer-substring-no-properties beg end))
           (link (concat "dict:" (replace-regexp-in-string " " "_" text t t))))
      (delete-region beg end)
      (insert "[[" link "][" text "]]")))

  (defun my-muse-surround-math (&optional beg end)
    "If a region is higlighted, surround it with <math>...</math>.
If no region is highlighted, insert <math></math> and leave the point
between the two tags."
    (interactive (list (ignore-errors (mark)) (point)))
    (if (and beg end)
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (insert "<math>")
          (goto-char (point-max))
          (insert "</math>"))
        (insert "<math>")
        (save-excursion (insert "</math>"))))

  (defun my-muse-cdotize-region (beg end)
    (interactive "r")
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward " *\\* *" nil t)
        (replace-match " \\\\cdot "))))

;;; Key customizations
  (when nil
    (global-set-key "\C-cpl" 'muse-blosxom-new-entry)
    (global-set-key "\C-cpL" #'(lambda () (interactive)
                                       (my-muse-project-find-file "Blog")))
    (global-set-key "\C-cpi" #'(lambda () (interactive)
                                       (my-muse-project-find-file "_Private")))
    (global-set-key "\C-cpm" #'(lambda () (interactive)
                                       (my-muse-project-find-file "MA453")))
    (global-set-key "\C-cpn" #'(lambda () (interactive)
                                       (my-muse-project-find-file "MyNotes")))
    (global-set-key "\C-cpp" #'(lambda () (interactive)
                                       (my-muse-project-find-file "_Plans")))
    (global-set-key "\C-cpr" #'(lambda () (interactive)
                                       (my-muse-project-find-file "Projects")))
    (global-set-key "\C-cps" #'(lambda () (interactive)
                                       (my-muse-project-find-file "_Classes")))
    (global-set-key "\C-cpw" #'(lambda () (interactive)
                                       (my-muse-project-find-file "Website")))
    (global-set-key "\C-cpC" #'my-muse-cdotize-region)
    (global-set-key "\C-cpM" #'my-muse-surround-math)
    (global-set-key "\C-cpW" #'my-muse-dictize)
    (global-set-key "\C-cpx" #'my-muse-prepare-entry-for-xanga)
    )
;;; Custom variables

  (custom-set-variables
   `(muse-blosxom-base-directory ,(concat *created-content-dir* "/gen/web/site/blog"))
   '(muse-colors-autogen-headings (quote outline))
   '(muse-colors-inline-image-method (quote muse-colors-use-publishing-directory))
   '(muse-completing-read-function (quote ido-completing-read))
   '(muse-html-charset-default "utf-8")
   '(muse-html-encoding-default (quote utf-8))
   '(muse-html-footer ,(concat *created-content-dir* "/gen/web/site/meta/generic/muse/footer.html"))
   '(muse-html-header ,(concat *created-content-dir* "/gen/web/site/meta/generic/muse/header.html"))
   '(muse-html-meta-content-encoding (quote utf-8))
   '(muse-html-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/common.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"screen\" href=\"/screen.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"print\" href=\"/print.css\" />")
   '(muse-latex-header "~/personal-site/muse/header.tex")
   '(muse-latex-pdf-browser "evince %s &")
   '(muse-mode-hook (quote (flyspell-mode footnote-mode)))
   '(muse-publish-comments-p t)
   '(muse-publish-date-format "%b. %e, %Y")
   '(muse-publish-desc-transforms (quote (muse-wiki-publish-pretty-title muse-wiki-publish-pretty-interwiki muse-publish-strip-URL)))
   '(muse-wiki-publish-small-title-words (quote ("the" "and" "at" "on" "of" "for" "in" "an" "a" "page")))
   `(muse-xhtml-footer ,(concat *created-content-dir* "/gen/web/site/meta/generic/muse/footer.html"))
   `(muse-xhtml-header ,(concat *created-content-dir* "/gen/web/site/meta/generic/muse/header.html"))
   '(muse-xhtml-style-sheet "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"all\" href=\"/common.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"screen\" href=\"/screen.css\" />
<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" media=\"print\" href=\"/print.css\" />"))
  (custom-set-faces
   '(muse-bad-link ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold)))))


  (deh-section "from here"
    (defun muse-help ()
      (interactive)
      (find-file-other-window "/usr/share/doc/muse-el/examples/QuickStart.muse"))
    (define-key muse-mode-local-map (kbd "C-c C-.") 'muse-help)

    (deh-require-maybe org

      ;; quick fix
      (push "org" muse-ignored-extensions)
      (when (fboundp 'muse-update-ignored-extensions-regexp)
        (muse-update-ignored-extensions-regexp
         'muse-ignored-extensions muse-ignored-extensions))



      ;; (setq html (org-export-region-as-html beg end t 'string))
      (defun org-export-string-as-html-string (text)
        (with-temp-buffer
          (insert text)
          (org-export-region-as-html 0 (point-max) t 'string)))

      ;; Hurdle
      ;; (string-match muse-explicit-link-regexp "[[/~s/tmp/xx.org][sdfds]]")
      ;; (string-match org-bracket-link-analytic-regexp++ "[[/~s/tmp/xx.org][sdfds]]")

      (add-to-list 'muse-publish-markup-regexps
                   '(4000 org-bracket-link-analytic-regexp++ 0 org-export-string-as-html-string)))))


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
