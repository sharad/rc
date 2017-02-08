
(eval-when-compile '(require 'cl))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/muse-el/experimental")


(require 'publishing-config)

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
  ;; (muse-derive-style "my-xhtml" "xhtml"
  ;;                    :header (concat *muse-top-dir* "/web/site/meta/generic/header.html")
  ;;                    :footer (concat *muse-top-dir* "/web/site/meta/generic/footer.html"))

  ;; This uses a different header and footer than normal
  (muse-derive-style "my-xhtml" "xhtml"
                     :header "<lisp>(muse-insert-meta-file \"header.html\")</lisp>"
                     :footer "<lisp>(muse-insert-meta-file \"footer.html\")</lisp>")

  ;; muse-publishing-styles
  ;; (assoc "my-xhtml" muse-publishing-styles)
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

(defun muse-make-css-link (media href)
  (let ((media (or media "all")))
    (concat
     "<link"
     " rel=\"stylesheet\""
     " type=\"text/css\""
     " charset=\"utf-8\""
     " media=\"" media "\""
     " href=\"" href "\" />\n")))

;; (muse-project "MyNotes")

;; ("MyNotes"
;;  ("~/Documents/CreatedContent/contents/muse/web/site/wiki/notes" :force-publish ("index") :default "index")
;;  (:base "xhtml" :base-url (concat *website-address* "/notes/") :path "~/Documents/CreatedContent/gen/web/site/wiki/notes/html")
;;  (:base "my-pdf" :base-url "http://hello.org//notes/" :path "~/Documents/CreatedContent/gen/web/site/wiki/notes/pdf"))

(deh-section "muse-publishing"

 ;; (file-relative-name "/tmp/xx" "/tmp/asfd/sdf")

 ;; edit-project-style-file
 ;; edit-project-header
 ;; edit-project-footer

;;; Custom variables
 (custom-set-variables
  `(muse-blosxom-base-directory ,(concat *created-content-dir* "/gen/web/site/blog"))
  `(muse-colors-autogen-headings (quote outline))
  `(muse-colors-inline-image-method (quote muse-colors-use-publishing-directory))
  `(muse-completing-read-function (quote ido-completing-read))
  `(muse-html-charset-default "utf-8")
  `(muse-html-encoding-default (quote utf-8))
  ;; `(muse-html-footer ,(concat *muse-top-dir* "/web/site/meta/generic/footer.html"))
  ;; `(muse-html-header ,(concat *muse-top-dir* "/web/site/meta/generic/header.html"))
  `(muse-html-footer "<lisp>(muse-insert-meta-file \"footer.html\")</lisp>")
  `(muse-html-header "<lisp>(muse-insert-meta-file \"header.html\")</lisp>")

  `(muse-html-meta-content-encoding (quote utf-8))
  `(muse-html-style-sheet
    "<lisp>
       (concat
        (muse-insert-css-link \"all\" \"common.css\")
        (muse-insert-css-link \"screen\" \"screen.css\")
        (muse-insert-css-link \"print\" \"print.css\"))
       </lisp>")
  `(muse-latex-header "<lisp>(muse-insert-meta-file \"header.tex\")</lisp>")
  `(muse-latex-pdf-browser "evince %s &")
  `(muse-mode-hook (quote (flyspell-mode footnote-mode)))
  `(muse-publish-comments-p t)
  `(muse-publish-date-format "%b. %e, %Y")
  `(muse-publish-desc-transforms (quote (muse-wiki-publish-pretty-title muse-wiki-publish-pretty-interwiki muse-publish-strip-URL)))
  `(muse-wiki-publish-small-title-words (quote ("the" "and" "at" "on" "of" "for" "in" "an" "a" "page")))
  `(muse-xhtml-footer "<lisp>(muse-insert-meta-file \"footer.html\")</lisp>")
  `(muse-xhtml-header "<lisp>(muse-insert-meta-file \"header.html\")</lisp>")
  `(planner-xhtml-footer "<lisp>(muse-insert-meta-file \"footer.html\")</lisp>")
  `(planner-xhtml-header "<lisp>(muse-insert-meta-file \"header.html\")</lisp>")
  `(muse-xhtml-style-sheet
    "<lisp>
       (concat
        (muse-insert-css-link \"all\" \"common.css\")
        (muse-insert-css-link \"screen\" \"screen.css\")
        (muse-insert-css-link \"print\" \"print.css\"))
       </lisp>"))
 (custom-set-faces
  '(muse-bad-link ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))))))





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

(deh-section "many changes"
    (defun muse-help ()
      (interactive)
      (find-file-other-window "/usr/share/doc/muse-el/examples/QuickStart.muse"))
    (define-key muse-mode-local-map (kbd "C-c C-.") 'muse-help)

    (deh-require-maybe (and org org-html)
      ;; quick fix
      (progn
        (push "org" muse-ignored-extensions)
        (when (fboundp 'muse-update-ignored-extensions-regexp)
          (muse-update-ignored-extensions-regexp
           'muse-ignored-extensions muse-ignored-extensions)))


      ;; (defadvice muse-update-ignored-extensions-regexp ())


      ;; (setq html (org-export-region-as-html beg end t 'string))
      (defun org-export-string-as-html-string (text)
        (with-temp-buffer
          (insert text)
          (org-export-region-as-html 0 (point-max) t 'string)))

      ;; Hurdle
      ;; (string-match muse-explicit-link-regexp "[[/~s/tmp/xx.org][sdfds]]")
      ;; (string-match muse-explicit-link-regexp "[[/~s/office/Adding tables to Controller-new.muse][Adding tables to Controller]]")
      ;; (string-match org-bracket-link-analytic-regexp++ "[[/~s/tmp/xx.org][sdfds]]")
      ;; (string-match org-bracket-link-analytic-regexp++ "[[/~s/tmp/xx.muse][sdfds]]")
      ;; (string-match org-bracket-link-analytic-regexp++ "[[xx.muse][sdfds]]")

      (add-to-list 'muse-publish-markup-regexps
                   '(4000 org-bracket-link-analytic-regexp++ 0 org-export-string-as-html-string)))

    (defvar custome/muse-ignore-existing-muse-file nil "custome/muse-ignore-existing-muse-file")
    (setq custome/muse-ignore-existing-muse-file t)
    ;; custome/muse-ignore-existing-muse-file is nil
    ;; or output Publishing Directory should be different from muse file directory.

    (defun muse-publish-classify-url (target)
      "Transform anchors and get published name, if TARGET is a page.
The return value is two linked cons cells.  The car is the type
of link, the cadr is the page name, and the cddr is the anchor."
      (save-match-data
        (cond ((or (null target) (string= target ""))
               nil)
              ((string-match "\\`[uU][rR][lL]:\\(.+\\)\\'" target)
               (cons 'url (cons (match-string 1 target) nil)))
              ((string-match muse-image-regexp target)
               (cons 'image (cons target nil)))
              ((string-match muse-url-regexp target)
               (cons 'url (cons target nil)))
              ;; SOLVE it, which stops [[/~s/office/notes.org][Notes]] or [[/~s/office/MemoryDebug.muse][Memory Debugging]] link to works
              ;; ((string-match muse-file-regexp target)
              ;;  (cons 'file (cons target nil)))
              ((string-match "#" target)
               (if (eq (aref target 0) ?\#)
                   (cons 'anchor-ref (cons nil (substring target 1)))
                   (cons 'link-and-anchor
                         ;; match-data is changed by
                         ;; `muse-publish-link-page' or descendants.
                         (cons (save-match-data
                                 (muse-publish-link-page
                                  (substring target 0 (match-beginning 0))))
                               (substring target (match-end 0))))))
              (t
               (cons 'link (cons (muse-publish-link-page target) nil))))))


    (defun muse-project-link-page (page)
      (let ((project (muse-project-of-file)))
        (if project
            (muse-project-resolve-link page
                                       (muse-project-current-output-style)
                                       (muse-project-applicable-styles
                                        (muse-project-page-file page project)
                                        (cddr project)))
            (muse-publish-link-file page))))

    (defsubst muse-publish-link-file (file &optional style)
      "Turn FILE into a URL.

If FILE exists on the system as-is, return it without
modification.  In the case of wanting to link to Muse files when
`muse-file-extension' is nil, you should load muse-project.el.

Otherwise, assume that it is a Muse file and call
`muse-publish-link-name' to add :prefix, :link-suffix, :suffix,
and removing ignored file extensions, but preserving the
directory part of FILE."
      (setq style (muse-style style))
      (if (and
           (null custome/muse-ignore-existing-muse-file)
           (file-exists-p file))
          file
          (concat (file-name-directory file)
                  (muse-publish-link-name file style))))



    ;;
    ;; (testing
    (when nil


     (defsubst muse-publish-link-name (&optional file style)
       "Take FILE and add :prefix and either :link-suffix or :suffix from STYLE.
We assume that FILE is a Muse file.

We call `muse-page-name' on FILE to remove the directory part of
FILE and any extensions that are in `muse-ignored-extensions'."
       (setq style (muse-style style))
       (concat (muse-style-element :prefix style)
               (muse-page-name file)
               (or (muse-style-element :link-suffix style)
                   (muse-style-element :suffix style))))


      (muse-style-element :link-suffix (car muse-publishing-styles))
      (muse-style-element :suffix (car muse-publishing-styles))

      (muse-publish-link-name "xx.org"
                              (muse-style
                               (assoc "html" muse-publishing-styles)))

      (assoc "xhtml" muse-publishing-styles)

      (muse-style-element :suffix
                          (muse-style
                           (assoc "html" muse-publishing-styles)))

      (muse-style-element :rules
                          (assoc "xhtml" muse-publishing-styles))

      (muse-publish-markup-string
       "[[Controller.muse][Control]]"
       (assoc "xhtml" muse-publishing-styles))

      ;; (muse-publish-insert-url "aa.muse" "xx" "aa.muse" t)
      ;; (muse-publish-url url desc orig-url explicit)
      ;; (muse-publish-classify-url)


      ;; check for
      ;; (defsubst muse-publish-link-page (page)
      ;;   (if (fboundp 'muse-project-link-page)
      ;;       (muse-project-link-page page)
      ;;     (muse-publish-link-file page)))


      )))

(provide 'muse-config)
