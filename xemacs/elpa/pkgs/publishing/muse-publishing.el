;;; muse publishing.el --- muse publishing

;; Copyright (C) 2015  sharad

;; Author: sharad <spratap@merunetworks.com>
;; Keywords:convenience, data, hypermedia, wp

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

;; Publishing related configuration.

;;; Code:

(provide 'muse-publishing)

(require 'cl)
(require 'publishing)
(require 'muse)
(require 'muse-project)


;; (defvar *muse-top-dir*           (muse-publishing-created-contents-path))
;; (defvar *muse-top-style-dir*     (muse-publishing-created-contents-path "generic/muse/style"))
;; ;; (defvar *muse-generated-top-dir* (expand-file-name "gen/muse" *created-content-dir*))
;; (defvar *muse-generated-top-dir* (publishing-generated-contents-path 'muse))
;; (defvar *muse-website-address*   (publishing-website-address 'muse))

;;;###autoload
(defun muse-publishing-created-contents-path (&optional path)
  "thisandthat."
  (publishing-created-contents-path 'muse path))

;;;###autoload
(defun muse-publishing-generated-contents-path (&optional path)
  "thisandthat."
  (publishing-generated-contents-path 'muse path))

;;;###autoload
(defun muse-publishing-website-address (&optional localpath)
  (publishing-website-address 'muse localpath))

(defun muse-publishing-created-contents-style-path (&optional path)
  (muse-publishing-created-contents-path path))

;;;###autoload
(defun* read-muse-style-spec ()
  (let* ((muse-dir (read-directory-name "Muse Project Directory: " (muse-publishing-created-contents-path)))
         (publishing-path
          (read-directory-name
           "Muse Project Directory: "
           (publishing-generated-contents-path 'muse (replace-regexp-in-string (muse-publishing-created-contents-path) "" muse-dir))))
         (publishing-style
          (ido-completing-read "Muse Publishing Style: " (mapcar 'car muse-publishing-styles)))
         (publishing-url (read-from-minibuffer "Publishing Base URL: "))
         (publishing-options nil))
    (list muse-dir publishing-path publishing-style publishing-url publishing-options)))

;;;###autoload
(defun* read-muse-project-spec ()
  (let* ((name (read-from-minibuffer "Project Name: "))
         (muse-dirs
          (read-directory-name "Muse Project Directory: " (concat (muse-publishing-created-contents-path) "/" name)))
         (publishing-path
          (read-directory-name
           "Muse Project Directory: "
           (publishing-generated-contents-path 'muse
                   (replace-regexp-in-string (muse-publishing-created-contents-path) ""
                                             (if (consp muse-dirs) (car muse-dirs) muse-dirs)))))
         (publishing-style
          (ido-completing-read "Muse Publishing Style: " (mapcar 'car muse-publishing-styles)))
         (publishing-url (read-from-minibuffer "Publishing Base URL: "))
         (publishing-options nil))
    `(,name
      ,@(make-muse-style-spec
         (if (consp muse-dirs) (car muse-dirs) muse-dirs)
         publishing-path
         publishing-style
         publishing-url
         publishing-options))))

;;;###autoload
(defun add-muse-project (project-spec)
  "Add muse project."
  (interactive
   (let ((project-spec
          (read-muse-project-spec)))))
  (if (member (car project-spec)
              (mapcar 'car muse-project-alist))
      (if (or (not (called-interactively-p 'interactive))
              (y-or-n-p (format "project %s already present, do you want to overwrite it?: " (car project-spec))))
          (progn
            (remove-muse-project project-spec)
            (add-muse-project project-spec)))
      (add-to-list 'muse-project-alist project-spec)))

;;;###autoload
(defun remove-muse-project (project-spec)
  (interactive
   (let ((project (ido-completing-read "Project: "
                                       (mapcar 'car muse-project-alist))))
     (if project
         (list project)
         (error "No project %s present" project))))
  (let ((project
         (cond
           ((and (consp project-spec)
                 (stringp (car project-spec)))
            (car project-spec))
           ((stringp project-spec)
            project-spec)
           (t nil))))
    (if project
        (setq muse-project-alist
              (delete* project muse-project-alist
                       :key 'car
                       :test 'string-equal)))))

;;;###autoload
(defun* make-muse-style-spec (muse-dir publishing-path publishing-style publishing-url &rest publishing-options)
  (interactive
   (let* ((muse-dir (read-directory-name "Muse Project Directory: " (muse-publishing-created-contents-path)))
          (publishing-path
           (read-directory-name
            "Muse Project Directory: "
            (publishing-generated-contents-path 'muse
                    (replace-regexp-in-string (muse-publishing-created-contents-path) "" muse-dir))))
          (publishing-style
           (ido-completing-read "Muse Publishing Style: " (mapcar 'car muse-publishing-styles)))
          (publishing-url (read-from-minibuffer "Publishing Base URL: "))
          (publishing-options nil))
     (list muse-dir publishing-path publishing-style publishing-url publishing-options)))
  (apply
   'muse-project-alist-styles
   (append
    (list
     muse-dir
     publishing-path)
    (list publishing-style)
    (if publishing-url
        (list :base-url publishing-url))
    publishing-options)))

;;;###autoload
(defun* make-muse-project-spec (name muse-dirs publishing-path publishing-style publishing-url &rest publishing-options)
  (interactive
   (let* ((name (read-from-minibuffer "Project Name: "))
          (muse-dirs
           (read-directory-name "Muse Project Directory: " (concat (muse-publishing-created-contents-path) "/" name)))
          (publishing-path
           (read-directory-name
            "Muse Project Directory: "
            (publishing-generated-contents-path 'muse
                    (replace-regexp-in-string (muse-publishing-created-contents-path) ""
                                              (if (consp muse-dirs) (car muse-dirs) muse-dirs)))))
          (publishing-style
           (ido-completing-read "Muse Publishing Style: " (mapcar 'car muse-publishing-styles)))
          (publishing-url (read-from-minibuffer "Publishing Base URL: "))
          (publishing-options nil))
     (list name muse-dirs publishing-path publishing-style publishing-url publishing-options)))
  `(,name
    ,@(make-muse-style-spec
       (if (consp muse-dirs) (car muse-dirs) muse-dirs)
       publishing-path
       publishing-style
       publishing-url
       publishing-options)))

;; (mapcar 'car muse-publishing-styles)
;; (muse-project-alist-styles
;;  (muse-publishing-created-contents-path "/doc/priv")
;;  (publishing-generated-contents-path 'muse "/doc/pdf/doc/priv/pdf")
;;  "pdf")

;; (muse-project-alist-styles
;;  (muse-publishing-created-contents-path"/web/site/blog" )
;;  (publishing-generated-contents-path 'muse "/web/site/blog/pdf")
;;  "ikiwiki"
;;  :base-url ((publishing-website-address 'muse "blog/"))







;; (file-name-nondirectory "/sdf/sdfds//")

(when nil
  ;; Do corrections
  (dir-final-slash "/sdfds/dfg/dfg/fdg")
  (pathname-delete-trailing-/ "/sdfds/dfg/dfg/fdg"))



;; (defun muse-meta-style-dirname (dir)
;;   (let* ((style (plist-get muse-publishing-current-style :base))
;;          (dir (if (stringp dir)
;;                      (concat
;;                       (if (consp dir) (car dir) dir)
;;                       "/styles/"

;;                       (unless (string-equal
;;                                style
;;                                (file-name-nondirectory
;;                                 (pathname-delete-trailing-/ dir)))
;;                           style)))))
;;         dir))

;;;###autoload
(defun muse-meta-style-dirname (dir style)
  (let* ((dir (if (stringp dir)
                  (concat
                   (if (consp dir) (car dir) dir)
                   "/styles/"
                   (unless (string-equal
                            style
                            (file-name-nondirectory
                             (pathname-delete-trailing-/ dir)))
                     style)))))
    dir))

(defmacro muse-with-project-style (&rest body)
  `(let* ((muse-current-project (or (muse-project) (muse-read-project "Muse Project: " t t)))
          (muse-publishing-current-style (or muse-publishing-current-style
                                             (cdr
                                              (muse-publish-get-style
                                               (mapcar (lambda (style)
                                                         (cons (muse-get-keyword :base style) style))
                                                       (cddr muse-current-project)))))))
     (message "muse-with-project-style: body:- %s muse-current-project:- %s" body muse-current-project)
     (message "muse-with-project-style: body:- %s muse-publishing-current-style:- %s" body muse-publishing-current-style)
     ,@body))


(defmacro muse-with-project-style (&rest body)
  `(let ((muse-current-project
          (or (muse-project)
              (muse-read-project "Muse Project: " t t))))
     (message "muse-with-project-style: muse-current-project %s" muse-current-project)
     (let ((muse-publishing-current-style
            (or muse-publishing-current-style
                (cdr
                 (muse-publish-get-style
                  (mapcar (lambda (style)
                            (cons (muse-get-keyword :base style) style))
                          (cddr muse-current-project)))))))
       (message "muse-with-project-style: body:- %s muse-current-project:- %s" body muse-current-project)
       (message "muse-with-project-style: body:- %s muse-publishing-current-style:- %s" body muse-publishing-current-style)
       ,@body)))

;; (muse-publish-get-style
;;  (mapcar (lambda (style)
;;            (cons (muse-get-keyword :base style) style))
;;          (cddr (muse-read-project "Publish project: " t t))))

;; (ido-completing-read "sdfds" '(("aaa" "sdfds") ("ppp" "asfddsa")))


;; (mapcar (lambda (style)
;;            (cons (muse-get-keyword :base style) style))
;;          (cddr (muse-read-project "Publish project: " t t)))

;; (ido-completing-read "sdfads: "
;; '(("xhtml" :base "xhtml" :base-url (muse-publishing-website-address "notes/") :path "~/Documents/CreatedContent/gen/web/site/wiki/notes/xhtml")
;;  ("my-pdf" :base "my-pdf" :base-url "http://hello.org//notes/" :path "~/Documents/CreatedContent/gen/web/site/wiki/notes/my-pdf"))
;; )


;; muse-publishing-styles

(defvar *muse-meta-style-dirname-fns*
  `(
    ("project-export"
     (:path-function (lambda ()
                       ;; (message "current-style %s " muse-publishing-current-style)
                       (if muse-publishing-current-style
                           ;; (message "muse-publishing-current-style %s" muse-publishing-current-style)
                           (or
                            (muse-meta-style-dirname
                             (or
                              (plist-get muse-publishing-current-style :path)
                              (when muse-current-output-style
                                (plist-get muse-current-output-style :path)))
                             (plist-get muse-publishing-current-style :base))
                            'pass)
                           'pass))))
    ("project"
     (:path-function (lambda ()
                       (if (and (muse-project)
                                muse-publishing-current-style)
                           (let* ((project-dir (cadr (muse-project)))
                                  (project-dir (if (consp project-dir) (car project-dir) project-dir)))
                             ;; (message "(cadr (muse-project)) %s" project-dir)
                             (muse-meta-style-dirname project-dir (plist-get muse-publishing-current-style :base)))
                           'pass))))

    ("style"
     (:path-function (lambda ()
                       (if muse-publishing-current-style
                           (muse-meta-style-dirname
                            (muse-publishing-created-contents-style-path "generic/muse/style")
                            ;; (muse-publishing-created-contents-path "generic/muse/style")
                            (plist-get muse-publishing-current-style :base))
                           'pass))))

    ("base"
     (:path-function (muse-publishing-created-contents-path "generic/muse/style"))))
  "*muse-meta-style-dirname-fns*")

;; (muse-publishing-created-contents-path "generic/muse/style")
;; "~/Documents/CreatedContent/contents/muse/generic/style"
;; (cadar *muse-meta-style-dirname-fns*)

;;;###autoload
(defun mkdir-copy-file (src-file dst-file)
  (unless (file-exists-p dst-file)
    (progn
      (message "%s not exists" dst-file)
      (make-directory
       (expand-file-name
        (dirname-of-file dst-file)) t)
      (if (file-exists-p src-file)
          (copy-file src-file dst-file)
          (error "file %s not exists" src-file))))
  dst-file)

;; (defun lotus-muse-find-or-create-meta-file (filename &optional fnslist)
;;   "asfds"
;;   (let ((fnslist (or fnslist *muse-meta-style-dirname-fns*)))
;;     (muse-with-project-style
;;      (lotus-muse-find-or-create-meta-file-main filename fnslist))))

;;;###autoload
(defun lotus-muse-find-or-create-meta-file (filename &optional dirfnslist)
  "asfds"
  (let ((dirfnslist (or dirfnslist *muse-meta-style-dirname-fns*)))
    (lotus-muse-find-or-create-meta-file-main filename dirfnslist)))

;;;###autoload
(defun lotus-muse-find-or-create-meta-file-main (filename dirfnslist)
  "sdfds"
  ;; (message "calling lotus-muse-find-or-create-meta-file-main filename %s dirfnslist %s (cadar dirfnslist) %s" filename dirfnslist (cadar dirfnslist))
  (if dirfnslist
      (let* ((style-dirname-list (car dirfnslist))
             (style-name (car style-dirname-list))
             (strorfn (plist-get (cadr style-dirname-list) :path-function)))
        (let ((dirpath
               (cond
                 ((functionp strorfn)
                  ;; (muse-with-project-style
                  (funcall strorfn))
                 ;; )
                 ((stringp strorfn)   strorfn)
                 ((and (symbolp strorfn)
                       (stringp (symbol-value strorfn)))
                  (symbol-value strorfn))
                 ((null strorfn) nil)
                 (t (error "wrong")))))

          (cond
            ((and (stringp dirpath)
                  (file-directory-p dirpath))
             (let ((filepath (progn
                               ;; (message "dirfnslist %s" dirfnslist)
                               ;; (message "fn %s list fns no %d retval %s" strorfn  (length dirfnslist) dirpath)
                               (if (stringp dirpath) (expand-file-name filename dirpath)))))
               ;; (message "filepath: %s" filepath)
               ;; (unless dirpath
               ;;   (error "can not get dirpath from style %s" style-name))
               (if filepath
                   ;; (message "Xfilepath: %s" filepath)
                   (if (file-exists-p filepath)
                       filepath
                       (let ((parent-filepath (lotus-muse-find-or-create-meta-file-main filename (cdr dirfnslist))))
                         ;; (message "Have come here")
                         (if parent-filepath
                             (if (file-exists-p parent-filepath)
                                 (progn
                                   (mkdir-copy-file parent-filepath filepath)
                                   filepath)
                                 (error "file %s did not got created for %s" parent-filepath filepath))
                             (progn
                               (message "You need to create %s file manually" filepath)
                               (error "Can not file futher %s file now." filename))))))))
            ((eq dirpath 'pass)
             (lotus-muse-find-or-create-meta-file-main filename (cdr dirfnslist)))
            (t (error "can not get dirpath %s from style %s" dirpath style-name)))))
      (error "can not get parent file for %s" filename)))

;;;###autoload
(defun lotus-muse-get-meta-path-plist (&optional dirfnslist)
  "sdfds"
  (interactive)
  (let ((dirfnslist (or dirfnslist *muse-meta-style-dirname-fns*)))
    (remove-if-not '(lambda (c)
                     (stringp (cdr c)))
                   (mapcar
                    (lambda (fn-list)
                      (let ((name (car fn-list))
                            (strfn (plist-get (cadr fn-list) :path-function)))
                        (cons name
                              (cond
                                ((if (symbolp strfn)
                                     (stringp (symbol-value strfn)))
                                 (symbol-value strfn))
                                ((stringp strfn) strfn)
                                ((functionp strfn) (funcall strfn))
                                (t (error "error"))))))
                    dirfnslist))))

;;;###autoload
(defun lotus-muse-edit-meta-file ()
  (interactive)
  (let* ((path-alist (lotus-muse-get-meta-path-plist *muse-meta-style-dirname-fns*))
         (name
          (funcall muse-completing-read-function
                   "Get dir: " path-alist nil t))
         (path (cdr (assoc name path-alist))))
    (ido-find-file-in-dir path)))

;;;###autoload
(defun lotus-muse-delete-meta-file ()
  (interactive)
  (let* ((path-alist (lotus-muse-get-meta-path-plist *muse-meta-style-dirname-fns*))
         (name
          (funcall muse-completing-read-function
                   "Get dir: " path-alist nil t))
         (path (cdr (assoc name path-alist)))
         (delete-file (ido-read-file-name "delete muse meta file: " path)))
    (when (y-or-n-p (format "really delete %s :" delete-file))
      (delete-file delete-file)
      (message "file %s deleted." delete-file))))

;;;###autoload
(defun muse-insert-css-link (media filename)
  (muse-make-css-link media
                      (file-relative-name
                       (lotus-muse-find-or-create-meta-file filename)
                       (plist-get muse-publishing-current-style :path))))

;;;###autoload
(defun muse-insert-meta-file (filename)
  (get-string-from-file
   (lotus-muse-find-or-create-meta-file filename)))



























(progn
  ;; from: http://mwolson.org/projects/emacs-config/muse-init.el.html
  ;; I use initsplit.el to separate customize settings on a per-project
  ;; basis.

  ;; In order to see the scripts that used to publish my website to a
  ;; remote webserver, check out
  ;; http://mwolson.org/projects/SiteScripts.html.

  (dolist (lib
            '(
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
              ;; muse-message  ; load message support (experimental)
              ))
    (require lib))

  (require 'muse-message nil t)

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
  ;;                    :header (muse-publishing-created-contents-path "/web/site/meta/generic/header.html")
  ;;                    :footer (muse-publishing-created-contents-path "/web/site/meta/generic/footer.html"))

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

  ;; Key customizations
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
  ;;  (:base "xhtml" :base-url ((publishing-website-address 'muse "notes/") :path "~/Documents/CreatedContent/gen/web/site/wiki/notes/html")
  ;;  (:base "my-pdf" :base-url "http://hello.org//notes/" :path "~/Documents/CreatedContent/gen/web/site/wiki/notes/pdf"))

  (progn ;; "muse-publishing"

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
     ;; `(muse-html-footer ,(muse-publishing-created-contents-path "/web/site/meta/generic/footer.html"))
     ;; `(muse-html-header ,(muse-publishing-created-contents-path "/web/site/meta/generic/header.html"))
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

  (progn ;; "many changes"
    (defun muse-help ()
      (interactive)
      (find-file-other-window "/usr/share/doc/muse-el/examples/QuickStart.muse"))
    (define-key muse-mode-local-map (kbd "C-c C-.") 'muse-help)

    (progn ;; deh-require-maybe (and org org-html)
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

(progn ;; "many changes"


  (defun muse-help ()
    (interactive)
    (find-file-other-window "/usr/share/doc/muse-el/examples/QuickStart.muse"))

  (define-key muse-mode-local-map (kbd "C-c C-.") 'muse-help)

  (when nil ;;  (deh-require-maybe  ;; (and org org-html)
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


    ))
;;; muse-publishing.el ends here
