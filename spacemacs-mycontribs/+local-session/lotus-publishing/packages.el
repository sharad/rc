;;; packages.el --- lotus-publishing layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-publishing-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-publishing/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-publishing/pre-init-PACKAGE' and/or
;;   `lotus-publishing/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-publishingS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-publishing-packages
  '(
    (publishing :location local)
    (muse-publishing :location local)
    (org-publishing :location local)
    )
  "The list of Lisp packages required by the lotus-publishing layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-publishing/init-publishing ()
  (use-package publishing
      :defer t
      :config
      (progn
        (setq
         *doc-root*            (expand-file-name "Documents" "~")
         *created-content-dir* (expand-file-name "CreatedContent" *doc-root*)
         *website-address*     "http://sharad.ddns.net/~s/gen/"))))

(defun lotus-publishing/init-muse-publishing ()
  (use-package muse-publishing
      ;; :defer t
      :demand t
      :config
      (progn
        (require 'publishing)
        (progn
          (setq
           *muse-top-dir*           (expand-file-name "contents/muse" *created-content-dir*)
           *muse-top-style-dir*     (expand-file-name "generic/muse/style" *muse-top-dir*)
           *muse-generated-top-dir* (expand-file-name "gen/muse" *created-content-dir*)
           *muse-website-address*   (concat *website-address* "muse/")))
        (progn
          ;; Here is my master project listing.
          ;; (make-muse-style-spec
          ;;  (content-muse-dir "web/site/wiki/projects")
          ;;  (content-muse-publishing-dir "web/site/wiki/projects/my-xhtml")
          ;;  "my-xhtml"
          ;;  (content-muse-publishing-url "projects/"))

          (setq muse-project-alist nil)

          (add-muse-project
           `("Website"
             (
              ,(content-muse-dir "web/site/wiki/web")
               ,(content-muse-dir "web/site/wiki/web/testdir")
               :force-publish ("WikiIndex")
               :default "WelcomePage")
             (:base "my-xhtml"
                    :base-url ,(content-muse-publishing-url "web/")
                    :include "/web/[^/]+"
                    :path ,(content-muse-publishing-dir "web/site/wiki/web/my-xhtml"))
             (:base "my-xhtml"
                    :base-url ,(content-muse-publishing-url "web/")
                    :include "/testdir/[^/]+"
                    :path ,(content-muse-publishing-dir "web/site/wiki/web/testdir/my-xhtml"))
             (:base "my-pdf"
                    :base-url ,(content-muse-publishing-url "web/")
                    :path ,(content-muse-publishing-dir "doc/pdf/site/wiki/web/my-pdf")
                    :include "/\\(CurriculumVitae\\|BriefResume\\)[^/]*$")))

          (if t
              (add-muse-project
               `("Projects" ( ,(content-muse-dir "web/site/wiki/projects")
                               :force-publish ("WikiIndex" "MuseQuickStart")
                               :default "WelcomePage")
                            (:base "my-xhtml"
                                   :base-url ,(content-muse-publishing-url "web/site/wiki/projects/my-xhtml/")
                                   :path ,(content-muse-publishing-dir "web/site/wiki/projects/my-xhtml"))))

              (add-muse-project
               `("Projects"
                 ( ,(content-muse-dir "web/site/wiki/projects")
                    :force-publish ("WikiIndex" "MuseQuickStart")
                    :default "WelcomePage")

                 ,(make-muse-style-spec
                   (content-muse-dir "web/site/wiki/projects")
                   (content-muse-publishing-dir "web/site/wiki/projects/my-xhtml/")
                   "my-xhtml"
                   (content-muse-publishing-url "web/site/wiki/projects/my-xhtml/"))))
              )

          (add-muse-project
           `("WikiWriting"
             (,(content-muse-dir "web/site/wiki/writing")
               ;; :force-publish ("WikiIndex" "MuseQuickStart")
               :major-mode muse-mode
               :default "index")
             (:base "my-xhtml"
                    :base-url ,(content-muse-publishing-url "projects/")
                    :path ,(content-muse-publishing-dir "web/site/wiki/writing/my-xhtml"))))


          (add-muse-project
           `("Blog" (,@(muse-project-alist-dirs (content-muse-dir "web/site/blog"))
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
                       (content-muse-dir "web/site/blog")
                       (content-muse-publishing-dir "web/site/blog/ikiwiki")
                       "ikiwiki"
                       :base-url (content-muse-publishing-url "blog/"))))

          ;; "http://grepfind.hello.org/blog/"
          (add-muse-project
           `("MyNotes" (,(content-muse-dir "web/site/wiki/notes")
                         :force-publish ("index")
                         :default "index")
                       (:base "xhtml"
                              :base-url ,(content-muse-publishing-url "notes/")
                              :path ,(content-muse-publishing-dir "web/site/wiki/notes/xhtml"))
                       (:base "my-pdf"
                              :base-url ,(content-muse-publishing-url "notes/")
                              :path ,(content-muse-publishing-dir "web/site/wiki/notes/my-pdf"))))

          (add-muse-project
           `("_Private" (,(content-muse-dir "doc/priv"))
                        ,@(muse-project-alist-styles (content-muse-dir "doc/priv")
                                                     (content-muse-publishing-dir "doc/pdf/doc/priv/pdf")
                                                     "pdf")))

          (add-muse-project
           `("_Classes" (,@(muse-project-alist-dirs (content-muse-dir "web/site/wiki/classes"))
                           :default "index")
                        ,@(muse-project-alist-styles (content-muse-dir "web/site/wiki/classes")
                                                     (content-muse-publishing-dir "web/site/wiki/classes/xhtml")
                                                     "xhtml")))

          (add-muse-project
           `("MA366" (,(content-muse-dir "doc/pdf/classes/ma366"))
                     (:base "pdf-uh"
                            :path ,(content-muse-publishing-dir "doc/pdf/classes/ma366/pdf-uh"))))

          (add-muse-project
           `("ENGL238" (,(content-muse-dir "doc/pdf/classes/eng238"))
                       (:base "pdf-uh"
                              :path ,(content-muse-publishing-dir "doc/pdf/classes/eng238/pdf-uh"))))

          (add-muse-project
           `("CS426" (,(content-muse-dir "web/site/wiki/classes/cs426"))
                     (:base "pdf-uh"
                            :path "~/proj/classes/cs426/pdf-uh")))

          (add-muse-project
           `("_Plans" (,(content-muse-dir "web/site/wiki/plans")
                        :default "TaskPool"
                        :major-mode planner-mode
                        :visit-link planner-visit-link)
                      (:base "planner-xhtml"
                             :path ,(content-muse-publishing-dir "web/site/wiki/plans/html"))))))))

(defun lotus-publishing/init-org-publishing ()
  (use-package org-publishing
      ;; :defer t
      :demand t
      :config
      (progn
        (progn
          (setq
           *org-top-dir*           (expand-file-name "contents/org" *created-content-dir*)
           *org-top-style-dir*     (expand-file-name "generic/org/style" *org-top-dir*)
           *org-generated-top-dir* (expand-file-name "gen/org" *created-content-dir*)
           *org-website-address*   (concat *website-address* "org/")))
        (progn
          (setq org-publish-project-alist nil)

          (add-org-project
           "inherit-org-info-js"
           ;; :base-directory "~/develop/org/Worg/code/org-info-js/"
           :base-directory (content-org-dir "org-info-js/")
           :recursive t
           :base-extension "js"
           :publishing-directory (content-org-publishing-dir "js/")
           :publishing-function 'org-publish-attachment)

          (add-org-project
           "inherit-org-info-js"
           ;; :base-directory "~/develop/org/Worg/code/org-info-js/"
           :base-directory (content-org-dir "org-info-js/")
           :recursive t
           :base-extension "js"
           :publishing-directory (content-org-publishing-dir "js/")
           :publishing-function 'org-publish-attachment)

          (add-org-project
           "org-notes"
           :base-directory (content-org-dir "org/")
           :base-extension "org"
           :publishing-directory (content-org-publishing-dir "org/html/")
           :recursive t
           :publishing-function 'org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t)

          (add-org-project
           "org-static"
           :base-directory (content-org-dir "org/")
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory (content-org-publishing-dir "org/html/")
           :recursive t
           :publishing-function 'org-publish-attachment)

          (add-org-project "org" :components '("org-notes" "org-static"))

          (add-org-project
           "generic-inherit"
           :base-directory (content-org-dir "generic/")
           :recursive t
           :base-extension "css\\|js"
           :publishing-directory (content-org-publishing-dir "generic/html/")
           :publishing-function 'org-publish-attachment)

          (add-org-project
           "generic-static"
           :base-directory (content-org-dir "generic/")
           :recursive t
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory (content-org-publishing-dir "generic/html/")
           :publishing-function 'org-publish-attachment)

          (add-org-project
           "generic-org"
           :base-directory (content-org-dir "generic/")
           :auto-index t
           :auto-sitemap t
           :sitemap-title "Sitemap"
           :index-filename "sitemap.org"
           :index-title "Sitemap"
           :recursive t
           :base-extension "org"
           :publishing-directory (content-org-publishing-dir "generic/html/")
           ;; :publishing-function org-publish-org-to-html
           :publishing-function 'org-html-publish-to-html
           :headline-levels 3
           :auto-preamble t)

          (add-org-project
           "journal-notes"
           :base-directory (content-org-dir "jorunal/")
           :auto-index t
           :auto-sitemap t
           :sitemap-title "Sitemap"
           :index-filename "sitemap.org"
           :index-title "Sitemap"
           :recursive t
           :base-extension "org"
           :publishing-directory (content-org-publishing-dir "journal/html/")
           ;; :publishing-function org-publish-org-to-html
           :publishing-function 'org-html-publish-to-html
           :headline-levels 3
           :with-section-numbers nil
           :table-of-contents nil
           :auto-preamble t
           :auto-postamble nil)

          (add-org-project
           "journal"
           :base-extension "org"
           :components '("inherit-org-info-js" "generic-inherit" "journal-notes" "generic-static"))

          (add-org-project
           "tasks-notes"
           :base-directory (content-org-dir "tasks/")
           :auto-index t
           :auto-sitemap t
           :sitemap-title "Sitemap"
           :index-filename "sitemap.org"
           :index-title "Sitemap"
           :recursive t
           :base-extension "org"
           :publishing-directory (content-org-publishing-dir "tasks/html/")
           ;; :publishing-function org-publish-org-to-html
           :publishing-function 'org-html-publish-to-html
           :headline-levels 3
           :with-section-numbers nil
           :table-of-contents nil
           :auto-preamble t
           :auto-postamble nil)

          (add-org-project
           "tasks"
           :base-extension "org"
           :components '("inherit-org-info-js" "generic-inherit" "tasks-notes" "generic-static"))

          (add-org-project
           "notes-notes"
           :base-directory (content-org-dir "notes/")
           :auto-index t
           :auto-sitemap t
           :sitemap-title "Sitemap"
           :index-filename "sitemap.org"
           :index-title "Sitemap"
           :recursive t
           :base-extension "org"
           :publishing-directory (content-org-publishing-dir "notes/html/")
           ;; :publishing-function org-publish-org-to-html
           :publishing-function 'org-html-publish-to-html
           :headline-levels 3
           :with-section-numbers nil
           :table-of-contents nil
           :auto-preamble t
           :auto-postamble nil)

          (add-org-project
           "notes"
           :base-extension "org"
           :components '("inherit-org-info-js" "generic-inherit" "notes-notes" "generic-static"))))))

;;; packages.el ends here
