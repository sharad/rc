;;; packages.el --- lotus-package layer packages file for Spacemacs.
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
;; added to `lotus-package-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-package/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-package/pre-init-PACKAGE' and/or
;;   `lotus-package/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-package-packages
  '(
    ;; package
    (apt-mode :location local) ;; http://www.netfort.gr.jp/~dancer/software/downloads/list.cgi#apt-el
    (apt :location local)
    (apt-elserv :location local)
    (elget :location local)
    (helm-apt :location local)
    (package :location local)
    (package-x :location local)
    package-dev-utils-lotus)
  "The list of Lisp packages required by the lotus-package layer.

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

;; (when t

 (defun lotus-package/init-package ()
   (use-package package
       :defer t
       :config
       (progn
         (progn


           ;;; This was installed by package-install.el.
           ;;; This provides support for the package system and
           ;;; interfacing with ELPA, the package archive.
           ;;; Move this code earlier if you want to reference
           ;;; packages in your .emacs.

           ;; (when
           ;;     (load
           ;;      (expand-file-name "~/.xemacs/elpa/package.el"))
           ;;   (package-initialize))

           ;; (require 'cl)
           ;; (require 'utils-config)

           ;; (defvar package-user-dir
           ;;   (expand-file-name (convert-standard-filename "~/.emacs.d/elpa"))
           ;;   "package-user-dir")

           (defvar lotus-package-installed-archive (expand-file-name "installed-archive.el" package-user-dir) "Known Installed packages.")

           ;; (defconst *elpa-package-dir* "~/.emacs.d/elpa")

           (defvar package-archives nil "package archive")

           ;; (setq
           ;;  package-user-dir (expand-file-name (convert-standard-filename "~/.emacs.d/elpa"))

           ;;  lotus-package-installed-archive (expand-file-name "installed-archive.el" package-user-dir))

           ;; *elpa-package-dir* "~/.emacs.d/elpa"

           (when nil ;; progn

             (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

             (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))

             (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

             (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/")))


           (when nil (package-initialize))

           ;; (when (file-directory-p *elpa-package-dir*)
           ;;   (mapc #'(lambda (path)
           ;;             (when (file-directory-p path)
           ;;               (add-to-list 'load-path path)))
           ;;         (directory-files *elpa-package-dir* t "[a-zA-Z]+"))
           ;;   (byte-recompile-directory *elpa-package-dir*))

           (when nil
             (when (file-exists-p lotus-package-installed-archive)
               (when (set-difference (mapcar 'car (lotus-read-sexp lotus-package-installed-archive))
                                     (mapcar 'car package-alist))
                 (message "Your do not have all packages installed.\n install it will lotus-package-install-from-installed-archive.")))))

         (progn
           (autoload 'package-list-packages "package" "Elap Package" t)
           (autoload 'list-packages         "package" "Elap Package" t)
           (autoload 'package-install       "package" "Elap Package" t)
           (autoload 'package-require       "package" "Elap Package" nil)
           (autoload 'package-activate       "package" "Elap Package" nil)
           ))))
 ;; )

(defun lotus-package/init-apt-mode ()
  (use-package apt-mode
    :defer t
    :config
    (progn
      )))

(defun lotus-package/init-apt ()
  (use-package apt
    :defer t
    :config
    (progn
      )))

(defun lotus-package/init-apt-elserv ()
  (use-package apt-elserv
    :defer t
    :config
    (progn
      )))

(defun lotus-package/init-elget ()
  (use-package elget
    :defer t
    :config
    (progn
      )))

(defun lotus-package/init-helm-apt ()
  (use-package helm-apt
    :defer t
    :config
    (progn

      (unless (require 'el-get nil t)
        (url-retrieve
         "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
         (lambda (s)
           (goto-char (point-max))
           (eval-print-last-sexp))))

      (defvar el-get-sources nil "el-get-sources")
      ;; now either el-get is `require'd already, or have been `load'ed by the
      ;; el-get installer.
      (setq
       el-get-sources
       '(el-get				; el-get is self-hosting
         escreen            			; screen for emacs, C-\ C-h
         php-mode-improved			; if you're into php...
         switch-window			; takes over C-x o
         auto-complete			; complete as you type with overlays
         zencoding-mode			; http://www.emacswiki.org/emacs/ZenCoding

         (:name buffer-move			; have to add your own keys
                :after (lambda ()
                         (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                         (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                         (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                         (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

         (:name smex				; a better (ido like) M-x
                :after (lambda ()
                         (setq smex-save-file "~/.emacs.d/.smex-items")
                         (global-set-key (kbd "M-x") 'smex)
                         (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

         (:name magit				; git meet emacs, and a binding
                :after (lambda ()
                         (global-set-key (kbd "C-x C-z") 'magit-status)))

         (:name goto-last-change		; move pointer back to last change
                :after (lambda ()
                         ;; when using AZERTY keyboard, consider C-x C-_
                         (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

      (unless (string-match "apple-darwin" system-configuration)
        (loop for p in '(color-theme		; nice looking emacs
                         color-theme-tango	; check out color-theme-solarized
                         )
              do (add-to-list 'el-get-sources p)))

      ;;
      ;; Some recipes require extra tools to be installed
      ;;
      ;; Note: el-get-install requires git, so we know we have at least that.
      ;;
      (when (el-get-executable-find "cvs")
        (add-to-list 'el-get-sources 'emacs-goodies-el)) ; the debian addons for emacs

      (when (el-get-executable-find "svn")
        (loop for p in '(psvn    		; M-x svn-status
                         yasnippet		; powerful snippet mode
                         )
              do (add-to-list 'el-get-sources p)))

      ;; install new packages and init already installed packages
      (el-get 'sync))))

(defun lotus-package/init-package ()
  (use-package package
      :defer t
      :config
      (progn
        ;; (progn
        ;;   (push
        ;;    '("local" . "~/.xemacs/elpa/upload")
        ;;    package-archives))

        (progn

          (setq
           package-source-path "~/.xemacs/elpa/pkgs"
           package-local-dev-archive "local"
           package-archive-upload-base "~/.xemacs/elpa/upload")

          (when package-archive-upload-base
            (unless (assoc package-local-dev-archive package-archives)
              (push
               (cons package-local-dev-archive package-archive-upload-base)
               package-archives)))))))

(defun lotus-package/init-package-x ()
  (use-package package-x
      :defer t
      :commands (package-upload-file package-upload-buffer)
      :config
      (progn
        (progn
          (setq
           package-archive-upload-base "~/.xemacs/elpa/upload")))))


(defun lotus-package/init-package-dev-utils-lotus ()
  (use-package package-dev-utils-lotus
      :defer t
      ;; :commands (package-upload-file package-upload-buffer)
      :config
      (progn
        (setq
         package-source-path "~/.xemacs/elpa/pkgs"
         package-local-dev-archive "local"
         package-archive-upload-base "~/.xemacs/elpa/upload")

        (when package-archive-upload-base
          (unless (assoc package-local-dev-archive package-archives)
            (push
             (cons package-local-dev-archive package-archive-upload-base)
             package-archives))))))

;;; packages.el ends here
