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
    (package-x :location local))
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

(when nil
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

        (defvar package-user-dir
          (expand-file-name (convert-standard-filename "~/.xemacs/pkgrepos/elpa"))
          "package-user-dir")

        (defvar sharad/package-installed-archive (expand-file-name "installed-archive.el" package-user-dir) "Known Installed packages.")

        (defconst *elpa-package-dir* "~/.xemacs/pkgrepos/elpa")

        (defvar package-archives nil "package archive")

        (setq
         package-user-dir
         (expand-file-name (convert-standard-filename "~/.xemacs/pkgrepos/elpa"))

         sharad/package-installed-archive (expand-file-name "installed-archive.el" package-user-dir))

        ;; *elpa-package-dir* "~/.xemacs/pkgrepos/elpa"

        (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

        (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))

        (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))


        (when nil (package-initialize))

        (when (file-directory-p *elpa-package-dir*)
          (mapc #'(lambda (path)
                    (when (file-directory-p path)
                      (add-to-list 'load-path path)))
                (directory-files *elpa-package-dir* t "[a-zA-Z]+"))
          (byte-recompile-directory *elpa-package-dir*))

        (when nil
         (when (file-exists-p sharad/package-installed-archive)
          (when (set-difference (mapcar 'car (sharad/read-sexp sharad/package-installed-archive))
                                (mapcar 'car package-alist))
            (message "Your do not have all packages installed.\n install it will sharad/package-install-from-installed-archive.")))))

      (progn
        (autoload 'package-list-packages "package" "Elap Package" t)
        (autoload 'list-packages         "package" "Elap Package" t)
        (autoload 'package-install       "package" "Elap Package" t)
        (autoload 'package-require       "package" "Elap Package" nil)
        (autoload 'package-activate       "package" "Elap Package" nil)
        )))))

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
        (progn
          (push
           '("local" . "~/.xemacs/elpa/upload")
           package-archives))

        (progn

          (defvar package-source-path "~/.xemacs/elpa/pkgs")

          (defun package-make-package-desc (pkg-name &optional archive)
            (let* ((archive (or archive "local"))
                   (package (assoc
                             (intern pkg-name)
                             (let* ((contents-file (format "archives/%s/archive-contents" archive))
                                    (contents (package--read-archive-file contents-file)))
                               contents)))
                     (name (car package))
                     (version (package--ac-desc-version (cdr package)))
                     (pkg-desc
                      (package-desc-create
                       :name name
                       :version version
                       :reqs (package--ac-desc-reqs (cdr package))
                       :summary (package--ac-desc-summary (cdr package))
                       :kind (package--ac-desc-kind (cdr package))
                       :archive archive
                       :extras (and (> (length (cdr package)) 4)
                                    ;; Older archive-contents files have only 4
                                    ;; elements here.
                                    (package--ac-desc-extras (cdr package)))))
                     ;; (existing-packages (assq name package-archive-contents))
                     ;; (pinned-to-archive (assoc name package-pinned-packages))
                     )
              pkg-desc))

          (defun package-build-package-dir (dir)
            (interactive
             (let ((dir (read-directory-name "package directory: ")))
               (list dir)))
            ;; version is today date
            (let* ((dir-of-current-file (directory-file-name dir))
                   (pkg-name-version
                    (file-name-nondirectory dir-of-current-file))
                   (pkg-name
                    (replace-regexp-in-string
                     "-[0-9\.]\*\$" "" pkg-name-version))
                   (version (format-time-string "%Y%m.%H%M"))
                   (currdir-pkg-def-file
                    (expand-file-name
                     (format "%s-pkg.el" pkg-name)
                     dir-of-current-file))
                   (pkg-def
                    (let ((pkg-def-file
                           currdir-pkg-def-file))
                      (if (file-exists-p pkg-def-file)
                          (car
                           (read-from-string
                            (with-temp-buffer
                              (insert-file-contents-literally pkg-def-file)
                              (let ((contents
                                     (condition-case e
                                         ;; (read (current-buffer))
                                         (buffer-string)
                                       ('end-of-file nil))))
                                contents))))
                          `(define-package ,pkg-name ,version ni nil))))
                   (tmp-dir (expand-file-name "elpa" (getenv "TMP")))
                   (pkg-dir
                    (expand-file-name
                     (format "%s-%s" pkg-name version)
                     tmp-dir)))
              (when
                  (or (file-exists-p currdir-pkg-def-file)
                      (y-or-n-p
                       (format "Do you want to make package of %s from %s: "
                               pkg-name
                               dir-of-current-file)))
                (copy-directory dir-of-current-file pkg-dir nil t t)

                (when (file-directory-p package-source-path)
                  (unless (string-match-p
                           (concat "^"
                                   (regexp-quote
                                    (file-truename package-source-path)))
                           (file-truename dir-of-current-file))
                    (copy-directory
                     dir-of-current-file
                     (expand-file-name pkg-name package-source-path)
                     nil t t)))

                (progn
                  (setcar (nthcdr 2 pkg-def) version)
                  (unless (nth 3 pkg-def)
                    (let ((desc
                           (read-from-minibuffer (formet "package %s desc: " pkg-name))))
                      (setcar (nthcdr 3 pkg-def) desc)))
                  (unless (nth 4 pkg-def)
                    (let ((dep
                           (read-from-string (read-from-minibuffer (formet "package %s dependency: " pkg-name)))))
                      (setcar (nthcdr 4 pkg-def) dep))))
                (let ((pkgdir-def-file (expand-file-name (format "%s-pkg.el" pkg-name) pkg-dir)))
                  (with-current-buffer
                      (or (find-buffer-visiting pkgdir-def-file)
                          (find-file-noselect pkgdir-def-file))
                    ;; (expand-file-name (format "%s-pkg.el" pkg-name) pkg-dir)
                    (set-buffer-file-coding-system
                     (if (coding-system-p 'utf-8-emacs)
                         'utf-8-emacs
                         'emacs-mule))
                    (erase-buffer)
                    (insert (prin1-to-string pkg-def))
                    (write-file pkgdir-def-file))

                  ;; TODO: copy pkgdir-def-file dir-of-current-file/*-pkg.el
                  (copy-file pkgdir-def-file currdir-pkg-def-file t)

                  (let ((pkgdir-def-file-buff (find-buffer-visiting pkgdir-def-file)))
                    (when pkgdir-def-file-buff (kill-buffer pkgdir-def-file-buff))))
                (let ((default-directory tmp-dir))
                  (if (shell-command
                       (format "tar cf %s/%s-%s.tar -C %s %s-%s"
                               tmp-dir pkg-name version
                               tmp-dir
                               pkg-name version))
                      (format "%s/%s-%s.tar"
                              tmp-dir pkg-name version))))))

          (defun package-upload-package-dir (dir)
            (interactive
             (let ((dir (read-directory-name "package directory: ")))
               (list dir)))
            (let* ((pkg-tar (package-build-package-dir dir))
                   (pkg-name
                    (replace-regexp-in-string
                     "-[0-9\.]\*\.tar\\(\.gz\\)?\$" ""
                     (file-name-nondirectory pkg-tar))))
              (when (string= (file-name-extension pkg-tar) "tar")
                (package-upload-file pkg-tar)
                (if (file-directory-p package-archive-upload-base)
                    (progn
                      (let ((old-pkgs (directory-files
                                       package-archive-upload-base
                                       t
                                       (concat (regexp-quote pkg-name) "-[0-9\.]\+\.tar"))))
                        (dolist (op old-pkgs)
                          (delete-file op)))
                      (message "copy %s %s"
                               pkg-tar
                               package-archive-upload-base)
                      (copy-file pkg-tar
                                 package-archive-upload-base
                                 t))
                    (message "package-archive-upload-base not exists."))
                (package-refresh-contents))
              (package-make-package-desc pkg-name "local")))

          (defun package-install-package-dir (dir)
            (interactive
             (let ((dir (read-directory-name "package directory: ")))
               (list dir)))
            (let* ((pkg-desc (package-upload-package-dir dir))
                   (pkg-sym (package-desc-name pkg-desc))
                   (pkg-name (symbol-name pkg-sym)))
              (when (package-installed-p pkg-sym)
                (ignore-errors
                 (package-delete pkg-desc)))
              (let ((old-installed-pkgs (directory-files
                               package-user-dir
                               t
                               (concat (regexp-quote pkg-name) "-[0-9\.]\+"))))
                (dolist (oipdir old-installed-pkgs)
                  (delete-directory oipdir t)))
              (message "installing package %s" pkg-sym)
              (package-install pkg-desc)))))))

(when nil

  (package-desc-name (package-make-package-desc "sessions-unified" "local"))

  (package-install 'sessions-unified)

  (package-delete (package-make-package-desc "org-context-clocking" "local"))

  (package-install (package-make-package-desc "sessions-unified" "local"))

  (package-delete (package-make-package-desc "sessions-unified" "local"))

  (package-delete 'sessions-unified)

  (package-read-archive-contents "local")

  package-archives

  (package-read-all-archive-contents)

  (intern "xxx")

  (package- 'semi)

  (package-installed-p )

  (package-install "semi")

  (symbol-name (package-desc-name (package-make-package-desc "sessions-unified")))

  (assoc 'sessions-unified (package--read-archive-file
                            (expand-file-name "archive-contents" package-archive-upload-base))))

(defun lotus-package/init-package-x ()
  (use-package package-x
      :defer t
      :commands (package-upload-file package-upload-buffer)
      :config
      (progn




        (setq
         package-archive-upload-base
         "~/.xemacs/elpa/upload"))))

;;; packages.el ends here
