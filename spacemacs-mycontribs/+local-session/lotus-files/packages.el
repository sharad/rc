;;; packages.el --- lotus-files layer packages file for Spacemacs.
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
;; added to `lotus-files-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-files/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-files/pre-init-PACKAGE' and/or
;;   `lotus-files/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-files-packages
  '(
    ;; (PACKAGE :location local)
    helm
    (files :location local)
    find-dired
    find-file-in-project
    lusty-explorer
    (ff-paths :location local)
    filecache
    (iswitchb :location local)
    (iswitchb-fc :location local)
    (ifind-mode :location local)
    ffw
    multibackup
    (ff-relativedir :location local)
    ;; find-files-unified
    (locations :location local)
    ffap
    aok
    )
  "The list of Lisp packages required by the lotus-files layer.

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


(defun lotus-files/post-init-helm ()
  (use-package helm
      :defer t
      :config
      (progn
        (setq
         helm-ff-skip-boring-files t
         ))))

(defun lotus-files/init-files ()
  (use-package files
      :defer t
      :config
      (progn

        (progn
          (setq
           view-read-only t
           ))

        (progn
          (defconst *workdirectory* (expand-file-name "paradise/" "~/.."))
          (when (and
                 (boundp '*workdirectory*)
                 (file-directory-p *workdirectory*))
            (cd *workdirectory*)))

        (progn
          (setq revert-without-query '("^/scp:" "^/ssh:")))

        (progn
          (when nil
            ;; (deh-section "File no writable problem"
            ;; not solved the problems. code now more complex in files.el
            (make-variable-buffer-local
             (defvar my-override-mode-on-save nil
               "Can be set to automatically ignore read-only mode of a file when saving."))

            (defadvice file-writable-p (around my-overide-file-writeable-p act)
              "override file-writable-p if `my-override-mode-on-save' is set."
              (setq ad-return-value (or
                                     my-override-mode-on-save
                                     ad-do-it)))

            (defun lotus-override-toggle-read-only ()
              "Toggle buffer's read-only status, keeping `my-override-mode-on-save' in sync."
              (interactive)
              (setq my-override-mode-on-save (not my-override-mode-on-save))
              (toggle-read-only))

            (ad-disable-advice 'file-writable-p 'around 'my-overide-file-writeable-p)
            (ad-remove-advice 'file-writable-p 'around 'my-overide-file-writeable-p)
            (ad-update 'file-writable-p)
            ))

        (progn ;; "ff-mode"
          (defvar ff-mode-map
            (let ((map (make-sparse-keymap)))
              ;; These bindings roughly imitate those used by Outline mode.
              ;;(define-key map "\C-c@\C-c"	      'hs-toggle-hiding)
              ;;(define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
              map)
            "Keymap for hideshow minor mode.")

          (unless (and
                   (boundp 'ff-mode-map)
                   ff-mode-map) ; if it is not already defined
            ;; from: http://ergoemacs.org/emacs/elisp_menu_for_major_mode.html
            ;; assign command to keys
            (setq ff-mode-map (make-sparse-keymap)))

          (define-minor-mode ff-mode
              "Prepare for working with collarative office project."
            :init-value 1
            ;; :lighter " all finder"
            :global t
            :keymap ff-mode-map
            (when office-mode
              (message "calling ff mode")))))))

(defun lotus-files/init-find-dired ()
  (use-package find-dired
      :defer t
      :config
      (progn
        )))

(defun lotus-files/init-find-file-in-project ()
  (use-package find-file-in-project
      :defer t
      :config
      (progn
        (progn ;; find-file-in-project
          ;; If non-nil, this function is called to determine the project root.
          (setq
           ;; ffip-project-root-function nil
           ffip-project-root-function (lambda () (let ((dir (ido-read-directory-name "dir: " default-directory)))
                                                   (message "dir: %s" dir)
                                                   dir))
           ;; define suitable functon for it.
           ;; ffip-project-root "~/"
           ffip-project-root nil
           ffip-patterns (append '("*.cpp" "*.h" "*.c") ffip-patterns))

          (defun ffip-set-project-root ()
            (interactive)
            (setq
             ffip-project-root (ido-read-directory-name "FFip Root Dir: " ffip-project-root)))


          (defun ffip-project-files ()          ;make it for tramp files
            "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
            (let ((file-alist nil)
                  (root (expand-file-name (or ffip-project-root (ffip-project-root)
                                              (error "No project root found")))))
              (mapcar (lambda (file)
                        (if ffip-full-paths
                            (cons (substring (expand-file-name file) (length root))
                                  (expand-file-name file))
                            (let ((file-cons (cons (file-name-nondirectory file)
                                                   (expand-file-name file))))
                              (when (assoc (car file-cons) file-alist)
                                (ffip-uniqueify (assoc (car file-cons) file-alist))
                                (ffip-uniqueify file-cons))
                              (add-to-list 'file-alist file-cons)
                              file-cons)))
                      (split-string (shell-command-to-string
                                     (format "find %s -type f \\( %s \\) %s | head -n %s"
                                             root (ffip-join-patterns)
                                             ffip-find-options ffip-limit))))))))))

(defun lotus-files/init-lusty-explorer ()
  (use-package lusty-explorer
      :defer t
      :config
      (progn
        (progn ;; lusty-explorer

          (find-file-wizard-add
           "lusty"
           ;; ido-find-file
           (lambda (initstr)
             (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
             (lusty-file-explorer))

           (lambda (ff initial-string)
             (let ((lusty-setup-hook
                    (cons
                     (lambda ()
                       (define-key lusty-mode-map
                           (kbd "s-f") ;; (plist-get plist :key)
                         (lambda (arg)
                           (interactive "P")
                           (setq initial-string ido-text
                                 ;; ido-text 'fallback-wizard
                                 ;; ido-exit 'done
                                 )
                           ;; (exit-minibuffer)
                           (throw 'nextff (list 'next (list (cons :initial-string initial-string)))))))
                     lusty-setup-hook)))
               (funcall ff initial-string))))))))

(defun lotus-files/init-ff-paths ()
  (use-package ff-paths
    :defer t
    :init
    (when (fboundp 'ff-paths-install)
      (ff-paths-install))
    :config
    (progn
      (progn ;; ff-paths
        ;; (ff-paths-install)

        ;;FIX
        (setq buf nil)

        ;; FIX
        (defun ff-paths-locate (filename)
          "Try finding FILENAME using the locate command.
Return a string if a single match, or a list if many matches."
          (let ((ff-buffer (get-buffer-create "*ff-paths-locate*"))
                status matches
                (count 0))
            (save-excursion
              (with-current-buffer ff-buffer
                (let ((default-directory (if (file-directory-p default-directory) default-directory "~/")))
                  (setq status
                        (call-process "sh" nil t nil "-c"
                                      (concat "locate " (shell-quote-argument filename)))))
                (goto-char 1)
                (if (eq status 1)
                    nil                           ;Not found...
                  (while (and (or (not (boundp 'ff-paths-locate-max-matches))
                                  (not ff-paths-locate-max-matches)
                                  (> ff-paths-locate-max-matches count))
                              (re-search-forward (if (and (boundp 'ff-paths-gzipped)
                                                          ff-paths-gzipped)
                                                     (concat "/" filename "\\(.gz\\)?$")
                                                   (concat "/" filename "$"))
                                                 nil t))
                    (let ((the-file (buffer-substring (progn (beginning-of-line)(point))
                                                      (progn (end-of-line)(point)))))
                      (setq count (1+ count))
                      (if (and (file-exists-p the-file)
                               (not (file-directory-p the-file)))
                          (setq matches (cond ((not matches)
                                               (list the-file))
                                              (t
                                               (cons the-file matches))))))))
                (if (and (boundp 'ff-paths-locate-max-matches)
                         ff-paths-locate-max-matches
                         (<= ff-paths-locate-max-matches count))
                    (setq ff-paths-have-reached-locate-max t))
                (kill-buffer ff-buffer)
                matches))))))))

(defun lotus-files/post-init-filecache ()
  (use-package filecache
      :defer t
      :config
      (progn
        (progn ;; filecache



          ;; {{ http://emacswiki.org/emacs/FileNameCache#toc2
          (defun file-cache-save-cache-to-file (file)
            "Save contents of `file-cache-alist' to FILE.
For later retrieval using `file-cache-read-cache-from-file'"
            (interactive "FFile: ")
            (with-temp-file (expand-file-name file)
              (prin1 file-cache-alist (current-buffer))))

          ;;   (defun file-cache-read-cache-from-file (file)
          ;;     "Clear `file-cache-alist' and read cache from FILE.
          ;; The file cache can be saved to a file using
          ;; `file-cache-save-cache-to-file'."
          ;;     (interactive "fFile: ")
          ;;     (file-cache-clear-cache)
          ;;     (let ((buf (find-file-noselect file)))
          ;;       (setq file-cache-alist (read buf))
          ;;       (kill-buffer buf)))

          (defun file-cache-read-cache-from-file (file)
            "Clear `file-cache-alist' and read cache from FILE.
  The file cache can be saved to a file using
  `file-cache-save-cache-to-file'."
            (interactive "fFile: ")
            (file-cache-clear-cache)
            (save-excursion
              (set-buffer (find-file-noselect file))
              (beginning-of-buffer)
              (setq file-cache-alist (read (current-buffer)))))

          (add-hook 'after-init-hook '(lambda ()
                                       (file-cache-read-cache-from-file "~/.file_cache")))

          (add-hook 'kill-emacs-hook '(lambda ()
                                       (file-cache-save-cache-to-file "~/.file_cache")))

          ;;}}


          ;; http://emacswiki.org/emacs/FileNameCache
          (defun file-cache-add-this-file ()
            (and buffer-file-name
                 (file-exists-p buffer-file-name)
                 (file-cache-add-file buffer-file-name)))
          (add-hook 'find-file-hook 'file-cache-add-this-file)


          ;; (file-cache-read-cache-from-file "~/.file_cache")

          (defun file-cache-ido-find-file (file)
            "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
            (interactive (list (file-cache-ido-read "File: "
                                                    (mapcar car file-cache-alist))))
            (let* ((record (assoc file file-cache-alist)))
              (find-file
               (expand-file-name
                file
                (if (= (length record) 2)
                    (car (cdr record))
                    (file-cache-ido-read
                     (format "Find %s in dir: " file) (cdr record)))))))

          (defun file-cache-ido-read (prompt choices)
            (let ((ido-make-buffer-list-hook
                   (lambda ()
                     (setq ido-temp-list choices))))
              (ido-read-buffer prompt)))


          (defun jcl-file-cache-ido-find-file ()
            "Open a file from the file cache.
First select a file from `file-cache-alist'.  If the file exist
in more than one directory one is asked to select which to open.
If you find out that the desired file is not present in the file
cache then you may want to fallback to normal ido find file with
C-f.
Bind this command to C-x C-f to get:

 C-x C-f         -> Open file in filecache.
 C-x C-f C-f     -> Open file with normal ido.
 C-x C-f C-f C-f -> Open file with vanilla find-file.
"
            (interactive)
            (let* (jcl-ido-text
                   (file (let ((ido-setup-hook (cons (lambda ()
                                                       (define-key ido-completion-map [(control ?f)]
                                                         (lambda (arg)
                                                           (interactive "P")
                                                           (if jcl-ido-text
                                                               (ido-magic-forward-char arg)
                                                               (setq jcl-ido-text ido-text
                                                                     ido-text 'fallback-from-cache
                                                                     ido-exit 'done)
                                                               (exit-minibuffer)))))
                                                     ido-setup-hook)))
                           (ido-completing-read "Cached File: "
                                                (mapcar 'car file-cache-alist)))))
              (if (eq file 'fallback-from-cache)
                  (progn
                    (setq minibuffer-history (delete 'fallback-from-cache minibuffer-history))
                    (ido-file-internal ido-default-file-method
                                       nil
                                       nil
                                       "Ido Find File: "
                                       nil
                                       jcl-ido-text))
                  (let ((record (assoc file file-cache-alist)))
                    (find-file
                     (expand-file-name
                      file
                      (if (= (length record) 2)
                          (cadr record)
                          (ido-completing-read (format "Find %s in dir: " file)
                                               (cdr record)
                                               nil
                                               t))))))))

          )

        (progn ;; "Find file in other dirs."
          (defun find-file-in-other-dir (&optional not-same-location)
            (interactive)
            (let ((dirs
                   (remove* (file-name-directory buffer-file-name)
                            (cdr (assoc-string (file-name-nondirectory buffer-file-name) file-cache-alist))
                            :test #'string-equal)))
              (if dirs (find-file-existing (concat (ido-completing-read "dirs: " dirs) "/" (file-name-nondirectory buffer-file-name))))))))))

(defun lotus-files/init-iswitchb ()
  (use-package iswitchb
      :defer t
      :config
      (progn
        ;; (global-set-key "\C-cf" 'file-cache-iswitchb-file)
        (progn
          ;; iswitchb-fc: Integrate file-cache with iswitchb

          ;; http://tao.uab.es/cgi-bin/archzoom.cgi/jao@gnu.org--2004/unix--emacs--0--patch-23/other/iswitchb-fc.el?download

          ;; That site seems no longer to be available (2012-07-23). This seems to work: https://github.com/emacsmirror/iswitchb-fc/blob/master/iswitchb-fc.el – DrewAdams

          ;; I think iswitchb-fc is awesome!! – Anonymous

          (defun iswitchb-fc-read-buffer (prompt &optional default existing)
            (save-window-excursion (buffer-name (iswitchb))))
          (defadvice read-buffer (around iswitchb-fc-read-buffer)
            (setq ad-return-value (iswitchb-fc-read-buffer prompt)))

          ;; patch to integrate find-file – rubikitch
          ;; Using iswitchb to open files from file name cache -- take two

          ;; Filecache rules but I did not like the way it completes the file names. Iswitchb-fc above is a really cool enhancement to filecache but I did not like that it was integrated with ‘C-x b’ (I guess I want to know what files I actually have open). Hence this little hack:

          (defun file-cache-iswitchb-file ()
            "Using iswitchb, interactively open file from file cache'.
First select a file, matched using iswitchb against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
            (interactive)
            (let* ((file (file-cache-iswitchb-read "File: "
                                                   (mapcar
                                                    (lambda (x)
                                                      (car x))
                                                    file-cache-alist)))
                   (record (assoc file file-cache-alist)))
              (find-file
               (concat
                (if (= (length record) 2)
                    (car (cdr record))
                    (file-cache-iswitchb-read
                     (format "Find %s in dir: " file) (cdr record))) file))))

          (defun file-cache-iswitchb-read (prompt choices)
            (let ((iswitchb-make-buflist-hook
                   (lambda ()
                     (setq iswitchb-temp-buflist choices))))
              (iswitchb-read-buffer prompt)))

          ;; I bind ‘C-c f’ to it:

          ;; (global-set-key "\C-cf" 'file-cache-iswitchb-file)

          ;; Happy happy, joy joy! – MaDa
          ))))

(defun lotus-files/init-iswitchb-fc ()
  (use-package iswitchb-fc
      :defer t
      :config
      (progn
        ;; (global-set-key "\C-cf" 'file-cache-iswitchb-file)
        )))

(defun lotus-files/init-ifind-mode ()
  (use-package ifind-mode
      :defer t
      :config
      (progn
        (progn
          (defvar workspace-dir nil)
          (setq workspace-dir nil)))))

(defun lotus-files/init-ffw ()
  (use-package ffw
    :defer t
    :config
    (progn
      (global-set-key-if-unbind (kbd "s-x s-f") 'find-file-wizard)
      )))

(defun lotus-files/init-multibackup ()
  (use-package multibackup
      :defer t
      :config
      (progn
        )))

(defun lotus-files/init-ff-relativedir ()
  (use-package ff-relativedir
      :defer t
      :config
      (progn
        )))

;; (defun lotus-files/init-find-files-unified ()
;;   (use-package find-files-unified
;;       :defer t
;;       :config
;;       (progn
;;         )))

(defun lotus-files/init-locations ()
  (use-package locations
      :defer t
      :config
      (progn
        ;; may be good
        ;; https://www.assembla.com/code/saintamh/subversion/nodes/2389/emacs/.emacsd/locations.el
        )))

(defun lotus-files/init-ffap ()
  (use-package ffap
      :defer t
      :config
      (progn
        (progn
          (defun ignore-ffap-p (name abs default-directory)
            (string-match "\\*\\*\\*\\*" name))

          (defadvice ffap-file-at-point (around ignore-ffap activate)
            "calculate ignore criteria to no call ffap"
            ;; Note: this function does not need to look for url's, just
            ;; filenames.  On the other hand, it is responsible for converting
            ;; a pseudo-url "site.com://dir" to an ftp file name
            (let* ((case-fold-search t)		; url prefixes are case-insensitive
                   (data (match-data))
                   (string (ffap-string-at-point)) ; uses mode alist
                   (name
                    (or (condition-case nil
                            (and (not (string-match "//" string)) ; foo.com://bar
                                 (substitute-in-file-name string))
                          (error nil))
                        string))
                   (abs (file-name-absolute-p name))
                   (default-directory default-directory)
                   (oname name))
              (unless (ignore-ffap-p name abs default-directory)
                ad-do-it))))

        (progn
          (use-package ido
              :defer t
              :config
              (progn
                (defun ido-plain-directory ()
                  "Read current directory again.
May be useful if cached version is no longer valid, but directory
timestamp has not changed (e.g. with ftp or on Windows)."
                  (interactive)
                  (if (and ido-mode (memq ido-cur-item '(file dir)))
                      (progn
                        (if (ido-is-unc-root)
                            (setq ido-unc-hosts-cache t)
                            (ido-remove-cached-dir ido-current-directory))
                        (setq ido-current-directory default-directory)
                        ;; (setq ido-text-init ido-text)
                        (setq ido-text-init "")
                        (setq ido-rotate-temp t)
                        (setq ido-exit 'refresh)
                        (exit-minibuffer))))

                ;; ido-file-completion-map is only defined when ido-mode is called.
                ;; (add-hook 'ido-setup-hook
                ;;           (lambda ()
                ;;             (keymap-set-key-if-unbind
                ;;              ido-file-completion-map
                ;;              (kbd "C-.")
                ;;              'ido-plain-directory)))
                )))))
  (when nil

    ;; set bindings
    ;; (ffap-bindings)
    (add-to-list 'helm-for-files-preferred-list
                 'helm-c-source-ffap-line)
    (add-to-list 'helm-for-files-preferred-list
                 'helm-c-source-ffap-guesser)

    (setq
     helm-for-files-preferred-list
     '(  helm-c-source-locate))

    (pron
     (require 'helm-config)
     (require 'helm-misc)
     (require 'helm-projectile)
     (require 'helm-mode)
     (require 'helm-match-plugin)
     (require 'helm-buffers)
     (require 'helm-files)
     (require 'helm-locate))

    (setq old-helm-for-files-preferred-list helm-for-files-preferred-list)
    (setq helm-for-files-preferred-list old-helm-for-files-preferred-list )

    (require 'helm-ffap)
    (setq
     helm-for-files-preferred-list
     '(helm-source-buffers-list helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-files-in-current-dir helm-source-locate))

    (use-package helm-match-plugin
        :ensure t)

  ;; (setq helm-for-files-preferred-list
  ;;   '(helm-c-source-ffap-line
  ;;     helm-c-source-ffap-guesser
  ;;     helm-c-source-buffers-list
  ;;     helm-c-source-recentf
  ;;     helm-c-source-bookmarks
  ;;     helm-c-source-file-cache
  ;;     helm-c-source-files-in-current-dir+
  ;;     helm-c-source-locate)

    ;; (helm-source-buffers-list helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-files-in-current-dir helm-source-locate)
  ))

(defun lotus-files/init-aok ()
  (use-package aok
      :defer t
      :config
      (progn
        )))

;;; packages.el ends here
