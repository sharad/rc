;;; package-config.el --- package

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords: lisp

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

;;

;;; Code:





;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.

;; (when
;;     (load
;;      (expand-file-name "~/.xemacs/elpa/package.el"))
;;   (package-initialize))

(require 'init-config "~/.xemacs/init-config.el")

(eval-after-load "package"
  '(progn

    (require 'cl)
    (require 'utils-config)

    (defvar package-user-dir
      (expand-file-name (convert-standard-filename "~/.xemacs/pkgrepos/elpa"))
      "package-user-dir")

    (defvar lotus-package-installed-archive (expand-file-name "installed-archive.el" package-user-dir) "Known Installed packages.")

    (defconst *elpa-package-dir* "~/.xemacs/pkgrepos/elpa")

    (defvar package-archives nil "package archive")

    (setq
     package-user-dir
     (expand-file-name (convert-standard-filename "~/.xemacs/pkgrepos/elpa")))

    lotus-package-installed-archive (expand-file-name "installed-archive.el" package-user-dir)

    ;; *elpa-package-dir* "~/.xemacs/pkgrepos/elpa"

    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

    (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))

    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))


    (package-initialize)

    (when (file-directory-p *elpa-package-dir*)
      (mapc #'(lambda (path)
                (when (file-directory-p path)
                  (add-to-list 'load-path path)))
            (directory-files *elpa-package-dir* t "[a-zA-Z]+"))
      (byte-recompile-directory *elpa-package-dir*))

    (when (file-exists-p lotus-package-installed-archive)
      (when (set-difference (mapcar 'car (lotus-read-sexp lotus-package-installed-archive))
                            (mapcar 'car package-alist))
        (message "Your do not have all packages installed.\n install it will lotus-package-install-from-installed-archive.")))))


(autoload 'package-list-packages "package" "Elap Package" t)
(autoload 'list-packages         "package" "Elap Package" t)
(autoload 'package-install       "package" "Elap Package" t)
(autoload 'package-require       "package" "Elap Package" nil)
(autoload 'package-activate       "package" "Elap Package" nil)

(deh-section "package detail"
  ;; Make sure a package is installed
  (defun package-require (package)
    "Install a PACKAGE unless it is already installed
or a feature with the same name is already active.

Usage: (package-require 'package)"
                                        ; try to activate the package with at least version 0.
    (package-activate package '(0))
                                        ; try to just require the package. Maybe the user has it in his local config
    (condition-case nil
        (require package)
                                        ; if we cannot require it, it does not exist, yet. So install it.
      (error (package-install package))))

  (add-hook 'lotus-enable-desktop-restore-interrupting-feature
            ;; 'lotus-enable-startup-interrupting-feature-hook
            '(lambda ()
              (run-at-time-or-now 7
               '(lambda ()
                 ;; Initialize installed packages
                 (package-initialize)
                 ;; package init not needed, since it is done anyway in emacs 24 after reading the init
                 ;; but we have to load the list of available packages
                 (package-refresh-contents))))))

(defun lotus-update-installed-package-archive ()
  (interactive)
  (require 'package)
  (if package-alist
      (write-region
       (with-output-to-string
           (pp package-alist))
       ;; (prin1-to-string package-alist)
       nil lotus-package-installed-archive)
      (error "package-alist is not defiend, not doing anything.")))

(defun lotus-package-install-from-installed-archive ()
  (interactive)
  (require 'cl)
  (let* ((packages-from-installed-archive
          (mapcar 'car (lotus-read-sexp lotus-package-installed-archive)))
         (packages-from-package-alist (mapcar 'car package-alist))
             (packages-missing (set-difference packages-from-installed-archive packages-from-package-alist)))
    (if packages-missing
        (progn
          (package-refresh-contents)
          (dolist (p packages-missing)
            (package-install p)))
        (message "No missing package found."))))

(deh-require-maybe (progn
                     ;; apt-utils
                     ;; apt-utils-ido
                     apt-mode ;; http://www.netfort.gr.jp/~dancer/software/downloads/list.cgi#apt-el
                     apt
                     apt-elserv
                     helm-apt)
  ;; make  function to install missing package.

  )



(deh-section "el-get"
  ;; http://www.emacswiki.org/emacs/el-get
  (deh-require-maybe elget
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
    (el-get 'sync)))


(provide 'package-config)
;;; package-config.el ends here
