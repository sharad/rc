;;; package-config.el --- package

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <>
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

(eval-after-load "package"
  '(progn

    (require 'cl)
    (require 'utils-config)

    (defvar package-user-dir
      (expand-file-name (convert-standard-filename "~/.xemacs/pkgrepos/elpa"))
      "package-user-dir")

    (defvar sharad/package-installed-archive (expand-file-name "installed-archive.el" package-user-eadiar) "Known Installed packages.")

    (defconst *elpa-package-dir* "~/.xemacs/pkgrepos/elpa")

    (setq
     package-user-dir
     (expand-file-name (convert-standard-filename "~/.xemacs/pkgrepos/elpa")))

    sharad/package-installed-archive (expand-file-name "installed-archive.el" package-user-eadiar)

    ;; *elpa-package-dir* "~/.xemacs/pkgrepos/elpa"
    )

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

    (when (file-exists-p sharad/package-installed-archive)
      (when (set-difference (mapcar 'car (sharad/read-sexp sharad/package-installed-archive))
                            (mapcar 'car package-alist))
        (message "Your do not have all packages installed.\n install it will sharad/package-install-from-installed-archive.")))))


(autoload 'package-list-packages "package" "Elap Package" t)
(autoload 'list-packages "package" "Elap Package" t)
(autoload 'package-install "package" "Elap Package" t)

(defun sharad/update-installed-package-archive ()
  (interactive)
  (require 'package)
  (if package-alist
      (write-region
       (with-output-to-string
           (pp package-alist))
       ;; (prin1-to-string package-alist)
       nil sharad/package-installed-archive)
      (error "package-alist is not defiend, not doing anything.")))

(defun sharad/package-install-from-installed-archive ()
  (interactive)
  (require 'cl)
  (let* ((packages-from-installed-archive
          (mapcar 'car (sharad/read-sexp sharad/package-installed-archive)))
         (packages-from-package-alist (mapcar 'car package-alist))
             (packages-missing (set-difference packages-from-installed-archive packages-from-package-alist)))
    (if packages-missing
        (progn
          (package-refresh-contents)
          (dolist (p packages-missing)
            (package-install p)))
        (message "No missing package found."))))

(deh-require-maybe (progn
                     apt-utils
                     apt-utils-ido
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
         (end-of-buffer)
         (eval-print-last-sexp))))

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

;;; package.el ends here
