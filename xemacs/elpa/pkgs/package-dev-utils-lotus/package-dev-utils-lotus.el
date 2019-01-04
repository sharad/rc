;;; package-dev-utils-lotus.el --- package-dev-utils-lotus

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

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

;;; Code:


(require 'package)
(require 'package-x)
(require 'package-build "~/.emacs.d/core/libs/package-build.el")

(defvar package-source-path "~/.xemacs/elpa/pkgs" "Source code path for packages.")
(defvar package-archive-upload-base "~/.xemacs/elpa/upload")
(defvar package-local-dev-archive "local" "Local archive specified in package-archives")
(defvar *package-install-packages-wait-secs-in-install* 7)

(unless (assoc package-local-dev-archive package-archives)
  (push
   (cons package-local-dev-archive package-archive-upload-base)
   package-archives))

(unless (functionp 'directory-files-recursively)
  (defun directory-files-recursively (directory regexp &optional include-directories)
    "Return all files under DIRECTORY whose names match REGEXP.
This function searches the specified directory and its
sub-directories, recursively, for files whose basenames (i.e.,
without the leading directories) match the specified regexp, and
returns a list of the absolute file names of the matching
files (see absolute file names). The file names are returned in
depth-first order, meaning that files in some sub-directory are
returned before the files in its parent directory. In addition,
matching files found in each subdirectory are sorted
alphabetically by their basenames. By default, directories whose
names match regexp are omitted from the list, but if the optional
argument INCLUDE-DIRECTORIES is non-nil, they are included"
    (let* ((files-list '())
           (current-directory-list
            (directory-files directory t)))
      ;; while we are in the current directory
      (while current-directory-list
        (let ((f (car current-directory-list)))
          (cond
            ((and
              (file-directory-p f)
              (file-readable-p f)
              (if include-directories (not (string-match regexp f)) t)
              (not (string-equal ".." (substring f -2)))
              (not (string-equal "." (substring f -1))))
             ;; recurse only if necessary
             (setq files-list (append files-list (directory-files-recursively f regexp include-directories))))
            ((and
              (file-regular-p f)
              (file-readable-p f)
              (string-match regexp f))
             (setq files-list (cons f files-list)))
            (t)))
        (setq current-directory-list (cdr current-directory-list)))
      files-list)))

(defun package-install-local (pkg-desc)
  (let ((package-archives (list
                           (cons package-local-dev-archive package-archive-upload-base))))
    (message "Installing locally")
    (package-install pkg-desc)))

(defun package-make-package-desc (pkg-name &optional archive)
  (let* ((archive (or archive package-local-dev-archive))
         (package (assoc
                   (intern pkg-name)
                   (let* ((contents-file (format "archives/%s/archive-contents" archive))
                          (contents (package--read-archive-file contents-file)))
                     contents))))
    (if package
        (let* ((name (car package))
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
          pkg-desc)
        (error "not able to find package for %s" pkg-name))))

(defun package-desc-package-from-dir (dir &optional archive)
  (let* ((dir-of-current-file (directory-file-name dir))
         (pkg-name-version
          (file-name-nondirectory dir-of-current-file))
         (pkg-name
          (replace-regexp-in-string
           "-[0-9\.]\*\$" "" pkg-name-version))
         (version
          (package-version-join
           (package-build--valid-version
            (format-time-string "%Y%m%d.%H%M")))))
    (package-make-package-desc pkg-name package-local-dev-archive)))

(defun package-requirements-package-from-dir (dir)
  (let* ((dir-of-current-file (directory-file-name dir))
         (pkg-name-version
          (file-name-nondirectory dir-of-current-file))
         (pkg-name
          (replace-regexp-in-string
           "-[0-9\.]\*\$" "" pkg-name-version))
         (version
          (package-version-join
           (package-build--valid-version
            (format-time-string "%Y%m%d.%H%M"))))
         (currdir-pkg-def-file
          (expand-file-name
           (format "%s-pkg.el" pkg-name)
           dir-of-current-file))
         (pkg-def-exists (file-exists-p currdir-pkg-def-file))
         (pkg-def
          (let ((pkg-def-file currdir-pkg-def-file))
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
                `(define-package ,pkg-name ,version ,(format "%s" pkg-name) nil)))))
    (cadr (nth 4 pkg-def))))

;;;###autoload
(defun package-build-package-from-dir (dir &optional update-source-pkg-desc)
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
         (version
          (package-version-join
           (package-build--valid-version
            (format-time-string "%Y%m%d.%H%M"))))
         (currdir-pkg-def-file
          (expand-file-name
           (format "%s-pkg.el" pkg-name)
           dir-of-current-file))
         (pkg-def-exists (file-exists-p currdir-pkg-def-file))
         (pkg-def
          (let ((pkg-def-file currdir-pkg-def-file))
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
                `(define-package ,pkg-name ,version ,(format "%s" pkg-name) nil))))
         (tmp-dir (expand-file-name "elpa" (getenv "TMP")))
         (pkg-dir
          (expand-file-name
           (format "%s-%s" pkg-name version)
           tmp-dir)))

    (message "building package %s" pkg-name)
    (when (or (file-exists-p currdir-pkg-def-file)
              (y-or-n-p
               (format "Do you want to make package of %s from %s: "
                       pkg-name
                       dir-of-current-file)))
      ;; add org-tangle-file here
      (let ((default-directory dir-of-current-file))
        (dolist (org-file (directory-files dir-of-current-file t "'\*\.org$"))
          (org-babel-tangle-file org-file)))
      (copy-directory dir-of-current-file pkg-dir nil t t)
      ;; delete unnecessary files
      (let ((default-directory pkg-dir))
        (dolist (del-file (directory-files-recursively pkg-dir "'\*~$\\|'RCS$"))
          (delete-file del-file)))

      (if (file-directory-p package-source-path)
          (unless (string-match-p
                   (concat "^"
                           (regexp-quote
                            (file-truename package-source-path)))
                   (file-truename dir-of-current-file))
            (copy-directory
             dir-of-current-file
             (expand-file-name pkg-name package-source-path)
             nil t t))
          (error "package-source-path do ot exists."))
      (setcar (nthcdr 2 pkg-def) version)
      (unless pkg-def-exists ;; version exist mean file -pkg.el was there presently, so need to ask any question.
        (progn
          (setcar (nthcdr 2 pkg-def) version)
          (unless (nth 3 pkg-def)
            (let ((desc
                   (read-from-minibuffer (format "package %s desc: " pkg-name))))
              (setcar (nthcdr 3 pkg-def) desc)))
          (when nil
            (unless (nth 4 pkg-def)
              (let ((dep
                     (car (read-from-string (read-from-minibuffer (format "package %s dependency: " pkg-name))))))
                (setcar (nthcdr 4 pkg-def) dep))))))

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
          (insert (pp-to-string pkg-def))
          (write-file pkgdir-def-file))

        (when (or (not pkg-def-exists)
                  update-source-pkg-desc)
          (message "Creating %s file" currdir-pkg-def-file)
          ;; TODO: copy pkgdir-def-file dir-of-current-file/*-pkg.el
          (copy-file pkgdir-def-file currdir-pkg-def-file t))

        (let ((pkgdir-def-file-buff (find-buffer-visiting pkgdir-def-file)))
          (when pkgdir-def-file-buff (kill-buffer pkgdir-def-file-buff))))
      (let ((default-directory tmp-dir))
        (prog1
            (if (shell-command
                 (format "tar cf %s/%s-%s.tar -C %s %s-%s"
                         tmp-dir pkg-name version
                         tmp-dir
                         pkg-name version))
                (format "%s/%s-%s.tar"
                        tmp-dir pkg-name version))
          (message "built package %s" pkg-name))))))

;;;###autoload
(defun package-upload-package-from-dir (dir &optional archive)
  (interactive
   (let ((dir (read-directory-name "package directory: ")))
     (list dir)))
  (let* ((pkg-tar (package-build-package-from-dir dir))
         (pkg-name
          (replace-regexp-in-string
           "-[0-9\.]\*\.tar\\(\.gz\\)?\$" ""
           (file-name-nondirectory pkg-tar))))
    (message "uploading package %s" pkg-name)
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
          (error "package-archive-upload-base not exists."))
      (package-refresh-contents))
    (message "uploaded package %s" pkg-name)
    (package-make-package-desc pkg-name (or archive package-local-dev-archive))))

;;;###autoload
(defun package-install-package-from-dir (dir)
  (interactive
   (let ((dir (read-directory-name "package directory: ")))
     (list dir)))
  (let* ((pkg-desc (package-upload-package-from-dir dir))
         (pkg-sym  (package-desc-name pkg-desc))
         (pkg-name (symbol-name pkg-sym)))
    (when (package-installed-p pkg-sym)
      ;; (package-delete pkg-desc)
      (when pkg-desc
        (ignore-errors
         (package-delete pkg-desc))))

    (let ((old-installed-pkgs (directory-files
                               package-user-dir
                               t
                               (concat (regexp-quote pkg-name) "-[0-9\.]\+"))))
      (dolist (oipdir old-installed-pkgs)
        (delete-directory oipdir t)))
    (message "installing package %s" pkg-sym)
    (unless (package-install-local pkg-desc)
      (package-install pkg-desc))
    (message "installed package %s" pkg-sym)))


;;;###autoload
(defun package-build-packages-from-source-path (&optional base)
  (interactive)
  (let ((base (or base package-source-path)))
    (dolist (f (directory-files base))
      (let ((pkgdir (expand-file-name f base)))
        (when (and (file-directory-p pkgdir)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (package-build-package-from-dir pkgdir))))))
;;;###autoload
(defun package-upload-packages-from-source-path (&optional base)
  (interactive)
  (let ((base (or base package-source-path)))
    (dolist (f (directory-files base))
      (let ((pkgdir (expand-file-name f base)))
        (when (and (file-directory-p pkgdir)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (package-upload-package-from-dir pkgdir))))))
;;;###autoload
(defun package-install-packages-from-source-path (&optional base)
  (interactive)
  (let ((base (or base package-source-path)))
    (dolist (f (directory-files base))
      (let ((pkgdir (expand-file-name f base)))
        (sleep-for *package-install-packages-wait-secs-in-install*)
        (when (and (file-directory-p pkgdir)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (ignore-errors
            (package-install-package-from-dir pkgdir))
          (message "Installed %s" pkgdir))))
    (message "Installed all packages from %s" base)))

;;;###autoload
(defun package-install-packages-from-source-path-fast (&optional base)
  (interactive)
  (let ((base (or base package-source-path)))

    ;; First build and upload all subdirs
    (progn
      (package-upload-packages-from-source-path base)
      (message "Uploaded all packages from %s" base))

    (let* ((subdirs
            (remove-if-not
             #'(lambda (d) (file-directory-p (expand-file-name d base)))
             (remove ".."(remove "." (directory-files base)))))
           (subdir-paths
            (mapcar
             #'(lambda (d) (expand-file-name d base))
             subdirs))
           (subdir-paths
            (remove-if-not
             #'file-directory-p
             subdir-paths))
           (dependencies-with-version
            (apply #'append
                   (remove nil
                           (mapcar
                            #'package-requirements-package-from-dir
                            subdir-paths))))
           (dependencies-without-version
            (delete-dups
             (mapcar 'car dependencies-with-version)))
           (dependencies-external
            (remove-if
             #'(lambda (d)
                 (file-directory-p
                  (expand-file-name (symbol-name d) base)))
             dependencies-without-version))
           ;; (dependencies-external
           ;;  (delete-dups
           ;;   dependencies-external))
           )

      ;; Than find all dependencies and install them
      ;; try to install dependencies first
      (progn
        (message "depecdencies to be installed %s" dependencies-external)
        (dolist (dep dependencies-external)
          (let ((dep-sym  (car (assoc dep package-archive-contents)))
                (dep-desc (cadr (assoc dep package-archive-contents))))
            (if (package-installed-p dep-sym)
                (message "dependency package %s already installed." dep)
                (progn
                  (message "installing dep %s %s" dep (symbolp dep))
                  (package-install dep-desc)
                  (sleep-for *package-install-packages-wait-secs-in-install*)
                  (message "Installed dependency package %s" dep)))))
        (message "dependencies installed %s" dependencies-external))

      ;; now install all subdirs.
      (progn
        (dolist (pkg-path subdir-paths)
          ;; as already uploaded.
          (let (;; (dep-desc (cadr (assoc dep package-archive-contents)))
                (pkg (package-desc-package-from-dir pkg-path)))
            (message "Installing %s" pkg-path)
            (package-install pkg)
            (sleep-for *package-install-packages-wait-secs-in-install*)
            (message "Installed %s" pkg-path)))
        (message "Installed all packages from %s" base)))))

(defun package-requirement-from-package (pkg) ;uploaded package
  (let ((pkg-desc (if (package-desc-p pkg) pkg (package-make-package-desc pkg))))
    (mapcar 'car '(package-desc-reqs pkg-desc))))


;; check about
;; package-compute-transaction

;;;###autoload
(defun lotus-package-delete (pkg)
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (unless package--initialized
       (package-initialize t))
     (unless package-archive-contents
       (package-refresh-contents))
     (list (intern (completing-read
                    "Install package: "
                    (delq nil
                          (mapcar (lambda (elt)
                                    (when (package-installed-p (car elt))
                                      (symbol-name (car elt))))
                                  ;; package-archive-contents
                                  package-alist))
                    nil t)))))
  (add-hook 'post-command-hook #'package-menu--post-refresh)
  (let ((pkg-desc (if (package-desc-p pkg)
                      pkg
                    (cadr (assq pkg package-alist)))))
    (if pkg-desc
        (if (package-installed-p (package-desc-name pkg-desc))
            (package-delete pkg-desc t)
          (message "package %s is not installed" pkg))
      (message "No such package %s" pkg))))

;;;###autoload
(defun lotus-package-autoremove ()
  "Remove packages that are no more needed.

Packages that are no more needed by other packages in
`package-selected-packages' and their dependencies
will be deleted."
  (interactive)
  ;; If `package-selected-packages' is nil, it would make no sense to
  ;; try to populate it here, because then `package-autoremove' will
  ;; do absolutely nothing.
  (when (or package-selected-packages
            (yes-or-no-p
             (format-message
              "`package-selected-packages' is empty! Really remove ALL packages? ")))
    (let* ((removable
             (package--removable-packages))

           (removable
             (remove-if-not
               #'(lambda (p)
                   (let* ((pdesc
                            (cadr (assq p package-alist)))
                          (dir (if pdesc
                                   (package-desc-dir pdesc))))
                     (string-prefix-p (file-name-as-directory
                                       (expand-file-name package-user-dir))
                                      (expand-file-name dir))))
               removable)))

      (if removable
          (when (y-or-n-p
                 (format "%s packages will be deleted:\n%s, proceed? "
                         (length removable)
                         (mapconcat #'symbol-name removable ", ")))
            (mapc (lambda (p)
                    (package-delete (cadr (assq p package-alist)) t))
                  removable))
          (message "Nothing to autoremove")))))



;; check about
;; package-compute-transaction

(when nil
  (package-desc-p (cdar package-archive-contents))
  (caar package-archive-contents)
  (assoc
   'yasnippet-classic-snippets
   package-archive-contents))

;;;###autoload
(defun package-resolve-org-plus-contrib-upgrade ()
  (interactive)
  (message "started package-resolve-org-plus-contrib-upgrade")
  (let ((pkgs '(ox-pandoc
                orgit
                org-present
                org-category-capture
                org-projectile
                org-brain
                ob-elixir
                ob-async
                elfeed-org
                org-password-manager
                org-plus-contrib)))

    (dolist (p pkgs)
      (lotus-package-delete p))

    (dolist (p (reverse pkgs))
      (package-install p)))
  (message "finished package-resolve-org-plus-contrib-upgrade"))


(provide 'package-dev-utils-lotus)
;;; package-dev-utils-lotus.el ends here
