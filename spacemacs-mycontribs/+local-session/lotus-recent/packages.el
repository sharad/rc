;;; packages.el --- lotus-recent layer packages file for Spacemacs.
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
;; added to `lotus-recent-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-recent/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-recent/pre-init-PACKAGE' and/or
;;   `lotus-recent/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-recentS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-recent-packages
  '(
    ;; (PACKAGE :location local)
    recentf
    (recentf-buffer :location local)
    recentf-ext
    )
  "The list of Lisp packages required by the lotus-recent layer.

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

(defun lotus-recent/post-init-recentf ()
  (use-package recentf
    :defer t
    :config
    (progn
      (setq
       recentf-save-file (lotus-cache-file "recentf/recentf")
       recentf-exclude (list (regexp-opt '(".org$" ".rem$")))
       recentf-max-saved-items 99
       ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2007-07/msg00007.html
       ;; I do not want to clean up especially trampfiles.
       recentf-auto-cleanup 'never

       ;; http://stackoverflow.com/a/2069425/341107
       ;; I had problem with recentf and remote file when the remote host was gone.
       recentf-keep '(file-remote-p file-readable-p)
       ;; May solve your problem (remote file will be kept without testing if they still exists).
       )
      ; Add to the "File" menu a list of recently opened files.
      (if (not running-xemacs)
          ;;displays this menu in a buffer
          (recentf-mode 1))

      (deh-section "Entries for files that were never displayed"
        ;; http://www.emacswiki.org/RecentFiles#toc16
        ;; Entries for files that were never displayed

        ;; If you, for example, use CEDET, the recentf package may be fairly
        ;; useless by default. The problem is that CEDET can scan lots of
        ;; source files and make files in the process of building a tags
        ;; database or managing the build system. The recentf list is then
        ;; saturated with files you haven’t displayed or edited on screen.

        ;; This is fixed in the CVS version of CEDET.

        ;; The following is a workaround.

        (defsubst file-was-visible-p (file)
          "Return non-nil if FILE's buffer exists and has been displayed."
          (let ((buf (find-buffer-visiting file)))
            (if buf
                (let ((display-count (buffer-local-value 'buffer-display-count buf)))
                  (if (> display-count 0) display-count nil)))))

        (defsubst keep-default-and-visible-recentf-p (file)
          "Return non-nil if recentf would, by default, keep FILE, and
FILE has been displayed."
          (if (recentf-keep-default-predicate file)
              (file-was-visible-p file)))

        ;; When a buffer is closed, remove the associated file from the recentf
        ;; list if (1) recentf would have, by default, removed the file, or
        ;; (2) the buffer was never displayed.  This is useful because, for
        ;; example, CEDET opens a lot of files in the background to generate
        ;; its tags database, etc.
        (setq recentf-keep '(keep-default-and-visible-recentf-p)))

      (progn ;; "Undo kill buffer"
        ;; http://www.emacswiki.org/RecentFiles#toc17
        ;; Undo kill buffer

        ;; The Opera web browser has a surprisingly useful feature called the
        ;; “trash can”. After you close a tab you can “undo” the close with
        ;; C-z. As it turns out when I want a tab reopened it’s usually one of
        ;; the last few I closed, so I almost never end up looking in the
        ;; history. I missed this feature in Emacs, so I implemented it on top
        ;; of recentf.

        (defun undo-kill-buffer (arg)
          "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
          (interactive "p")
          (let ((recently-killed-list (copy-sequence recentf-list))
                (buffer-files-list
                 (delq nil (mapcar (lambda (buf)
                                     (when (buffer-file-name buf)
                                       (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
            (mapc
             (lambda (buf-file)
               (setq recently-killed-list
                     (delq buf-file recently-killed-list)))
             buffer-files-list)
            (find-file
             (if arg (nth arg recently-killed-list)
               (car recently-killed-list)))))))))

(defun lotus-recent/init-recentf-buffer ()
  (use-package recentf-buffer
    :defer t
    :config
    (progn
      )))

(defun lotus-recent/init-recentf-ext ()
  (use-package recentf-ext
    :defer t
    :config
    (progn
      )))

;;; packages.el ends here
