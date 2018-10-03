;;; packages.el --- lotus-misc layer packages file for Spacemacs.
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
;; added to `lotus-misc-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-misc/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-misc/pre-init-PACKAGE' and/or
;;   `lotus-misc/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-misc-packages
  '(
    ace-popup-menu
    achievements
    midnight
    )
  "The list of Lisp packages required by the lotus-misc layer.

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

(defun lotus-misc/init-ace-popup-menu ()
  (use-package ace-popup-menu
               :defer t
               :config
               (progn
                 )))


(defun lotus-misc/init-achievements ()
  (use-package achievements
               :defer t
               :config
               (progn)))

(defun lotus-misc/init-midnight ()
  (use-package midnight
               :defer t
               :config
               (progn
                 (progn
                   ;;https://www.emacswiki.org/emacs/CleanBufferList
                   (setq
                    clean-buffer-list-delay-general 1       ;day
                    clean-buffer-list-delay-special (* 3 60 60)) ;hour min sec

                   (dolist (el
                            '("*buffer-selection*"
                              "*Finder*"
                              "*Finder Category*"
                              "*Finder-package*"
                              "*RE-Builder*"
                              "*vc-change-log*"))
                     (add-to-list 'clean-buffer-list-kill-buffer-names el))

                   (dolist (el
                            '("\\`\\*Customize .*\\*\\'"
                              "\\`\\*\\(Wo\\)?Man .*\\*\\'"))
                     (add-to-list 'clean-buffer-list-kill-regexps el))

                   (dolist (el
                            '("*eshell*"
                              "*ielm*"
                              "*mail*"
                              "*w3m*"
                              "*w3m-cache*"))
                     (add-to-list 'clean-buffer-list-kill-never-buffer-names el))

                   (when nil
                     (dolist (el
                              '("\\`\\*tramp/.*\\*\\`"
                                "\\`\\*ftp .*\\*\\`"))
                       (add-to-list 'clean-buffer-list-kill-never-regexps el)))))))

;;; packages.el ends here
