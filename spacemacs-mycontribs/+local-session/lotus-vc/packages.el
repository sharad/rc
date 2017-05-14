;;; packages.el --- lotus-vc layer packages file for Spacemacs.
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
;; added to `lotus-vc-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-vc/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-vc/pre-init-PACKAGE' and/or
;;   `lotus-vc/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-vcS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-vc-packages
  '(
    magit
    git-commit
    commit-msg-mode
    )
  "The list of Lisp packages required by the lotus-vc layer.

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

(defun lotus-vc/post-init-magit ()
  (use-package magit
    :defer t
    :config
    (progn
      ;; TODO: try to find instance when magit ask for key and recognize
      ;; it to set advice.
      (defadvice magit-push (before ad-update-ssh-agent-env activate)
        (update-ssh-agent)))))

(defun lotus-vc/post-init-git-commit ()
  (use-package git-commit
    :defer t
    :config
    (progn
      (use-package flyspell
        :defer t
        :config
        (add-hook 'git-commit-mode-hook 'turn-on-flyspell))
      (add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))
      (add-to-list 'auto-mode-alist '("hg-editor-.*$" . commit-msg-mode)))))

(defun lotus-vc/post-init-commit-msg-mode ()
  (use-package commit-msg-mde
      :defer t
      :commands (commit-msg-mode)
      :config
      (progn
        (use-package flyspell
            :defer t
            :config
            (add-hook 'git-commit-mode-hook 'turn-on-flyspell))
        (add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))
        (add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . commit-msg-mode)))))

;;; packages.el ends here
