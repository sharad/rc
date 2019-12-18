;;; packages.el --- lotus-python layer packages file for Spacemacs.
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
;; added to `lotus-python-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-python/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-python/pre-init-PACKAGE' and/or
;;   `lotus-python/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-pythonS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-python-packages
  '(
    ;; (PACKAGE :location local)
    python
    (pymacs :location local)
    )
  "The list of Lisp packages required by the lotus-python layer.

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

(defun lotus-python/post-init-python ()
  (use-package python
    :defer t
    :config
    (progn
      ;; If you are a programmer that works with the latest programming
      ;; languages, there will come a time when even Emacs does not support
      ;; the language out of the box. You might get a library from the
      ;; Internet or from a friend, however, which provides a new major mode
      ;; to deal with the language.

      ;; You know how to let Emacs know where to find the file. See the
      ;; section on where to put your elisp files for more information. Here
      ;; is what you need to do in order to use python-mode for all your .py
      ;; files in addition to getting the file python-mode.el and copying it
      ;; into a directory in your load-path.

      ;; ;; Python
      ;; (autoload 'python-mode "python-mode" "Python editing mode." t)
      ;; (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
      ;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

      ;; This does three things:

      ;;    1. If the function python-mode is called, Emacs will load the
      ;;    file python-mode.elc or python-mode.el in your load-path.

      ;;    2. The variable auto-mode-alist associates filename patterns
      ;;    with functions to call. The entry we are adding lets Emacs call
      ;;    the function python-mode whenever a file ending in .py is
      ;;    edited.

      ;;    3. The variable interpreter-mode-alist associates interpreters
      ;;    with functions to call. The interpreter is guessed from the
      ;;    first line of a file if it starts with #!. The entry we are
      ;;    adding lets Emacs call the function python-mode whenever a file
      ;;    for the python interpreter is edited.

      (progn ;; python-mode
        (progn
          (unless (and (boundp 'py-mode-map)
                       py-mode-map)
            (setq py-mode-map (make-sparse-keymap)))

          ;; from:
          ;; (autoload 'python-mode "python-mode" "Python Mode." t)
          (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
          (add-to-list 'interpreter-mode-alist '("python" . python-mode))

          (add-hook 'python-mode-hook
                    (lambda ()
                      (set-variable 'py-indent-offset 4)
                                        ;(set-variable 'py-smart-indentation nil)
                      (set-variable 'indent-tabs-mode nil)
                      (define-key py-mode-map (kbd "RET") 'newline-and-indent)
                                        ;(define-key py-mode-map [tab] 'yas/expand)
                                        ;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
                      (when (and (featurep 'smart-operator)
                                 (functionp 'smart-operator-mode-on))
                        (smart-operator-mode-on))))

          (global-unset-key (kbd "C-x pn")))

        (use-package auto-complete
          :defer t
          :config
          (progn
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;; Auto-completion
            ;;;  Integrates:
            ;;;   1) Rope
            ;;;   2) Yasnippet
            ;;;   all with AutoComplete.el
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            (defun prefix-list-elements (list prefix)
              (let (value)
                (nreverse
                 (dolist (element list value)
                   (setq value (cons (format "%s%s" prefix element) value))))))

            (defvar ac-source-rope
              '((candidates
                 . (lambda ()
                     (prefix-list-elements (rope-completions) ac-target))))
              "Source for Rope")

            (defun ac-python-find ()
              "Python `ac-find-function'."
              (require 'thingatpt)
              (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
                (if (null symbol)
                    (if (string= "." (buffer-substring (- (point) 1) (point)))
                        (point)
                      nil)
                  symbol)))

            (defun ac-python-candidate ()
              "Python `ac-candidates-function'"
              (let (candidates)
                (dolist (source ac-sources)
                  (if (symbolp source)
                      (setq source (symbol-value source)))
                  (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
                         (requires (cdr-safe (assq 'requires source)))
                         cand)
                    (if (or (null requires)
                            (>= (length ac-target) requires))
                        (setq cand
                              (delq nil
                                    (mapcar (lambda (candidate)
                                              (propertize candidate 'source source))
                                            (funcall (cdr (assq 'candidates source)))))))
                    (if (and (> ac-limit 1)
                             (> (length cand) ac-limit))
                        (setcdr (nthcdr (1- ac-limit) cand) nil))
                    (setq candidates (append candidates cand))))
                (delete-dups candidates)))

            (add-hook 'python-mode-hook
                      (lambda ()
                        (when (xrequire 'auto-complete)
                          (auto-complete-mode 1))
                        (set (make-local-variable 'ac-sources)
                             (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
                        (set (make-local-variable 'ac-find-function) 'ac-python-find)
                        (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
                        (set (make-local-variable 'ac-auto-start) nil)))
            ;;Ryan's python specific tab completion
            (defun ryan-python-tab ()
                                        ; Try the following:
                                        ; 1) Do a yasnippet expansion
                                        ; 2) Do a Rope code completion
                                        ; 3) Do an indent
              (interactive)
              (if (eql (ac-start) 0)
                  (indent-for-tab-command)))
            (defadvice ac-start (before advice-turn-on-auto-start activate)
              (set (make-local-variable 'ac-auto-start) t))
            (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
              (set (make-local-variable 'ac-auto-start) nil))
            (define-key py-mode-map "\t" 'ryan-python-tab)
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;; End Auto Completion
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ))

        ;; Auto Syntax Error Hightlight
        (use-package flymake
          :defer t
          :config
          (progn
            (defun flymake-pyflakes-init ()
              (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                 'flymake-create-temp-inplace))
                     (local-file (file-relative-name
                                  temp-file
                                  (file-name-directory buffer-file-name))))
                (list "pyflakes" (list local-file))))
            (add-to-list 'flymake-allowed-file-name-masks
                         '("\\.py\\'" flymake-pyflakes-init))))

        (use-package info-look
          :defer t
          :config
          (progn
            ;; http://www.emacswiki.org/emacs/?action=browse;oldid=PythonMode;id=PythonProgrammingInEmacs#toc5
            (info-lookup-add-help
             :mode 'python-mode
             :regexp "[[:alnum:]_]+"
             :doc-spec
             '(("(python)Index" nil ""))))))
      )))

(defun lotus-python/init-pymacs ()
  (use-package pymacs
    :defer t
    :config
    (progn
      (progn
        ;; pymacs
        (autoload 'pymacs-apply "pymacs")
        (autoload 'pymacs-call "pymacs")
        (autoload 'pymacs-eval "pymacs" nil t)
        (autoload 'pymacs-exec "pymacs" nil t)
        (autoload 'pymacs-load "pymacs" nil t)
        ;;(eval-after-load "pymacs"
        ;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
        (pymacs-load "ropemacs" "rope-")
        (setq ropemacs-enable-autoimport t)

        (defun pymacs-terminate-services-force ()
          ;; This function is mainly provided for documentation purposes.
          (garbage-collect)
          (pymacs-garbage-collect)
          (when (not pymacs-used-ids)
            (cond ((boundp 'post-gc-hook)
                   (remove-hook 'post-gc-hook 'pymacs-schedule-gc))
                  ((timerp pymacs-gc-timer)
                   (cancel-timer pymacs-gc-timer)))
            (when pymacs-transit-buffer
              (kill-buffer pymacs-transit-buffer))
            (setq pymacs-gc-running nil
                  pymacs-gc-timer nil
                  pymacs-transit-buffer nil
                  pymacs-lisp nil
                  pymacs-freed-list nil)))))))

;;; packages.el ends here
