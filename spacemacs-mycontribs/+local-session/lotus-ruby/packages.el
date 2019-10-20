;;; packages.el --- lotus-ruby layer packages file for Spacemacs.
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
;; added to `lotus-ruby-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-ruby/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-ruby/pre-init-PACKAGE' and/or
;;   `lotus-ruby/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-rubyS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-ruby-packages
  '(
    ;; (PACKAGE :location local)
    ruby-mode)
    
  "The list of Lisp packages required by the lotus-ruby layer.

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

(defun lotus-ruby/post-init-ruby-mode ()
  (use-package ruby-mode
    :defer t
    :config
    (progn
      ;; from http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-talk/114878
      (progn ;; ruby-mode

        ;;; Execute Ruby Command with f3
        (define-key ruby-mode-map [f3] 'ruby-load-file)

        (defun ruby-xmp-region (reg-start reg-end)
          "Pipe the region through Ruby's xmp utility and replace the region
with the result."
          (interactive "r")
          (shell-command-on-region reg-start reg-end
                                   "ruby -r xmp -n -e 'xmp($_, \"%l\t\t# %r\n\")'" t))

        (global-set-key [(meta f10)] 'ruby-xmp-region)


        (defun ruby-custom-setup ()
          (add-to-list 'hs-special-modes-alist
                       '(ruby-mode
                         "\\(def\\|do\\)"
                         "end"
                         "#"
                         (lambda (arg) (ruby-end-of-block))
                         nil))
                         
          (hs-minor-mode t))

        (add-hook 'ruby-mode-hook
                  '(lambda ()
                     (define-key ruby-mode-map "\M-q" 'jw-rb-fill-comment-region)))

        (add-hook 'ruby-mode-hook 'ruby-custom-setup)

        ; Chmod of scripts to u+x
        (add-hook 'after-save-hook
                  '(lambda ()
                     (progn
                       (and (save-excursion
                              (save-restriction
                                (widen)
                                (goto-char (point-min))
                                (save-match-data
                                  (looking-at "^#!"))))
                            (shell-command (concat "chmod u+x " buffer-file-name))
                            (message (concat "Saved as script: " buffer-file-name))))))
                            

        ;JeffreyRadcliffe -- Here is a method which calls the interpreter on the entire buffer, putting the output in another window:
        (defun ruby-eval-buffer () (interactive)
               "Evaluate the buffer with ruby."
               (shell-command-on-region (point-min) (point-max) "ruby"))

        (defun ruby-xmp-region (reg-start reg-end)
          "Pipe the region through Ruby's xmp utility and replace
      the region with the result."
          ;; PragDave -- This li'l beaut runs Gotoken's xmp processor on a region.
          ;; This adds the results of evaluating each line to the end of that line.
          ;; It's how I add values to code examples in my e-mail. Here it's bound to
          ;; M-F10
          (interactive "r")
          (shell-command-on-region reg-start reg-end
                                   "ruby -r xmp -n -e 'xmp($_, \"%l\t\t# %r\n\")'" t))

        (global-set-key [(control f10)] 'ruby-xmp-region)

        (global-set-key [dead-tilde] "~")

        (setq ri-ruby-script "~/.xemacs/packages/ruby/ri-emacs.rb")
        ;; (autoload 'ri "/home/bschroed/.xemacs/ri-ruby.el" nil t)
        ;;
        ;;  You may want to bind the ri command to a key.
        ;;  For example to bind it to F1 in ruby-mode:
        ;;
        (deh-require-maybe ri-ruby
          (add-hook 'ruby-mode-hook (lambda () (local-set-key (kbd "<f1>") 'ri))))

        ;; (setq ruby-mode-hook (remove (nth 4 ruby-mode-hook) ruby-mode-hook))

        ;; http://www.emacswiki.org/emacs/FlymakeRuby
        ;; http://github.com/purcell/emacs.d/blob/master/site-lisp/flymake-ruby/flymake-ruby.el
        ;; (require 'flymake-ruby nil nil)


        ;;{{ from: http://stackoverflow.com/questions/1282501/running-irb-in-emacs-via-run-ruby-echos-everything-i-type
        ;; Define Ruby Mode Hook
        (defun my-ruby-mode-hook ()
          (progn
            (setq comint-process-echoes t)
            (turn-on-font-lock)
            (auto-fill-mode)
            (yas/minor-mode)
            (inf-ruby-keys)))

        ;; Register Ruby Mode Hook
        (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)))))
        ;;}}

        ;; (deh-require-maybe rinari
        ;;   t)
        


      

;; (defun lotus-ruby/init-PACKAGE ()
;;   (use-package PACKAGE
;;     :defer t
;;     :config
;;     (progn
;;       )))

;;; packages.el ends here
