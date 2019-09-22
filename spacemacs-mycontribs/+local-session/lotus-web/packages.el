;;; packages.el --- lotus-web layer packages file for Spacemacs.
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
;; added to `lotus-web-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-web/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-web/pre-init-PACKAGE' and/or
;;   `lotus-web/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-webS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-web-packages
  '(
    ;; (PACKAGE :location local)
    (newpaste :location local)
    (paste2 :location local)
    (pastie :location local)
    dpaste
    pastebin
    w3m
    (w3m-load :location local)
    (w3m-session :location local)
    (w3m-lnum :location local)
    browse-url
    )
  "The list of Lisp packages required by the lotus-web layer.

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

(defun lotus-web/init-newpaste ()
  (use-package newpaste
    :defer t
    :config
    (progn
      )))

(defun lotus-web/init-paste2 ()
  (use-package paste2
    :defer t
    :config
    (progn
      )))

(defun lotus-web/init-pastie ()
  (use-package pastie
    :defer t
    :config
    (progn
      )))

(defun lotus-web/init-dpaste ()
  (use-package dpaste
    :defer t
    :config
    (progn
      )))

(defun lotus-web/init-pastebin ()
  (use-package pastebin
    :defer t
    :config
    (progn
      )))

(defun lotus-web/init-w3m ()
  (use-package w3m
    :defer t
    :config
    (progn
      (setq
       w3m-use-cookies t
       w3m-key-binding 'info
       w3m-home-page "http://www.google.com"
       w3m-use-cookies t)))

  (use-package w3m-util
    :defer t
    :commands (w3m-add-w3m-initial-frames)
    :config
    (progn
      (setq
       w3m-use-cookies t
       w3m-key-binding 'info
       w3m-home-page "http://www.google.com"
       w3m-use-cookies t))))

(defun lotus-web/init-w3m-load ()
  (use-package w3m-load
    :defer t
    :config
    (progn
      (defun jao-w3m-go-to-linknum ()
        "Turn on link numbers and ask for one to go to."
        (interactive)
        (let ((active w3m-link-numbering-mode))
          (when (not active) (w3m-link-numbering-mode))
          (unwind-protect
              (w3m-move-numbered-anchor (read-number "Anchor number: "))
            (when (not active) (w3m-link-numbering-mode)))))

      (defun local-w3m-mode-hook ()
        (define-key w3m-mode-map "f" 'jao-w3m-go-to-linknum)
        (define-key w3m-mode-map (kbd "C-x b") nil))

      (add-hook 'w3m-mode-hook 'local-w3m-mode-hook)

      ;; http://www.emacswiki.org/emacs/emacs-w3m
      (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
      ;; optional keyboard short-cut
      ;; (global-set-key-if-unbind "\C-xm" 'browse-url-at-point)
      )))

(defun lotus-web/init-w3m-session ()
  (use-package w3m-session
    :defer t
    :config
    (progn
      (setq
       w3m-session-file "~/.emacs.d/w3m-session"
       w3m-session-save-always nil
       w3m-session-load-always nil
       w3m-session-show-titles t
       w3m-session-duplicate-tabs 'ask))

    (progn

      ;; WThreeMKeymap

      ;; I found the default keymap somewhat confusing. The point of doing
      ;; web browsing in emacs is to use familiar keys for navigation. So,
      ;; here’s the keymap I use that feels more “emacs-like” for
      ;; me. Sweeten to taste.

      ;; (let ((map (make-keymap)))
      ;;   (suppress-keymap map)
      ;;   (define-key map [backspace] 'w3m-scroll-down-or-previous-url)
      ;;   (define-key map [delete] 'w3m-scroll-down-or-previous-url)
      ;;   (define-key map "\C-?" 'w3m-scroll-down-or-previous-url)
      ;;   (define-key map "\t" 'w3m-next-anchor)
      ;;   (define-key map [(shift tab)] 'w3m-previous-anchor)
      ;;   (define-key map [(shift iso-left-tab)] 'w3m-previous-anchor)
      ;;   (define-key map "\C-m" 'w3m-view-this-url)
      ;;   (define-key map [(shift return)] 'w3m-view-this-url-new-session)
      ;;   (define-key map [(shift kp-enter)] 'w3m-view-this-url-new-session)
      ;;   (define-key map [(button2)] 'w3m-mouse-view-this-url)
      ;;   (define-key map [(shift button2)] 'w3m-mouse-view-this-url-new-session)
      ;;   (define-key map " " 'scroll-up)
      ;;   (define-key map "a" 'w3m-bookmark-add-current-url)
      ;;   (define-key map "\M-a" 'w3m-bookmark-add-this-url)
      ;;   (define-key map "+" 'w3m-antenna-add-current-url)
      ;;   (define-key map "A" 'w3m-antenna)
      ;;   (define-key map "c" 'w3m-print-this-url)
      ;;   (define-key map "C" 'w3m-print-current-url)
      ;;   (define-key map "d" 'w3m-download)
      ;;   (define-key map "D" 'w3m-download-this-url)
      ;;   ;; (define-key map "D" 'w3m-download-with-wget)
      ;;   ;; (define-key map "D" 'w3m-download-with-curl)
      ;;   (define-key map "g" 'w3m-goto-url)
      ;;   (define-key map "G" 'w3m-goto-url-new-session)
      ;;   (define-key map "h" 'describe-mode)
      ;;   (define-key map "H" 'w3m-gohome)
      ;;   (define-key map "I" 'w3m-toggle-inline-images)
      ;;   (define-key map "\M-i" 'w3m-save-image)
      ;;   (define-key map "M" 'w3m-view-url-with-external-browser)
      ;;   (define-key map "n" 'w3m-view-next-page)
      ;;   (define-key map "N" 'w3m-namazu)
      ;;   (define-key map "o" 'w3m-history)
      ;;   (define-key map "O" 'w3m-db-history)
      ;;   (define-key map "p" 'w3m-view-previous-page)
      ;;   (define-key map "q" 'w3m-close-window)
      ;;   (define-key map "Q" 'w3m-quit)
      ;;   (define-key map "R" 'w3m-reload-this-page)
      ;;   (define-key map "s" 'w3m-search)
      ;;   (define-key map "S" (lambda ()
      ;;        		  (interactive)
      ;;        		  (let ((current-prefix-arg t))
      ;;        		    (call-interactively 'w3m-search))))
      ;;   (define-key map "u" 'w3m-view-parent-page)
      ;;   (define-key map "v" 'w3m-bookmark-view)
      ;;   (define-key map "W" 'w3m-weather)
      ;;   (define-key map "=" 'w3m-view-header)
      ;;   (define-key map "\\" 'w3m-view-source)
      ;;   (define-key map "?" 'describe-mode)
      ;;   (define-key map ">" 'scroll-left)
      ;;   (define-key map "<" 'scroll-right)
      ;;   (define-key map "." 'beginning-of-buffer)
      ;;   (define-key map "^" 'w3m-view-parent-page)
      ;;   (define-key map "]" 'w3m-next-form)
      ;;   (define-key map "[" 'w3m-previous-form)
      ;;   (define-key map "}" 'w3m-next-image)
      ;;   (define-key map "{" 'w3m-previous-image)
      ;;   (define-key map "\C-c\C-c" 'w3m-submit-form)
      ;;   (setq dka-w3m-map map))

      ;; (add-hook 'w3m-mode-hook '(lambda () (use-local-map dka-w3m-map)))
      )))

(defun lotus-web/init-w3m-lnum ()
  (use-package w3m-lnum
    :defer t
    :config
    (progn
      )))

(defun lotus-web/init-browse-url ()
  (use-package browse-url
    :defer t
    :config
    (progn
      (setq
       browse-url-browser-function 'w3m-browse-url
       browse-url-new-window-flag t)

      (progn
        (defun browse-url-conkeror (url &optional new-window)
          "Ask Mozilla/Netscape to load URL via the GNOME program `gnome-moz-remote'.
Default to the URL around or before point.  The strings in variable
`browse-url-gnome-moz-arguments' are also passed.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new browser window, otherwise use an
existing one.  A non-nil interactive prefix argument reverses the
effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
          (interactive (browse-url-interactive-arg "URL: "))
          (apply 'start-process (concat "conkeror -Po " url)
                 nil
                 browse-url-conkeror-program
                 (append
                  browse-url-gnome-moz-arguments
                  (if (browse-url-maybe-new-window new-window)
                      '("--newwin"))
                  (list "--raise" url)))))

      (progn
        ;; w3m browse url netscape conkeror emacs gnus
        ;; browse-url-browser-function

        ;; To invoke different browsers for different URLs:
        ;; (setq browse-url-browser-function '(("^mailto:" . browse-url-mail)
        ;; ("." . browse-url-netscape)))

        (setq browse-url-new-window-flag
              ;; Non-nil means always open a new browser window with appropriate browsers
              nil)

        (defun browse-url-conditional (url &optional ns)
          (funcall
           (case (intern (completing-read "what: " '("w3m" "conkeror") nil t))
             (w3m 'w3m-browse-url)
             (conkeror 'browse-url-conkeror))
           url (yes-or-no-p "New Window: ")))


        (setq browse-url-browser-function  'browse-url-conditional)


        ;; (defun widget-button-press (pos &optional event)
        ;;   "Invoke button at POS."
        ;;   (interactive "@d")
        ;;   (let ((button (get-char-property pos 'button)))
        ;;     (if button
        ;; 	(widget-apply-action button event)
        ;;       (let ((command (lookup-key widget-global-map (this-command-keys))))
        ;; 	(when (commandp command)
        ;; 	  (call-interactively command))))))
        )

      (progn
        ;; GNOME means of invoking either Mozilla or Netrape.
        (defvar browse-url-conkeror-program "conkeror")
        (defcustom browse-url-conkeror-arguments '( "-Po")
          "A list of strings passed to the GNOME mozilla viewer as arguments."
          :version "21.1"
          :type '(repeat (string :tag "Argument"))
          :group 'browse-url)

        (defun browse-url-default-browser (url &rest args)
          "Find a suitable browser and ask it to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window, if possible, otherwise use
a random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'.

The order attempted is gnome-moz-remote, Mozilla, Firefox,
Galeon, Konqueror, Netscape, Mosaic, Lynx in an xterm, and then W3."
          (apply
           (cond
            ((executable-find browse-url-conkeror-program) 'browse-url-conkeror)
            ((executable-find browse-url-gnome-moz-program) 'browse-url-gnome-moz)
            ((executable-find browse-url-mozilla-program) 'browse-url-mozilla)
            ((executable-find browse-url-firefox-program) 'browse-url-firefox)
            ((executable-find browse-url-galeon-program) 'browse-url-galeon)
            ((executable-find browse-url-kde-program) 'browse-url-kde)
            ((executable-find browse-url-netscape-program) 'browse-url-netscape)
            ((executable-find browse-url-mosaic-program) 'browse-url-mosaic)
            ((executable-find browse-url-xterm-program) 'browse-url-text-xterm)
            ((locate-library "w3") 'browse-url-w3)
            (t
             (lambda (&rest ignore) (error "No usable browser found"))))
           url args))

        ))))

;; (defun lotus-web/init-PACKAGE ()
;;   (use-package PACKAGE
;;     :defer t
;;     :config
;;     (progn
;;       )))

;;; packages.el ends here
