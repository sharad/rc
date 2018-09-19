;;; packages.el --- lotus-interactivity layer packages file for Spacemacs.
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
;; added to `lotus-interactivity-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-interactivity/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-interactivity/pre-init-PACKAGE' and/or
;;   `lotus-interactivity/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-interactivity-packages
  '(
    ;; (PACKAGE :location local)
    frame-utils
    buffer-utils
    ido
    smex
    idomenu
    pointback
    popup-kill-ring
    crm
    swiper
    )
  "The list of Lisp packages required by the lotus-interactivity layer.

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

(defun lotus-interactivity/init-buffer-utils ()
  (use-package buffer-utils
      :demand t
      :config
      (progn
        )))

(defun lotus-interactivity/post-init-frame-utils ()
  (use-package frame-utils
      :demand t
      :config
      (progn
        (defun frame-utils-config ()

          (setq frame-utils-notify 'message-notify)

          (defun make-mail-chat-frame (&optional force)
            (interactive "P")
            (frame-launcher "mail-chat"
                            '("gnus" "erc")
                            (if force
                                #'(lambda (group)
                                    (toggle-ibuffer-group group t))
                                #'toggle-ibuffer-group)))


          (defun make-mail-compose-frame ())

          (with-eval-after-load "sessions-mgr"
            (defadvice frame-launcher (around frame-launcher activate)
              (let ((*frame-session-restore* nil)  ;not need to restore elsession for frames
                    (org-donot-try-to-clock-in t)) ;no clock require to be clocked-in.
                ad-do-it))))

        (frame-utils-config))))

(defun lotus-interactivity/post-init-ido ()
  (use-package ido
      :defer t
      :config
      (progn
        ;;
        ;;
        ;;
        ;; ;; from http://www.emacswiki.org/emacs/InteractivelyDoThings
        ;;
        ;; ;; icicles or ido no iswitch
        ;;
        ;; ;;I find "C-xb RET" to complicated in comparison to how often it is used:
        ;; ;;(define-key global-map [(kbd "C-\r")] 'iswitchb-buffer)
        ;; ; nice way to change buffer. Use C-enter then C-s and C-r then enter to change the buffer.
        ;; ; More help with C-h f iswitchb.
        ;; ; C-c to toggle case-sensitivity
        ;;
        ;; ;;!! commented in favour of ido.
        ;; ;; ( if (xrequire 'iswitchb)
        ;; ;;     (iswitchb-default-keybindings))
        ;;
        ;; ;;in the .emacs

        (progn
          (defun ido-is-ftp-directory (&optional dir)
            (string-match
             (if nil ;; ido-enable-tramp-completion
                 "\\`/[^/:][^/:]+:"  ;; like tramp-file-name-regexp-unified, but doesn't match single drive letters
                 "\\`/[^/:][^/:]+:/")
             (or dir ido-current-directory))))

        (progn ;; ido

          (require 'misc-utils)

          (setq ido-save-directory-list-file (auto-config-file "ido/ido.last"))

            (defun ido-is-ftp-directory (&optional dir)
              (string-match
               (if nil ;; ido-enable-tramp-completion
                   "\\`/[^/:][^/:]+:"  ;; like tramp-file-name-regexp-unified, but doesn't match single drive letters
                   "\\`/[^/:][^/:]+:/")
               (or dir ido-current-directory)))

            ;; (input-pending-p)

            (defun ido-make-merged-file-list (text auto wide)
              (let (res)
                (message "Searching for `%s'...." text)
                (condition-case nil
                    (if (eq t (setq res
                                    (while-no-input
                                      (ido-make-merged-file-list-1 text auto wide))))
                        (setq res 'input-pending-p))
                  (quit
                   (setq res t
                         ido-try-merged-list nil
                         ido-use-merged-list nil)))
                (when (and res (listp res))
                  (setq res (ido-sort-merged-list res auto)))
                (when (and (or ido-rotate-temp ido-rotate-file-list-default)
                           (listp res)
                           (> (length text) 0))
                  (let ((elt (assoc text res)))
                    (when (and elt (not (eq elt (car res))))
                      (setq res (delq elt res))
                      (setq res (cons elt res)))))
                (message nil)
                res))

            (setq
             ido-default-buffer-method 'maybe-frame
             ido-case-fold t)

            (ido-mode t)

            (when nil
              ;; from: http://www.emacswiki.org/emacs/init-ido.el
              (ido-mode t)                                        ;开启ido模式
              (setq ido-enable-flex-matching t)                   ;模糊匹配
              (setq ido-everywhere nil)                           ;禁用ido everyting, 拷贝操作不方便
              (add-hook 'ido-make-file-list-hook 'ido-sort-mtime) ;文件的排序方法
              (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)  ;目录的排序方法
              )


            (when nil ;; "bookmark"
              (with-eval-after-load "bookmark"
                ;; http://www.emacswiki.org/emacs/InteractivelyDoThings#toc10

                ;; (setq enable-recursive-minibuffers nil)
                (define-key ido-file-dir-completion-map [(meta control ?b)] 'ido-goto-bookmark)
                (defun ido-goto-bookmark (bookmark)
                  (interactive
                   (let ((enable-recursive-minibuffers t))
                     (list (bookmark-completing-read "Jump to bookmark"
                                                     bookmark-current-bookmark))))
                  (unless bookmark
                    (error "No bookmark specified"))
                  (let ((filename (bookmark-get-filename bookmark)))
                    (if (file-directory-p filename)
                        (progn
                          (ido-set-current-directory filename)
                          (setq ido-text ""))
                        (progn
                          (ido-set-current-directory (file-name-directory filename))))
                    (setq ido-exit        'refresh
                          ido-text-init   ido-text
                          ido-rotate-temp t)
                    (exit-minibuffer))))))

        (progn
          ;; ido-work-file-list
          ;; ido-work-directory-list
          ;; ido-last-directory-list
          ;; command-history
          ;; (setq ido-last-directory-list (read (current-buffer))
          ;;       ido-work-directory-list (read (current-buffer))
          ;;       ido-work-file-list (read (current-buffer))
          ;;       ido-dir-file-cache (read (current-buffer))
          ;;       ido-unc-hosts-cache (read (current-buffer)))
          ;; (defun ido-load-history (&optional arg)
          ;; (defun ido-wash-history ()

          ;;
          ;;	 ;; Mimicking an invocation of ido followed by some keys
          ;;
          ;;	 ;; I use ERC, and I wanted to bind something to the equivalent of
          ;;	 ;; “C-x b #”: that is, launching ido-switch-buffer and hitting a #
          ;;	 ;; so that ERC channels are given as options. However, it’s tough in
          ;;	 ;; general to write elisp or keyboard macros that run a command and
          ;;	 ;; start entering things into a minibuffer without finishing the
          ;;	 ;; prompt. For this special case, though, this elisp works:
          ;;
          ;;	 ;; (ido-buffer-internal ido-default-buffer-method nil nil nil "#")
          ;;
          ;;	 ;; Miscellaneous Applications of Ido
          ;;	 ;; M-x mode
          ;;
          ;;	 (setq ido-execute-command-cache nil)
          ;;
          ;;	 (defun ido-execute-command ()
          ;;		 (interactive)
          ;;		 (call-interactively
          ;;			(intern
          ;;			 (ido-completing-read
          ;;				"M-x "
          ;;				(progn
          ;;					(unless ido-execute-command-cache
          ;;						(mapatoms (lambda (s)
          ;;												(when (commandp s)
          ;;													(setq ido-execute-command-cache
          ;;																(cons (format "%S" s) ido-execute-command-cache))))))
          ;;					ido-execute-command-cache)))))
          ;;
          ;;	 (add-hook 'ido-setup-hook
          ;;						 (lambda ()
          ;;							 (setq ido-enable-flex-matching t)
          ;;							 (global-set-key "\M-x" 'ido-execute-command)))
          ;;
          ;;	 ;; Invoking Bookmarks From Ido
          ;;
          ;;	 ;; Did you ever want to use bookmarks from within ido? I just did a
          ;;	 ;; little mashup of bookmark and ido code, just M-C-b from your ido
          ;;	 ;; file selection. – AnselmHelbig
          ;;
          ;;	 (setq enable-recursive-minibuffers t)
          ;;	 (define-key ido-file-dir-completion-map [(meta control ?b)] 'ido-goto-bookmark)
          ;;	 (defun ido-goto-bookmark (bookmark)
          ;;		 (interactive
          ;;			(list (bookmark-completing-read "Jump to bookmark"
          ;;																			bookmark-current-bookmark)))
          ;;		 (unless bookmark
          ;;			 (error "No bookmark specified"))
          ;;		 (let ((filename (bookmark-get-filename bookmark)))
          ;;			 (ido-set-current-directory
          ;;				(if (file-directory-p filename)
          ;;						filename
          ;;					(file-name-directory filename)))
          ;;			 (setq ido-exit				 'refresh
          ;;						 ido-text-init	 ido-text
          ;;						 ido-rotate-temp t)
          ;;			 (exit-minibuffer)))
          ;;
          ;;	 ;; If you don’t want to set recursive minibuffers globally, you
          ;;	 ;; could also activate them locally in the above function using a
          ;;	 ;; let declaration.
          ;;
          ;;	 ;; See also InvokeBookmarks.
          ;;	 ;; Complete find-tag using ido
          ;;
          ;;	 (defun my-ido-find-tag ()
          ;;		 "Find a tag using ido"
          ;;		 (interactive)
          ;;		 (tags-completion-table)
          ;;		 (let (tag-names)
          ;;			 (mapc (lambda (x)
          ;;							 (unless (integerp x)
          ;;								 (push (prin1-to-string x t) tag-names)))
          ;;						 tags-completion-table)
          ;;			 (find-tag (ido-completing-read "Tag: " tag-names))))
          ;;
          ;;	 ;; Find files in Tags File
          ;;
          ;;	 ;; From the screencast above:
          ;;
          ;;	 (defun ido-find-file-in-tag-files ()
          ;;		 (interactive)
          ;;		 (save-excursion
          ;;			 (let ((enable-recursive-minibuffers t))
          ;;				 (visit-tags-table-buffer))
          ;;			 (find-file
          ;;				(expand-file-name
          ;;				 (ido-completing-read
          ;;					"Project file: " (tags-table-files) nil t)))))
          ;;
          ;;	 ;; Selects among the files listed in the tags file. Similar to “find file
          ;;	 ;; in project” in TextMate; the tags file defines your project.
          ;;
          ;;	 ;; Icicles command ‘icicle-find-file-in-tag-table’ does this also. See
          ;;	 ;; Icicles - Tags File Projects.	 Ido on steroids (make it to complete
          ;;	 ;; everything)
          ;;
          ;;	 ;; Hocus pocus, abracadabra, presto!
          ;;
          ;;	 (defadvice completing-read
          ;;		 (around foo activate)
          ;;		 (if (boundp 'ido-cur-list)
          ;;				 ad-do-it
          ;;			 (setq ad-return-value
          ;;						 (ido-completing-read
          ;;							prompt
          ;;							(all-completions "" collection predicate)
          ;;							nil require-match initial-input hist def))))
          ;;
          ;;	 ;; That works with everything but subr’s, from which
          ;;	 ;; execute-extended-command is the one that matters (what is binded
          ;;	 ;; to M-x). But we can get what we want from M-x
          ;;
          ;;	 (global-set-key
          ;;		"\M-x"
          ;;		(lambda ()
          ;;			(interactive)
          ;;			(call-interactively
          ;;			 (intern
          ;;				(ido-completing-read
          ;;				 "M-x "
          ;;				 (all-completions "" obarray 'commandp))))))
          ;;
          ;;	 ;; Make Ido complete almost anything (except the stuff where it
          ;;	 ;; shouldn't)
          ;;
          ;;	 ;; This is a refinement of the above. It adds two features:
          ;;
          ;;	 ;; 1. You can force the original completing-read to be used in specific
          ;;	 ;;		 cases by locally binding a variable.
          ;;
          ;;	 ;; 2. If there are no possible completions, the original completing-read
          ;;	 ;;		 will be used, since ido can’t contribute anything in this case.
          ;;
          ;;	 (defvar ido-enable-replace-completing-read t
          ;;		 "If t, use ido-completing-read instead of completing-read if possible.
          ;;
          ;;		 Set it to nil using let in around-advice for functions where the
          ;;		 original completing-read is required.	For example, if a function
          ;;		 foo absolutely must use the original completing-read, define some
          ;;		 advice like this:
          ;;
          ;;		 (defadvice foo (around original-completing-read-only activate)
          ;;			 (let (ido-enable-replace-completing-read) ad-do-it))")
          ;;
          ;;	 ;; Replace completing-read wherever possible, unless directed otherwise
          ;;
          ;;	 (defadvice completing-read
          ;;		 (around use-ido-when-possible activate)
          ;;		 (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          ;;						 (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
          ;;				 ad-do-it
          ;;			 (let ((allcomp (all-completions "" collection predicate)))
          ;;				 (if allcomp
          ;;						 (setq ad-return-value
          ;;									 (ido-completing-read prompt
          ;;																				allcomp
          ;;																				nil require-match initial-input hist def))
          ;;					 ad-do-it))))
          ;;
          ;;	 ;; Ido Hacks (modifying Ido's behavior) Display Completions Vertically
          ;;
          ;;	 ;; It’s a lot easier to scan long path names if they’re displayed
          ;;	 ;; vertically, instead of horizontally. Run this to achieve just that:
          ;;
          ;;	 ;; (setq ido-decorations
          ;;	 ;;				(quote ("\n-> " "" "\n	 " "\n	 ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
          ;;
          ;;	 ;; – timcharper Control-TAB buffer switching with Ido
          ;;
          ;;	 ;; nXhtml tweaks Ido to do ControlTABbufferCycling combined with Ido’s
          ;;	 ;; normal buffer switching.	Sort files by mtime
          ;;
          ;;	 ;; Why would anyone want an alphabetically sorted list? You can save
          ;;	 ;; keystrokes if the most recently modified files are at the front:
          ;;
          ;;																				 ; sort ido filelist by mtime instead of alphabetically
          ;;	 (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
          ;;	 (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
          ;;	 (defun ido-sort-mtime ()
          ;;		 (setq ido-temp-list
          ;;					 (sort ido-temp-list
          ;;								 (lambda (a b)
          ;;									 (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
          ;;												 (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
          ;;										 (if (= (nth 0 ta) (nth 0 tb))
          ;;												 (> (nth 1 ta) (nth 1 tb))
          ;;											 (> (nth 0 ta) (nth 0 tb)))))))
          ;;		 (ido-to-end	;; move . files to end (again)
          ;;			(delq nil (mapcar
          ;;								 (lambda (x) (if (string-equal (substring x 0 1) ".") x))
          ;;								 ido-temp-list))))
          ;;
          ;;	 ;; If you want to ensure ‘.’ is not buried by this, change the final
          ;;	 ;; lambda as follows (or equivalent:
          ;;
          ;;	 (lambda (x) (if (and (not (string-equal x ".")) (string-equal (substring x 0 1) ".")) x))
          ;;
          ;;	 ;; Depending on the type of entities (e.g. file names) in the list
          ;;	 ;; and your current context, it can often be more convenient to sort
          ;;	 ;; alphabetically. It all depends. For files and directories, this
          ;;	 ;; is why we have different sort orders in DiredMode (see, e.g.,
          ;;	 ;; DiredSortMenu).
          ;;
          ;;	 ;; See also SortOrder.	 A better (IMHO) ido-edit-input function
          ;;
          ;;	 ;; In order to be more consistent with the normal find-file HCI, to
          ;;	 ;; which I am really really used (and, by the waym with the way
          ;;	 ;; command shells do completion), I changed slighlty the behaviour
          ;;	 ;; of the backspace and C-e keys in ‘file mode :
          ;;
          ;;	 (defun ido-my-edit-input () "bla" (interactive)
          ;;		 (setq ido-current-directory
          ;;					 (concat (abbreviate-file-name ido-current-directory) ido-text ))
          ;;		 (setq ido-text "")
          ;;		 (ido-edit-input)
          ;;		 )
          ;;
          ;;	 (defun ido-my-keys ()
          ;;		 "Add my keybindings for ido."
          ;;		 (when (eq ido-cur-item 'file)
          ;;			 (define-key ido-mode-map (kbd "ESC DEL") 'ido-delete-backward-updir)
          ;;			 (define-key ido-mode-map (kbd "C-e") 'ido-my-edit-input)
          ;;			 (define-key ido-mode-map (kbd "<backspace>") 'ido-my-edit-input)
          ;;			 ))
          ;;
          ;;	 ;; Maybe this is useless with recent versions of emacs/ido, but here
          ;;	 ;; I’m forced to use emacs 21, so I downloaded ido 1.56 from
          ;;	 ;; cua.dk (see above) and it works like a charm. My only difficulty
          ;;	 ;; was that I had to comment this line in ido-read-internal, and i
          ;;	 ;; don’t really know what kind of wizardry I am trying to cheat
          ;;	 ;; here.
          ;;
          ;;	 ;;(process-environment (cons "HOME=/" process-environment))
          ;;
          ;; )
          ;;
          )

        (progn ;; "Completing Read Multiple"
          (defun ido-completing-read-multiple (prompt choices &optional predicate require-match initial-input hist def sentinel)
            "Read multiple items with ido-completing-read. Reading stops
  when the user enters SENTINEL. By default, SENTINEL is
  \"*done*\". SENTINEL is disambiguated with clashing completions
  by appending _ to SENTINEL until it becomes unique. So if there
  are multiple values that look like SENTINEL, the one with the
  most _ at the end is the actual sentinel value. See
  documentation for `ido-completing-read' for details on the
  other parameters."
            (let
                ((sentinel (if sentinel sentinel "*done*"))
                 (done-reading nil)
                 (res ()))

              ;; uniquify the SENTINEL value
              (while (find sentinel choices)
                (setq sentinel (concat sentinel "_")))
              (setq choices (cons sentinel choices))

              ;; read some choices
              (while (not done-reading)
                (setq this-choice (ido-completing-read prompt choices predicate require-match initial-input hist def))
                (if (equal this-choice sentinel)
                    (setq done-reading t)
                    (setq res (cons this-choice res))))

              ;; return the result
              res))
          )

        (progn


          (require 'misc-utils)

          ))))

(defun lotus-interactivity/post-init-smex ()
  (use-package smex
      :defer t
      :config
      (progn
        (setq smex-save-file (expand-file-name "~/.emacs.d/.smex-items"))
        (smex-initialize))))

(defun lotus-interactivity/init-idomenu ()
  (use-package idomenu
      :defer t
      :config
      (progn
        )))

(defun lotus-interactivity/init-pointback ()
  (use-package pointback
      :defer t
      :config
      (progn
        ;; http://stud4.tuwien.ac.at/~e0225855/pointback/pointback.html
        ;; http://stud4.tuwien.ac.at/~e0225855/pointback/pointback.el

        ;; When you have two windows X and Y showing different sections of the
        ;; same buffer B, then open a different buffer in X, and then show B
        ;; in X again, the new point in X will be the same as in Y. With
        ;; pointback-mode, window points are preserved instead, and point will
        ;; be where it originally was in X for B when you return to B.

        (pointback-mode))))

(defun lotus-interactivity/init-popup-kill-ring ()
  (use-package popup-kill-ring
      :defer t
      :config
      (progn
        ;; http://emacswiki.org/emacs/khiker
        )))

(defun lotus-interactivity/init-crm ()
  (use-package crm
      :defer t
      :config
      (progn
        ;; ;; testing and debugging
        ;; (defun crm-init-test-environ ()
        ;;   "Set up some variables for testing."
        ;;   (interactive)
        ;;   (setq my-prompt "Prompt: ")
        ;;   (setq my-table
        ;; 	'(("hi") ("there") ("man") ("may") ("mouth") ("ma")
        ;; 	  ("a") ("ab") ("abc") ("abd") ("abf") ("zab") ("acb")
        ;; 	  ("da") ("dab") ("dabc") ("dabd") ("dabf") ("dzab") ("dacb")
        ;; 	  ("fda") ("fdab") ("fdabc") ("fdabd") ("fdabf") ("fdzab") ("fdacb")
        ;; 	  ("gda") ("gdab") ("gdabc") ("gdabd") ("gdabf") ("gdzab") ("gdacb")
        ;; 	  ))
        ;;   (setq my-separator ","))

        ;; (completing-read-multiple my-prompt my-table)
        ;; (completing-read-multiple my-prompt my-table nil t)
        ;; (completing-read-multiple my-prompt my-table nil "match")
        ;; (completing-read my-prompt my-table nil t)
        ;; (completing-read my-prompt my-table nil "match")
        )))

(defun lotus-interactivity/post-init-swiper ()
  ;; https://www.emacswiki.org/emacs/UsePackage
  ;; http://oremacs.com/2015/04/16/ivy-mode/
  ;; http://irreal.org/blog/
  (use-package swiper
      :ensure t
      :config
      (progn
        (progn
          (ivy-mode 1)
          (setq ivy-use-virtual-buffers t))) ))

(defun lotus-interactivity/init-counsel ()
  (use-package counsel
      :ensure t
      :config
      (progn
        )))

;;; packages.el ends here
