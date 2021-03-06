;;;;
;;;;
;;;;
;;;; ;; from http://www.emacswiki.org/emacs/InteractivelyDoThings
;;;;
;;;; ;; icicles or ido no iswitch
;;;;
;;;; ;;I find "C-xb RET" to complicated in comparison to how often it is used:
;;;; ;;(define-key global-map [(kbd "C-\r")] 'iswitchb-buffer)
;;;; ; nice way to change buffer. Use C-enter then C-s and C-r then enter to change the buffer.
;;;; ; More help with C-h f iswitchb.
;;;; ; C-c to toggle case-sensitivity
;;;;
;;;; ;;!! commented in favour of ido.
;;;; ;; ( if (xrequire 'iswitchb)
;;;; ;;     (iswitchb-default-keybindings))
;;;;
;;;; ;;in the .emacs



(require 'init-config "~/.xemacs/init-config.el")

;; (load-file "~/ido.elc")
(eval-after-load "ido"
  '(defun ido-is-ftp-directory (&optional dir)
    (string-match
     (if nil ;; ido-enable-tramp-completion
         "\\`/[^/:][^/:]+:"  ;; like tramp-file-name-regexp-unified, but doesn't match single drive letters
         "\\`/[^/:][^/:]+:/")
     (or dir ido-current-directory))))

;; (when t
(deh-require-maybe ido

  (setq ido-save-directory-list-file (auto-config-file "ido/ido.last"))

  (defun ido-is-ftp-directory (&optional dir)
    (string-match
     (if nil ;; ido-enable-tramp-completion
         "\\`/[^/:][^/:]+:"  ;; like tramp-file-name-regexp-unified, but doesn't match single drive letters
         "\\`/[^/:][^/:]+:/")
     (or dir ido-current-directory)))

  ;; (input-pending-p)

  '(defun ido-make-merged-file-list (text auto wide)
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

  (setq ido-default-buffer-method 'maybe-frame
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

  (deh-require-maybe idomenu
    ;; http://emacswiki.org/emacs/ImenuMode
    )




  (deh-section "bookmark"
    ;; http://www.emacswiki.org/emacs/InteractivelyDoThings#toc10
    (require 'bookmark)
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
        (exit-minibuffer)))))

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

;;;;
;;;;   ;; Mimicking an invocation of ido followed by some keys
;;;;
;;;;   ;; I use ERC, and I wanted to bind something to the equivalent of
;;;;   ;; “C-x b #”: that is, launching ido-switch-buffer and hitting a #
;;;;   ;; so that ERC channels are given as options. However, it’s tough in
;;;;   ;; general to write elisp or keyboard macros that run a command and
;;;;   ;; start entering things into a minibuffer without finishing the
;;;;   ;; prompt. For this special case, though, this elisp works:
;;;;
;;;;   ;; (ido-buffer-internal ido-default-buffer-method nil nil nil "#")
;;;;
;;;;   ;; Miscellaneous Applications of Ido
;;;;   ;; M-x mode
;;;;
;;;;   (setq ido-execute-command-cache nil)
;;;;
;;;;   (defun ido-execute-command ()
;;;;     (interactive)
;;;;     (call-interactively
;;;;      (intern
;;;;       (ido-completing-read
;;;;        "M-x "
;;;;        (progn
;;;;          (unless ido-execute-command-cache
;;;;            (mapatoms (lambda (s)
;;;;                        (when (commandp s)
;;;;                          (setq ido-execute-command-cache
;;;;                                (cons (format "%S" s) ido-execute-command-cache))))))
;;;;          ido-execute-command-cache)))))
;;;;
;;;;   (add-hook 'ido-setup-hook
;;;;             (lambda ()
;;;;               (setq ido-enable-flex-matching t)
;;;;               (global-set-key "\M-x" 'ido-execute-command)))
;;;;
;;;;   ;; Invoking Bookmarks From Ido
;;;;
;;;;   ;; Did you ever want to use bookmarks from within ido? I just did a
;;;;   ;; little mashup of bookmark and ido code, just M-C-b from your ido
;;;;   ;; file selection. – AnselmHelbig
;;;;
;;;;   (setq enable-recursive-minibuffers t)
;;;;   (define-key ido-file-dir-completion-map [(meta control ?b)] 'ido-goto-bookmark)
;;;;   (defun ido-goto-bookmark (bookmark)
;;;;     (interactive
;;;;      (list (bookmark-completing-read "Jump to bookmark"
;;;;                                      bookmark-current-bookmark)))
;;;;     (unless bookmark
;;;;       (error "No bookmark specified"))
;;;;     (let ((filename (bookmark-get-filename bookmark)))
;;;;       (ido-set-current-directory
;;;;        (if (file-directory-p filename)
;;;;            filename
;;;;          (file-name-directory filename)))
;;;;       (setq ido-exit        'refresh
;;;;             ido-text-init   ido-text
;;;;             ido-rotate-temp t)
;;;;       (exit-minibuffer)))
;;;;
;;;;   ;; If you don’t want to set recursive minibuffers globally, you
;;;;   ;; could also activate them locally in the above function using a
;;;;   ;; let declaration.
;;;;
;;;;   ;; See also InvokeBookmarks.
;;;;   ;; Complete find-tag using ido
;;;;
;;;;   (defun my-ido-find-tag ()
;;;;     "Find a tag using ido"
;;;;     (interactive)
;;;;     (tags-completion-table)
;;;;     (let (tag-names)
;;;;       (mapc (lambda (x)
;;;;               (unless (integerp x)
;;;;                 (push (prin1-to-string x t) tag-names)))
;;;;             tags-completion-table)
;;;;       (find-tag (ido-completing-read "Tag: " tag-names))))
;;;;
;;;;   ;; Find files in Tags File
;;;;
;;;;   ;; From the screencast above:
;;;;
;;;;   (defun ido-find-file-in-tag-files ()
;;;;     (interactive)
;;;;     (save-excursion
;;;;       (let ((enable-recursive-minibuffers t))
;;;;         (visit-tags-table-buffer))
;;;;       (find-file
;;;;        (expand-file-name
;;;;         (ido-completing-read
;;;;          "Project file: " (tags-table-files) nil t)))))
;;;;
;;;;   ;; Selects among the files listed in the tags file. Similar to “find file
;;;;   ;; in project” in TextMate; the tags file defines your project.
;;;;
;;;;   ;; Icicles command ‘icicle-find-file-in-tag-table’ does this also. See
;;;;   ;; Icicles - Tags File Projects.  Ido on steroids (make it to complete
;;;;   ;; everything)
;;;;
;;;;   ;; Hocus pocus, abracadabra, presto!
;;;;
;;;;   (defadvice completing-read
;;;;     (around foo activate)
;;;;     (if (boundp 'ido-cur-list)
;;;;         ad-do-it
;;;;       (setq ad-return-value
;;;;             (ido-completing-read
;;;;              prompt
;;;;              (all-completions "" collection predicate)
;;;;              nil require-match initial-input hist def))))
;;;;
;;;;   ;; That works with everything but subr’s, from which
;;;;   ;; execute-extended-command is the one that matters (what is binded
;;;;   ;; to M-x). But we can get what we want from M-x
;;;;
;;;;   (global-set-key
;;;;    "\M-x"
;;;;    (lambda ()
;;;;      (interactive)
;;;;      (call-interactively
;;;;       (intern
;;;;        (ido-completing-read
;;;;         "M-x "
;;;;         (all-completions "" obarray 'commandp))))))
;;;;
;;;;   ;; Make Ido complete almost anything (except the stuff where it
;;;;   ;; shouldn't)
;;;;
;;;;   ;; This is a refinement of the above. It adds two features:
;;;;
;;;;   ;; 1. You can force the original completing-read to be used in specific
;;;;   ;;    cases by locally binding a variable.
;;;;
;;;;   ;; 2. If there are no possible completions, the original completing-read
;;;;   ;;    will be used, since ido can’t contribute anything in this case.
;;;;
;;;;   (defvar ido-enable-replace-completing-read t
;;;;     "If t, use ido-completing-read instead of completing-read if possible.
;;;;
;;;;     Set it to nil using let in around-advice for functions where the
;;;;     original completing-read is required.  For example, if a function
;;;;     foo absolutely must use the original completing-read, define some
;;;;     advice like this:
;;;;
;;;;     (defadvice foo (around original-completing-read-only activate)
;;;;       (let (ido-enable-replace-completing-read) ad-do-it))")
;;;;
;;;;   ;; Replace completing-read wherever possible, unless directed otherwise
;;;;
;;;;   (defadvice completing-read
;;;;     (around use-ido-when-possible activate)
;;;;     (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
;;;;             (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
;;;;         ad-do-it
;;;;       (let ((allcomp (all-completions "" collection predicate)))
;;;;         (if allcomp
;;;;             (setq ad-return-value
;;;;                   (ido-completing-read prompt
;;;;                                        allcomp
;;;;                                        nil require-match initial-input hist def))
;;;;           ad-do-it))))
;;;;
;;;;   ;; Ido Hacks (modifying Ido's behavior) Display Completions Vertically
;;;;
;;;;   ;; It’s a lot easier to scan long path names if they’re displayed
;;;;   ;; vertically, instead of horizontally. Run this to achieve just that:
;;;;
;;;;   ;; (setq ido-decorations
;;;;   ;;       (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;;;;
;;;;   ;; – timcharper Control-TAB buffer switching with Ido
;;;;
;;;;   ;; nXhtml tweaks Ido to do ControlTABbufferCycling combined with Ido’s
;;;;   ;; normal buffer switching.  Sort files by mtime
;;;;
;;;;   ;; Why would anyone want an alphabetically sorted list? You can save
;;;;   ;; keystrokes if the most recently modified files are at the front:
;;;;
;;;;                                         ; sort ido filelist by mtime instead of alphabetically
;;;;   (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
;;;;   (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
;;;;   (defun ido-sort-mtime ()
;;;;     (setq ido-temp-list
;;;;           (sort ido-temp-list
;;;;                 (lambda (a b)
;;;;                   (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
;;;;                         (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
;;;;                     (if (= (nth 0 ta) (nth 0 tb))
;;;;                         (> (nth 1 ta) (nth 1 tb))
;;;;                       (> (nth 0 ta) (nth 0 tb)))))))
;;;;     (ido-to-end  ;; move . files to end (again)
;;;;      (delq nil (mapcar
;;;;                 (lambda (x) (if (string-equal (substring x 0 1) ".") x))
;;;;                 ido-temp-list))))
;;;;
;;;;   ;; If you want to ensure ‘.’ is not buried by this, change the final
;;;;   ;; lambda as follows (or equivalent:
;;;;
;;;;   (lambda (x) (if (and (not (string-equal x ".")) (string-equal (substring x 0 1) ".")) x))
;;;;
;;;;   ;; Depending on the type of entities (e.g. file names) in the list
;;;;   ;; and your current context, it can often be more convenient to sort
;;;;   ;; alphabetically. It all depends. For files and directories, this
;;;;   ;; is why we have different sort orders in DiredMode (see, e.g.,
;;;;   ;; DiredSortMenu).
;;;;
;;;;   ;; See also SortOrder.  A better (IMHO) ido-edit-input function
;;;;
;;;;   ;; In order to be more consistent with the normal find-file HCI, to
;;;;   ;; which I am really really used (and, by the waym with the way
;;;;   ;; command shells do completion), I changed slighlty the behaviour
;;;;   ;; of the backspace and C-e keys in ‘file mode :
;;;;
;;;;   (defun ido-my-edit-input () "bla" (interactive)
;;;;     (setq ido-current-directory
;;;;           (concat (abbreviate-file-name ido-current-directory) ido-text ))
;;;;     (setq ido-text "")
;;;;     (ido-edit-input)
;;;;     )
;;;;
;;;;   (defun ido-my-keys ()
;;;;     "Add my keybindings for ido."
;;;;     (when (eq ido-cur-item 'file)
;;;;       (define-key ido-mode-map (kbd "ESC DEL") 'ido-delete-backward-updir)
;;;;       (define-key ido-mode-map (kbd "C-e") 'ido-my-edit-input)
;;;;       (define-key ido-mode-map (kbd "<backspace>") 'ido-my-edit-input)
;;;;       ))
;;;;
;;;;   ;; Maybe this is useless with recent versions of emacs/ido, but here
;;;;   ;; I’m forced to use emacs 21, so I downloaded ido 1.56 from
;;;;   ;; cua.dk (see above) and it works like a charm. My only difficulty
;;;;   ;; was that I had to comment this line in ido-read-internal, and i
;;;;   ;; don’t really know what kind of wizardry I am trying to cheat
;;;;   ;; here.
;;;;
;;;;   ;;(process-environment (cons "HOME=/" process-environment))
;;;;
;;;; )
;;;;


(define-key global-map [(kbd "C-xb")] 'ido-switch-buffer)
(global-set-key-if-unbind "\C-xb" 'ido-switch-buffer)


;; http://www.emacswiki.org/emacs/SwitchingBuffers
;; Flipping Buffers in Two Frames
;; In Emacs you can do many things at once in multiple
;; frames (outside Emacs frames are sometimes called “windows”). For
;; instance: If you have multiple screens on your machine you can
;; open individual Emacs frames for each screen. Each frame contains
;; its own buffers and of course each frame can be split into Emacs
;; windows.
;; I wrote the following bit of Emacs lisp in order to switch the
;; contents of two open frames. This is very useful for me at work
;; since I use a two monitor setup. Sometimes I want to edit
;; whatever is on the other monitor on my “main” monitor and
;; vise-versa.
(defun switch-buffers-between-frames ()
  "switch-buffers-between-frames switches the buffers between the two last frames"
  (interactive)
  (let ((this-frame-buffer nil)
	(other-frame-buffer nil))
    (setq this-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (other-frame 1)
    (setq other-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (switch-to-buffer this-frame-buffer)
    (other-frame 1)
    (switch-to-buffer other-frame-buffer)))
;; http://www.emacswiki.org/emacs/SwitchingBuffers
;; Transposing Two Buffers
;; If you have a window split showing two buffers, you can transpose
;; the two buffers:
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;;{{
;; http://stud4.tuwien.ac.at/~e0225855/pointback/pointback.html
;; http://stud4.tuwien.ac.at/~e0225855/pointback/pointback.el

;; When you have two windows X and Y showing different sections of the
;; same buffer B, then open a different buffer in X, and then show B
;; in X again, the new point in X will be the same as in Y. With
;; pointback-mode, window points are preserved instead, and point will
;; be where it originally was in X for B when you return to B.

(deh-require-maybe pointback
  (pointback-mode))
;;}}

;;{{  No need
;; The following function changes the way you switch buffers. You are
;; presented with a list of buffers that shrinks as you type the name,
;; only the matching buffers are shown, making buffer switching much
;; easier.
;; iswitchb
;; (when (functionp 'iswitchb-default-keybindings)
;; ;;  (iswitchb-mode nil)
;; ;;  (iswitchb-default-keybindings))
;; )
;;}}

(eval-after-load "smex"
  '(smex-initialize))

(deh-require-maybe smex
  (setq smex-save-file (expand-file-name "~/.emacs.d/.smex-items")))




(deh-require-maybe popup-kill-ring
  ;; http://emacswiki.org/emacs/khiker
  )



(deh-section "Completing Read Multiple"
  (deh-section "ido"

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


  (deh-require-maybe crm

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


    )

  )


(deh-section "Enable recursive minibuffer"
  (defun status-recursive-minibuffers ()
      (if enable-recursive-minibuffers
        (message "recursive minibuffers enabled")
        (message "recursive minibuffers disabled")))
  (defun enable-recursive-minibuffer ()
    (interactive)
    (setq enable-recursive-minibuffers t)
    (status-recursive-minibuffers))
  (defun disable-recursive-minibuffer ()
    (interactive)
    (setq enable-recursive-minibuffers t)
    (status-recursive-minibuffers))
  (defun toggle-recursive-minibuffer ()
    (interactive)
    (setq enable-recursive-minibuffers (not enable-recursive-minibuffers))
    (status-recursive-minibuffers)))


(deh-section "idle timeout lock"

  (defvar lock-passwd-keymap nil "lock passwd keymap")
  (setq lock-passwd-keymap (make-sparse-keymap))
  (define-key lock-passwd-keymap (kbd "C-j") 'newline)
  (define-key lock-passwd-keymap (kbd "RET") 'exit-minibuffer)
  (define-key lock-passwd-keymap (kbd "p") 'self-insert-command)

  (when nil
   (let ()
    (defun reset-global-map ()
      (interactive)
      (use-global-map global-map)
      (message "Done"))
    (run-at-time "1 min" nil 'reset-global-map)
    (use-global-map lock-passwd-keymap)
    (message (read-from-minibuffer "ABC: " "p" lock-passwd-keymap))
    (use-global-map global-map)))



  (defvar lock-emacs-master-password "abcd123" "lock emacs mster password")
  (defvar lock-emacs-password nil "lock emacs password")
  (defun set-emacs-lock-passwd ()
    (interactive)
    (let ((passwd (read-from-minibuffer "emacs lock passwd: " lock-emacs-password)))
      (setq lock-emacs-password passwd)
      (message "emacs lock password set to: %s" passwd)))

  (defun lock-emacs (&optional timeout)
    (interactive)
    (let ((timeout (or timeout 7)))
      (cond ((= (shell-command "pgrep xtrlock") 0)
             (message "Screen is already locked."))
            ((null lock-emacs-password)
             (message "password is not set, set it using set M-x set-emacs-lock-passwd"))
            (t
             (if (or
               (< (length (frame-list)) 2)
               (ad-find-advice 'call-interactively 'around 'lock))
              (message "Not locking emacs.")
              (progn
                (define-key (current-global-map) [remap self-insert-command] 'ignore)
                (eval
                 `(defadvice call-interactively (around lock activate)
                    ;; ,(help-function-interactive 'fun)
                    (catch 'foo
                      (condition-case e
                          (let ()
                            (define-key (current-global-map) [remap self-insert-command] 'ignore)
                            (when (or (< (length (frame-list)) 2) ;; *minimum-disable-login-session-frames*)
                                      (let ((passwd (with-timeout (,timeout (progn (message nil) (throw 'foo nil)))
                                                     (read-passwd "unlock password: "))))
                                        (or
                                         (string-equal passwd lock-emacs-password)
                                         (string-equal passwd lock-emacs-master-password))))
                              (unwind-protect
                                   ad-do-it
                                (progn
                                  (define-key (current-global-map) [remap self-insert-command] nil)
                                  (when (ad-find-advice 'call-interactively 'around 'lock)
                                    (ad-remove-advice 'call-interactively 'around 'lock)
                                    (ad-activate 'call-interactively)
                                    (ad-update  'call-interactively))))))
                        ('error
                         (progn
                           (message "Error: %s" e)
                           (define-key (current-global-map) [remap self-insert-command] nil)
                           (when (ad-find-advice 'call-interactively 'around 'lock)
                             (ad-remove-advice 'call-interactively 'around 'lock)
                             (ad-activate 'call-interactively)
                             (ad-update  'call-interactively))))))))
                (ad-enable-advice 'call-interactively 'around 'lock)
                (ad-activate 'call-interactively)
                (ad-update 'call-interactively)))))))

  ;; (defalias 'lock-emacs 'set-emacs-lock)

  (defvar zone-timer nil "Zone timer.")

  (defun zone-with-lock ()
    (unless (ad-find-advice 'call-interactively 'around 'lock)
      (lock-emacs 7))
    (zone))

  (defun enable-zone-lock ()
    (interactive)
    (setq zone-timer (run-with-idle-timer 600 t 'zone-with-lock)))

  (defun disable-zone-lock ()
    (interactive)
    (when zone-timer
      (cancel-timer zone-timer)
      (setq zone-timer nil)))

  ;; To enable Zone Mode for al


  ;; minutes, add the following Lisp code to you InitFile or try it out
  ;; by EvaluatingExpressions.

  ;; (require 'zone)
  ;; (zone-when-idle 120)

  ;; This will also enable Zone Mode after 2 minutes of idle.

  ;; (setq zone-timer (run-with-idle-timer 120 t
  ;;                                       '(lambda ()
  ;;                                         (zone)
  ;;                                         (set-emacs-lock))))

  ;; (cancel-timer zone-timer)

  )

(when nil
  (deh-section "swiper ivy etc"
    ;; https://www.emacswiki.org/emacs/UsePackage
    ;; http://oremacs.com/2015/04/16/ivy-mode/
    ;; http://irreal.org/blog/
    (use-package swiper
        :ensure t
        :config
        (ivy-mode 1)
        (setq ivy-use-virtual-buffers t)
        (global-set-key-if-unbind (kbd "C-s") 'swiper)
        (global-set-key-if-unbind (kbd "C-c C-r") 'ivy-resume)
        (global-set-key-if-unbind (kbd "M-x") 'counsel-M-x)
        (global-set-key-if-unbind (kbd "C-x C-f") 'counsel-find-file))
    (use-package counsel
        :ensure t)))

(provide 'interactivity-config)
