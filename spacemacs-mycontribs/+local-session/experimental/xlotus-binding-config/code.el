;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bindings
;; Time-stamp: <2014-02-27 14:45:52 s>
;;

;; (deh-section "Key binding utils"
;; )


;; (require 'utils)

(autoload 'describe-unbound-keys "unbound" "Find Unbound keys" t)

(defun iglobal-set-key-if-unbind (keys cmd &optional force)
  (interactive
   (let* (k
         (keys1 "")
         (keys (loop until (equal (setq k (read-key-sequence (concat "keys: [" keys1 "]"))) "")
                  do (setq keys1 (concat keys1 k))))
         (cmd (read-command "cmd: "))
         (force (or current-prefix-arg nil)))
     (list keys cmd force)))
  (if force
      (global-set-key keys cmd)
      (global-set-key-if-unbind keys cmd)))

(eval-when-compile
 (defvar replacement-map '(("M" . "s")) "default replacement key modifiers.")

  (defun replace-modifier (keys map)
    (dolist (v map keys)
      (setq keys
            (replace-regexp-in-string (concat (car v) "-")
                                      (concat (cdr v) "-") keys t)))))

(defmacro global-set-key-replace (keys cmd &optional rep-map)
  (let ((rep-map (or rep-map replacement-map)))
    `(let ((prev-cmd (global-key-binding (kbd ,keys))))
       (if prev-cmd
           (if (global-set-key (kbd ,(replace-modifier keys rep-map)) prev-cmd)
               (progn
                 (global-set-key (kbd ,keys) ',cmd)))
           (global-set-key (kbd ,keys) ',cmd)))))

(deh-section "Hyper Super etc"
  ;; http://nex-3.com/posts/45-efficient-window-switching-in-emacs#comments
  (defvar real-keyboard-keys
    '(("M-<up>"        . "\M-[1;3A")
      ("M-<down>"      . "\M-[1;3B")
      ("M-<right>"     . "\M-[1;3C")
      ("M-<left>"      . "\M-[1;3D")
      ("C-<return>"    . "\C-j")
      ("C-<delete>"    . "\M-[3;5~")
      ("C-<up>"        . "\M-[1;5A")
      ("C-<down>"      . "\M-[1;5B")
      ("C-<right>"     . "\M-[1;5C")
      ("C-<left>"      . "\M-[1;5D"))
    "An assoc list of pretty key strings
and their terminal equivalents.")

  ;;putty Arrow Keys
  ;;
  (defvar putty-real-keyboard-keys
    '(("M-<up>"        . "\C-[\C-[OA")
      ("M-<down>"      . "\C-[\C-[OB")
      ("M-<right>"     . "\C-[\C-[OC")
      ("M-<left>"      . "\C-[\C-[OD")
      ("C-<up>"        . "\C-[[A")
      ("C-<down>"      . "\C-[[B")
      ("C-<right>"     . "\C-[[C")
      ("C-<left>"      . "\C-[[D"))
    "An assoc list of pretty key strings and their terminal equivalents.")


  (defun key (desc)                     ;????
    (or (and window-system (read-kbd-macro desc))
        (or (cdr (assoc desc real-keyboard-keys))
            (read-kbd-macro desc))))

  ;; (global-set-key (key "M-<left>") 'windmove-left)          ; move to left windnow
  ;; (global-set-key (key "M-<right>") 'windmove-right)        ; move to right window
  ;; (global-set-key (key "M-<up>") 'windmove-up)              ; move to upper window
  ;; (global-set-key (key "M-<down>") 'windmove-down)          ; move to downer window

  )



;; This requires special setup by calling loadkeys(1) from .profile!
;; (global-set-key (kbd "ESC [ M") 'dabbrev-expand)

;; C-c i calls insert-date-string
(global-set-key (kbd "C-c i") 'insert-date-string)

;; ref: http://www.rouquier.org/jb/programmation/.emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal shortcuts
;; (global-set-key "C-tab" 'other-window)
;What's the difference between define-key and global-set-key?


;scroll down so that the line under the cursor comes at the top of the window
(defun recenter-zero () (interactive) (recenter 0))
;;(global-set-key  "â")
;;(unless (featureq 'xemacs)
(if (not running-xemacs)
  (global-set-key-if-unbind [M-up] 'recenter-zero))  ;; note: work under X not in
;;(global-set-key [(kbd "M <up>")] 'recenter-zero)  ;; note: work under X not in
					;; console, C-M-l do this in
					;; both.

;; C-home deletes indentation of the current line, then merges it with the preceeding line.
;; Useful when programming.
;; Default key-binding is M-^
(define-key global-map [(control home)] 'delete-indentation)

(defun rlr-copy-line-as-kill (&optional arg)
  "Copy current line to kill ring. With arg, copies that many lines."
  (interactive "nLines to copy?")
  (save-excursion
    (beginning-of-line)
    (copy-region-as-kill (point)
                         (progn (forward-line arg) (point)))))

(if (not running-xemacs)
    (global-set-key-if-unbind [C-w] 'rlr-copy-line-as-kill) ;use [S-del] to cut the region
  )


(global-set-key (kbd "M-SPC") 'just-one-space)

;; F5 is user by anything-config - resolve it.
;; (global-set-key [f5] 'font-lock-fontify-buffer) ;"reload colors"
(global-set-key (kbd "C-c g") 'goto-line) ;C-x g allows one to reach a line with its number

;;automatically close brackets, quotes, etc when typing
(setq skeleton-pair t)
(setq skeleton-pair-on-word t) ; apply skeleton trick even in front of a word.
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
;; and ensure those bindings are kept for c and c++ modes
(add-hook 'c-mode-hook '(lambda ()
 (define-key c-mode-base-map "{" 'skeleton-pair-insert-maybe)
 (define-key c-mode-base-map "(" 'skeleton-pair-insert-maybe)
))
(add-hook 'c++-mode-hook '(lambda ()
 (define-key c-mode-base-map "{" 'skeleton-pair-insert-maybe)
 (define-key c-mode-base-map "(" 'skeleton-pair-insert-maybe)
))


;; Simplified

;; from http://jasonmbaker.com/7-tools-for-working-with-python-in-emacs-and
(deh-section "Anything Keybindings"
    (when (and (xrequire 'anything-config)
               (xrequire 'anything-match-plugin))
      (global-set-key-if-unbind "\C-ca" 'anything)
      (global-set-key-if-unbind "\C-co" 'anything-for-files)))

(deh-section "Buffer"
  (define-key global-map [?\C-x right] 'next-buffer)
  (define-key global-map [?\C-x left] 'prev-buffer))

(deh-require-maybe follow-mode
  ;; see in buffer.el file
  (global-set-key-if-unbind [f8] 'follow-mode)
  (global-set-key-if-unbind [f7] 'follow-delete-other-windows-and-split)
  ;;
  ;; The hook;  Set better keys.  (Follow Mode is not allowed to
  ;; set keys other than `C-c <punctuation character> <whatever>').
  ;;
  (defun my-follow-mode-hook ()
    (define-key follow-mode-map "\C-c\C-v"  'follow-scroll-up)
    (define-key follow-mode-map "\C-cv"	    'follow-scroll-down)
    (define-key follow-mode-map "\C-cb"	    'follow-switch-to-buffer)
    (define-key follow-mode-map "\C-cl"	    'follow-recenter)))

(deh-section "Keybinding: Elscreen"
  ;;{{ elscreen
  ;; (global-set-key [C-z c] 'elscreen-create)
  (global-set-key-if-unbind [s-right] 'elscreen-next)
  (global-set-key-if-unbind [s-left]  'elscreen-previous)
  (global-set-key-if-unbind [H-right] 'elscreen-move-right)
  (global-set-key-if-unbind [H-left]  'elscreen-move-left)
  (global-set-key-if-unbind [M-H-right]    'elscreen-swap)
  ;; (global-set-key-if-unbind [H-down]  'elscreen-previous)
;;}}
)

(deh-require-maybe redo                  ;redo
  (global-set-key-if-unbind (kbd "s-=") 'redo))

(deh-section "Comment"
  (global-set-key-if-unbind (kbd "H-c H-c") 'comment-or-uncomment-region))


(deh-section "Box"
  ;; find in ~/.xemacs/session-start.d/art.el
  (global-set-key-if-unbind (kbd "H-c q") 'boxes-create)
  (global-set-key-if-unbind (kbd "H-c r") 'boxes-remove))


(deh-section "hide"
  ;; note S is for Shift
  ;; note s is for Super
  (global-set-key-if-unbind (kbd "s-c")   'hs-hide-all-but-at-point)
  (global-set-key-if-unbind (kbd "s-b")   'hs-hide-block-at-point)
  (global-set-key-if-unbind (kbd "s-u")   'hs-show-all))

;; it is find-file-read-only, find some other key combination.

(deh-require-maybe tramp
  ;; see function key-bnding
  (global-set-key-if-unbind (kbd "C-c C-r") 'find-alternative-file-with-sudo))


(deh-require-maybe planner
  ; s is for super
  ;(global-set-key-if-unbind [(super ?p)] 'plan)

  ;; Super-p not working with Stumpwm for some reason.

  (global-set-key-if-unbind (kbd "s-p p") 'plan)
  (global-set-key-if-unbind [(control super ?p)] 'planner-create-task-from-buffer)
  (global-set-key-if-unbind (kbd "s-p n") 'planner-goto-today)

  (global-set-key-if-unbind (kbd "H-p p") 'plan)
  (global-set-key-if-unbind [(control hyper ?p)] 'planner-create-task-from-buffer)
  (global-set-key-if-unbind (kbd "H-p n") 'planner-goto-today)
  )

(global-set-key-if-unbind (kbd "H-x H-p") 'fprint)

(deh-section "ECB"
  (global-set-key-if-unbind (kbd "H-x H-e") 'ecb-activate)
  (global-set-key-if-unbind (kbd "H-x H-d") 'ecb-deactivate))

(deh-section "Function Keys"
  (global-set-key [f3] 'bury-buffer) ;put the current buffer at the end of the buffer list
                                        ;C-x k
  (global-set-key [f4] 'kill-this-buffer))

;; (defmacro nn (x)
;;   `(global-key-binding (kbd ,x))
;;     )

;; (nn "M-.")

;; (macroexpand '(global-set-key-replace "M-." 'gtags-find-tag '(("M" . "S"))))

;; (global-set-key-replace "M-." find-tag (("M" . "s")))



(deh-require-maybe gtags
  ;; (global-set-key-replace "M->" ww-next-gtag (("M" . "S")))   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
  (global-set-key-replace "M-." gtags-find-tag (("M" . "s"))) ;; M-. finds tag
  (global-set-key-replace "M-*" gtags-pop-stack (("M" . "s")))
  (global-set-key-replace "C-M-." gtags-find-rtag (("M" . "s")))   ;; C-M-. find all references of tag
  (global-set-key-replace "C-M-," gtags-find-symbol (("M" . "s"))) ;; C-M-, find all usages of symbol.
  ;; (global-set-key-if-unbind [(control meta .)] 'gtags-find-rtag)   ;; C-M-. find all references of tag
  ;; (global-set-key-if-unbind [(control meta ,)] 'gtags-find-symbol) ;; C-M-, find all usages of symbol.
)


(deh-require-maybe inline-arithmetic
  ;; http://www.emacswiki.org/emacs/BrianKavanagh
  (global-set-key [C-kp-add]      'inline-arithmetic-add)
  (global-set-key [C-kp-divide]   'inline-arithmetic-divide)
  (global-set-key [C-kp-multiply] 'inline-arithmetic-multiply)
  (global-set-key [C-kp-subtract] 'inline-arithmetic-subtract))


(deh-require-maybe folding
  (global-set-key-if-unbind (kbd "C-+") 'toggle-hiding)
  (global-set-key-if-unbind (kbd "C-\\") 'toggle-selective-display))


(deh-require-maybe db-config
  (global-set-key [f12] 'enter-db-mode))

(deh-require-maybe org
  ;; (global-set-key "\C-cl" 'org-store-link)
  ;; (global-set-key "\C-ca" 'org-agenda)
  ;; (global-set-key "\C-cb" 'org-iswitchb)
  ;;;       (define-key org-mode-map [S-right] 'forward-word)
  ;;;       (define-key org-mode-map [S-left] 'backward-word)
  ;;;       (define-key org-mode-map [S-down] 'scroll-one-line-up)
  ;;;       (define-key org-mode-map [S-up] 'scroll-one-line-down)
)


(deh-section "compile"
  ;; (global-set-key (kbd "C-c s") 'compile-dwim-compile)
  ;; (global-set-key (kbd "C-c r") 'compile-dwim-run)
  )

;; not 'xsteve-gnus
(deh-require-maybe ibuf-ext
  (global-set-key-if-unbind [(meta f7)] 'toggle-ibuffer-group)
  (global-set-key-if-unbind (kbd "C-c b") 'sharad/context-switch-buffer))


(deh-require-maybe remember
  (global-set-key-if-unbind (kbd "s-s r p") 'sharad/remember-planner)
  (global-set-key-if-unbind (kbd "s-s r o") 'sharad/remember-org))

(when (xrequire 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer) ;force
  )


(deh-require-maybe calculator
  (global-set-key-if-unbind (kbd "C-<return>") 'calculator))

(deh-require-maybe calc
  (global-set-key-if-unbind (kbd "H-<return>") 'calc))

(deh-require-maybe xcscope
 (define-key cscope-list-entry-keymap "q" 'bury-buffer))


(deh-require-maybe ido-mode
  (defun insert-ts ()
    (interactive)
    (insert (time-stamp-string "-%:y-%02m-%02d-%02H:%02M:%02S-%u.")))

  (define-key ido-file-completion-map (kbd "C-,") 'insert-ts))


(deh-require-maybe muse-mode
  (define-key muse-mode-local-map (kbd "C-c C-.") 'muse-help))



(deh-require-maybe breadcrumb
  ;; https://github.com/pheaver/breadcrumb
  ;; (global-set-key-if-unbind [(shift space)]         'bc-set)            ;; Shift-SPACE for set bookmark
  (global-set-key-if-unbind (kbd "S-SPC")         'bc-set)            ;; Shift-SPACE for set bookmark
  ;; (global-set-key-if-unbind [(meta j)]              'bc-previous)       ;; M-j for jump to previous
  (global-set-key-if-unbind (kbd "M-j")              'bc-previous)       ;; M-j for jump to previous
  ;; (global-set-key-if-unbind [(shift meta j)]        'bc-next)           ;; Shift-M-j for jump to next
  (global-set-key-if-unbind (kbd "S-M-j")        'bc-next)           ;; Shift-M-j for jump to next
  ;; (global-set-key-if-unbind [(meta up)]             'bc-local-previous) ;; M-up-arrow for local previous
  (global-set-key-if-unbind (kbd "M-<up>")             'bc-local-previous) ;; M-up-arrow for local previous
  ;; (global-set-key-if-unbind [(meta down)]           'bc-local-next)     ;; M-down-arrow for local next
  (global-set-key-if-unbind (kbd "M-<down>")           'bc-local-next)     ;; M-down-arrow for local next
  ;; (global-set-key-if-unbind [(control c)(j)]        'bc-goto-current)   ;; C-c j for jump to current bookmark
  (global-set-key-if-unbind (kbd "C-c j")        'bc-goto-current)   ;; C-c j for jump to current bookmark
  ;; (global-set-key-if-unbind [(control x)(meta j)]   'bc-list)           ;; C-x M-j for the bookmark menu list
  (global-set-key-if-unbind (kbd "C-x M-j")   'bc-list)           ;; C-x M-j for the bookmark menu list
  )


(deh-require-maybe bm
  ;; https://github.com/joodland/bm
  ;; http://www.nongnu.org/bm/
  (global-set-key-if-unbind (kbd "<C-f2>") 'bm-toggle)
  (global-set-key-if-unbind (kbd "<f2>")   'bm-next)
  (global-set-key-if-unbind (kbd "<S-f2>") 'bm-previous)

  (global-set-key-if-unbind (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
  (global-set-key-if-unbind (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
  (global-set-key-if-unbind (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
  )


;; (deh-require-maybe (or lusty-explorer find-file-in-project find-dired)
(with-report-error "binding"
    (deh-require-maybe lusty-explorer
      ;;(deh-section "ff-mode"

      (defvar ff-mode-map
        (let ((map (make-sparse-keymap)))
          ;; These bindings roughly imitate those used by Outline mode.
          ;;(define-key map "\C-c@\C-c"	      'hs-toggle-hiding)
          ;;(define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
          map)
        "Keymap for hideshow minor mode.")

      (if (and
           (boundp 'ff-mode-map)
           ff-mode-map) ; if it is not already defined
          ;; from: http://ergoemacs.org/emacs/elisp_menu_for_major_mode.html
          ;; assign command to keys
          (message "ff-mode-map %s" ff-mode-map)
          (setq ff-mode-map (make-sparse-keymap)))

      (keymap-set-key-if-unbind ff-mode-map (kbd "s-x j") 'jcl-file-cache-ido-find-file)
      (keymap-set-key-if-unbind ff-mode-map (kbd "s-x o") 'find-file-in-other-dir)


      ;; definition for your keybinding and menu

      (define-key ff-mode-map (kbd "s-x l") 'lusty-file-explorer)
      (define-key ff-mode-map (kbd "s-x i") 'ido-find-file)
      (define-key ff-mode-map (kbd "s-x c") 'find-file-in-other-dir)
      (define-key ff-mode-map (kbd "s-x j") 'jcl-file-cache-ido-find-file)
      (define-key ff-mode-map (kbd "s-x p") 'ffip)

      ;; lusty-buffer-explorer find-file-in-project find-dired

      ;; … more here …

      ;; (define-key ff-mode-map [remap comment-dwim] 'xlsl-comment-dwim)
      ;;  ; above: make your comment command “xlsl-comment-dwim” use the current key for “comment-dwim” (because user may have changed the key for “comment-dwim”)

      ;; define your menu
      (define-key ff-mode-map [menu-bar] (make-sparse-keymap))

      (let ((menuMap (make-sparse-keymap "LSL")))
        (define-key ff-mode-map [menu-bar xlsl] (cons "LSL" menuMap))

        (define-key menuMap [about]
          '("About xlsl-mode" . xlsl-about))
        (define-key menuMap [customize]
          '("Customize xlsl-mode" . xlsl-customize))
        (define-key menuMap [separator]
          '("--"))
        (define-key menuMap [convert-rgb]
          '("Convert #rrggbb under cursor" . xlsl-convert-rgb))
        (define-key menuMap [copy-all]
          '("Copy whole buffer content" . xlsl-copy-all))
        (define-key menuMap [syntax-check]
          '("Check syntax" . xlsl-syntax-check))
        (define-key menuMap [lookup-onlne-doc]
          '("Lookup ref of word under cursor" . xlsl-lookup-lsl-ref)))


      (keymap-set-key-if-unbind ff-mode-map (kbd "s-x l") 'lusty-file-explorer)
      (keymap-set-key-if-unbind ff-mode-map (kbd "s-x b") 'lusty-buffer-explorer)

      (deh-require-maybe find-file-in-project
        (keymap-set-key-if-unbind ff-mode-map (kbd "s-x p") 'find-file-in-project))

      (deh-require-maybe find-dired
        (keymap-set-key-if-unbind ff-mode-map (kbd "s-x d") 'find-dired)
        )))



(deh-require-maybe smex
  (smex-initialize)
  (global-set-key-if-unbind (kbd "H-x") 'smex)
  (global-set-key-if-unbind (kbd "H-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c H-x") 'execute-extended-command))

(deh-require-maybe contentswitch
  (global-set-key-if-unbind (kbd "s-x b") 'contentswitch))

(define-key global-map [(kbd "C-x4t")] 'transpose-buffers)
  (global-set-key "\C-x4t" 'transpose-buffers)


(deh-require-maybe recentf
  (global-set-key-if-unbind (kbd "C-c C-o") 'recentf-open-files))

;; (deh-require-maybe recentf-buffer
;;   (global-set-key [?\C-c ?r ?f] 'recentf-open-files-in-simply-buffer))


(deh-section "Necessary"
  (global-set-key-warn-if-bind (kbd "C-x 1") 'delete-other-windows)
  (global-set-key-warn-if-bind (kbd "C-x 0") 'delete-window))


(deh-section "Find place of keybinding"
  (let* ((function 'find-file)
        (advised (and (symbolp function)  (featurep 'advice)  (ad-get-advice-info function)))
           ;; If the function is advised, use the symbol that has the real def, if already set up.
           (real-function  (or (and advised  (let ((origname  (cdr (assq 'origname advised))))
                                               (and (fboundp origname)  origname)))
                               function))
           ;; Get the real definition.
           (def            (if (symbolp real-function) (symbol-function real-function) function))
           (beg            (if (commandp def) "an interactive "  "a "))
           (pt1            (with-current-buffer (help-buffer) (point)))
           file-name string errtype)
    (find-lisp-object-file-name function def)
    ;; (help-xref-button 1 'help-function-def function file-name)
    ))


(deh-section "debug"
  (global-set-key (kbd "s-g g") 'keyboard-debug)
  (global-set-key (kbd "s-g G") 'debug))


(deh-section "bind frequent M-x command to keys"
  ;; http://aaronhawley.livejournal.com/27573.html
  ;; http://www.splode.com/~friedman/software/emacs-lisp/src/suggbind.el
  ;; http://emacs.1067599.n5.nabble.com/Unuseful-keybindings-td273016i20.html
  (deh-require-maybe unbound
    (defvar extended-command-freq-alist nil "")
    ;; (defadvice execute-extended-command (before find-unbound-extended-command activate)
    ;;   ())
    ))


(deh-section "orgextra"
  ;; http://notmuchmail.org/emacstips/
  (global-set-key-if-unbind (kbd "C-c l") 'org-store-link))

(provide 'binding-config)
