;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bindings
;; Time-stamp: <2011-11-09 13:23:06 spratap>
;;

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


  (defun key (desc)
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
      (global-set-key-if-unbind "\C-ce" 'anything-for-files)))

(deh-section "Buffer"
  (define-key global-map [?\C-x right] 'next-buffer)
  (define-key global-map [?\C-x left] 'prev-buffer))

(deh-require-maybe 'follow-mode
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
  (global-set-key-if-unbind [H-right] 'elscreen-next)
  (global-set-key-if-unbind [H-left]  'elscreen-previous)
  (global-set-key-if-unbind [M-H-right]    'elscreen-swap)
  ;; (global-set-key-if-unbind [H-down]  'elscreen-previous)
;;}}
)

(deh-require-maybe 'redo                  ;redo
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

(deh-require-maybe 'tramp
  ;; see function key-bnding
  (global-set-key-if-unbind (kbd "C-c C-r") 'find-alternative-file-with-sudo))


(deh-require-maybe 'planner
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


(deh-require-maybe 'gtags
  (global-set-key-if-unbind "\M->" 'ww-next-gtag)   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
  (global-set-key-if-unbind "\M-." 'gtags-find-tag) ;; M-. finds tag
  (global-set-key-if-unbind [(control meta .)] 'gtags-find-rtag)   ;; C-M-. find all references of tag
  (global-set-key-if-unbind [(control meta ,)] 'gtags-find-symbol) ;; C-M-, find all usages of symbol.
)


(deh-require-maybe 'inline-arithmetic
  ;; http://www.emacswiki.org/emacs/BrianKavanagh
  (global-set-key [C-kp-add]      'inline-arithmetic-add)
  (global-set-key [C-kp-divide]   'inline-arithmetic-divide)
  (global-set-key [C-kp-multiply] 'inline-arithmetic-multiply)
  (global-set-key [C-kp-subtract] 'inline-arithmetic-subtract)
)


(deh-require-maybe 'folding
  (global-set-key-if-unbind (kbd "C-+") 'toggle-hiding)
  (global-set-key-if-unbind (kbd "C-\\") 'toggle-selective-display))


(user-provide 'binding)

