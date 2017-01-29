
(when (configuration-layer/package-usedp 'PACKAGE)
  (defun spacemacs/PACKAGE-enable ()
    (progn ;; "Keybinding: Elscreen"
      ;;{{ elscreen
      (define-key evil-emacs-state-map (kbd "") nil)
      ;; (global-unset-key [C-z])
      (global-set-key [] 'elscreen-create)))

  (defun spacemacs/PACKAGE-disable ()
    (progn ;; "Keybinding: Elscreen"
      (define-key evil-emacs-state-map nil)
      (global-unset-key [])))

  (spacemacs/PACKAGE-enable))




(when nil
  ;; You can find out a character's decimal, octal, or hex values by
  ;; placing your cursor on the character, and type Alt+x
  ;; what-cursor-position (Ctrl+x =). You can get more info if you place
  ;; your cursor on the character, then type Ctrl+u Ctrl+x =.
  ;; http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
  ; set unicode data file location. (used by what-cursor-position)
  (let ((x "~/.data/UnicodeData.txt"))
    (when (file-exists-p x)
      (setq describe-char-unicodedata-file x)))

  ;-*- coding: utf-8 -*- ; not working.
  ;
  ; emacs customization for unicode input or
  ; other character insertion
  ; (using dvorak layout)
  ; source Xah Lee  2007-10  ∑ http://xahlee.org/
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;; INSERT UNICODE AND MATH CHARS
  (if
      (xrequire 'open-unicode-template)
      (progn
        (global-set-key (kbd "M-i M-i") 'open-unicode-template)
        (global-set-key (kbd "M-i <up>") "↑")
        (global-set-key (kbd "M-i <down>") "↓")
        (global-set-key (kbd "M-i <left>") "←")
        (global-set-key (kbd "M-i <right>") "→")
        (global-set-key (kbd "M-i <kp-1>") "↙")
        (global-set-key (kbd "M-i <kp-3>") "↘")
        (global-set-key (kbd "M-i <kp-7>") "↖")
        (global-set-key (kbd "M-i <kp-9>") "↗")
        (global-set-key (kbd "M-i <kp-2>") "⇓")
        (global-set-key (kbd "M-i <kp-4>") "⇐")
        (global-set-key (kbd "M-i <kp-6>") "⇒")
        (global-set-key (kbd "M-i <kp-8>") "⇑")
        (global-set-key (kbd "M-i <kp-add>") "⊕")
        (global-set-key (kbd "M-i <kp-subtract>") "⊖")
        (global-set-key (kbd "M-i <kp-multiply>") "×")
        (global-set-key (kbd "M-i *") "⊗")

        (global-set-key (kbd "M-i <") "≤") ; greater htan
        (global-set-key (kbd "M-i >") "≥") ; less than

        (global-set-key (kbd "M-i Z") "ℤ") ; integer
        (global-set-key (kbd "M-i Q") "ℚ") ; rational
        (global-set-key (kbd "M-i R") "ℝ") ; real
        (global-set-key (kbd "M-i C") "ℂ") ; complex

        (global-set-key (kbd "M-i a") "α") ; alpha
        (global-set-key (kbd "M-i b") "β") ; beta
        (global-set-key (kbd "M-i g") "γ") ; gamma
        (global-set-key (kbd "M-i t") "θ") ; theta
        (global-set-key (kbd "M-i l") "λ") ; lambda
        (global-set-key (kbd "M-i p") "π") ; pi

        (global-set-key (kbd "M-i A") "∀")
        (global-set-key (kbd "M-i E") "∃")
        (global-set-key (kbd "M-i ^") "∧") ; and
        (global-set-key (kbd "M-i 6") "∨") ; or
        (global-set-key (kbd "M-i !") "¬") ; not
        (global-set-key (kbd "M-i =") "≡") ; equivalent
        (global-set-key (kbd "M-i +") "≠") ; not equal
        (global-set-key (kbd "M-i .") "∎") ; end of proof

        ;;;; Hyper key

        (setq mac-option-modifier 'hyper)
        (global-set-key (kbd "H-1") 'insert-xahsig)


        ;;;; bullets and other symbols


        ; s § † ‡     — • ★
        ; 1 2 3 4 5 6 7 8 9 0 [ ]
        (global-set-key (kbd "H-2") "§") ; section mark
        (global-set-key (kbd "H-3") "†") ; dagger
        (global-set-key (kbd "H-4") "‡") ; double dagger
        (global-set-key (kbd "H-7") "—") ; em-dash
        (global-set-key (kbd "H-8") "•") ; bullet
        (global-set-key (kbd "H-9") "★") ; star

        ;             ‣ °
        ; ! @ # $ % ^ & * ( ) { }
        (global-set-key (kbd "H-&") "‣") ; triangle bullet
        (global-set-key (kbd "H-*") "°") ; degree

        ;;;; matching pairs

        ; «» {} () [] “”
        ; d  h  t  n  s
        (global-set-key (kbd "H-d") (lambda () (interactive) (insert "«»") (backward-char 1)))
        (global-set-key (kbd "H-h") (lambda () (interactive) (insert "{}") (backward-char 1)))
        (global-set-key (kbd "H-t") (lambda () (interactive) (insert "()") (backward-char 1)))
        (global-set-key (kbd "H-n") (lambda () (interactive) (insert "[]") (backward-char 1)))
        (global-set-key (kbd "H-s") (lambda () (interactive) (insert "“”") (backward-char 1)))

        ; ‹›    『』「」 ‘’
        ; D   H   T   N   S
        (global-set-key (kbd "H-D") (lambda () (interactive) (insert "‹›") (backward-char 1)))
        (global-set-key (kbd "H-T") (lambda () (interactive) (insert "『』") (backward-char 1)))
        (global-set-key (kbd "H-N") (lambda () (interactive) (insert "「」") (backward-char 1)))
        (global-set-key (kbd "H-S") (lambda () (interactive) (insert "‘’") (backward-char 1)))

        ;; hard to type, frequently used chars
        ; + =
        ; g c r l
        (global-set-key (kbd "H-g") (lambda () (interactive) (insert "+")))
        (global-set-key (kbd "H-c") (lambda () (interactive) (insert "=")))

        ;   "
        ; G C R L
        (global-set-key (kbd "H-C") (lambda () (interactive) (insert "\"\"") (backward-char 1)))
        ))
  ; kick the habit
  ;; (global-unset-key (kbd "("))
  ;; (global-unset-key (kbd ")"))
  ;; (global-unset-key (kbd "["))
  ;; (global-unset-key (kbd "]"))

  ;; (global-unset-key (kbd "+"))
  ;; (global-unset-key (kbd "="))
  )
