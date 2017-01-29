;;; packages.el --- lotus-voice layer packages file for Spacemacs.
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
;; added to `lotus-voice-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-voice/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-voice/pre-init-PACKAGE' and/or
;;   `lotus-voice/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-voiceS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-voice-packages
  '(
    speechd
    )
  "The list of Lisp packages required by the lotus-voice layer.

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

(defun lotus-voice/init-speechd ()
  (use-package speechd
    :defer t
    :config
    (progn
      ;; (define-key speechd-speak--mode-map [?\H-s] 'speechd-speak-prefix-command)
      ;; (setq speechd-speak-prefix nil)

      (if (functionp 'speechd-speak)
          (deh-require-maybe speechd
            (speechd-speak) ;start emacs with speechd
            ;; Set punctuation mode of the current connection.
            ;; VALUE must be one of the symbols none, some, all.
            ;; If called with a prefix argument, set it for all connections.
            (speechd-set-punctuation-mode 'some)
            (push "*scratch*" speechd-speak-insertions-in-buffers)
            ;speechd-out-active-drivers default value (ssip brltty)
            (setq speechd-out-active-drivers '(ssip)
                  speechd-default-key-priority 'message
                  speechd-speak-echo 'word
                  ;; speechd-speak-prefix (kbd "C-e")
                  ;; speechd-speak-prefix (kbd "H-e")
                  ;; speechd-speak-prefix [?\H-s]

                  ;; Ask the question
                  ;; /usr/share/emacs/site-lisp/speechd-el/speechd-speak.el 2034

                  )))



      ;; arrange C-m binding in C-m C-m

      ;; (global-set-key "\C-e" 'move-end-of-line)

      ;; `speechd-speak-mode' Minor Mode Bindings Starting With C-e:
      ;; key             binding
      ;; ---             -------

      ;; C-e C-a         speechd-add-connection-settings
      ;; C-e C-b         Prefix Command
      ;; C-e C-e         move-end-of-line
      ;; C-e TAB         Prefix Command
      ;; C-e C-l         speechd-speak-spell
      ;; C-e RET         speechd-speak-read-mode-line
      ;; C-e C-n         speechd-speak-read-next-line
      ;; C-e C-p         speechd-speak-read-previous-line
      ;; C-e C-r         speechd-speak-read-rectangle
      ;; C-e C-s         speechd-speak
      ;; C-e C-x         speechd-unspeak
      ;; C-e SPC         speechd-out-resume
      ;; C-e '           speechd-speak-read-sexp
      ;; C-e .           speechd-speak-read-sentence
      ;; C-e 1 .. C-e 9  speechd-speak-key-set-predefined-rate
      ;; C-e >           speechd-speak-read-rest-of-buffer
      ;; C-e [           speechd-speak-read-page
      ;; C-e b           speechd-speak-read-buffer
      ;; C-e c           speechd-speak-read-char
      ;; C-e d           Prefix Command
      ;; C-e i           speechd-speak-last-insertions
      ;; C-e l           speechd-speak-read-line
      ;; C-e m           speechd-speak-last-message
      ;; C-e o           speechd-speak-read-other-window
      ;; C-e p           speechd-out-pause
      ;; C-e q           speechd-speak-toggle-speaking
      ;; C-e r           speechd-speak-read-region
      ;; C-e s           speechd-out-stop
      ;; C-e w           speechd-speak-read-word
      ;; C-e x           speechd-out-cancel
      ;; C-e z           speechd-out-repeat
      ;; C-e {           speechd-speak-read-paragraph

      ;; C-e d C-v       speechd-set-synthesizer-voice
      ;; C-e d .         speechd-set-punctuation-mode
      ;; C-e d V         speechd-set-volume
      ;; C-e d c         speechd-set-capital-character-mode
      ;; C-e d l         speechd-speak-set-language
      ;; C-e d o         speechd-set-output-module
      ;; C-e d p         speechd-set-pitch
      ;; C-e d r         speechd-set-rate
      ;; C-e d v         speechd-set-voice

      ;; C-e C-b k       speechd-speak-toggle-braille-keys

      ;; C-e TAB b       speechd-speak-buffer-info
      ;; C-e TAB c       speechd-speak-coding-info
      ;; C-e TAB f       speechd-speak-frame-info
      ;; C-e TAB h       speechd-speak-header-line-info
      ;; C-e TAB i       speechd-speak-input-method-info
      ;; C-e TAB m       speechd-speak-mode-info
      ;; C-e TAB p       speechd-speak-process-info

      ;; 
      ;; `speechd-speak-map-mode' Minor Mode Bindings Starting With C-e:
      ;; key             binding
      ;; ---             -------

      ;; C-e C-b         Prefix Command
      ;; C-e TAB         Prefix Command
      ;; C-e d           Prefix Command

      ;; 
      ;; Global Bindings Starting With C-e:
      ;; key             binding
      ;; ---             -------

      ;; [back]

      )))

;;; packages.el ends here
