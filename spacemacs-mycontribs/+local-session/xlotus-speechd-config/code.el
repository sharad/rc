;;
;; speechd.el
;; Login : <s@taj>
;; Started on  Fri Jan 21 00:35:27 2011 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;


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



(provide 'speechd-config)
