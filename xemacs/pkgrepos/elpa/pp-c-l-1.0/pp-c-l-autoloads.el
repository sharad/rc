;;; pp-c-l-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (refresh-pretty-control-l pretty-control-l-mode
;;;;;;  pp^L-^L-string Pretty-Control-L) "pp-c-l" "pp-c-l.el" (20362
;;;;;;  27767))
;;; Generated autoloads from pp-c-l.el

(let ((loads (get 'Pretty-Control-L 'custom-loads))) (if (member '"pp-c-l" loads) nil (put 'Pretty-Control-L 'custom-loads (cons '"pp-c-l" loads))))

(defface pp^L-highlight (if (> emacs-major-version 21) '((((type x w32 mac graphic) (class color)) (:box (:line-width 3 :style pressed-button))) (t (:inverse-video t))) '((((type x w32 mac graphic) (class color)) (:foreground "Blue" :background "DarkSeaGreen1")) (t (:inverse-video t)))) "\
*Face used to highlight `pp^L-^L-vector'." :group (quote Pretty-Control-L) :group (quote faces))

(defvar pp^L-^L-string "          Section (Printable Page)          " "\
*Highlighted string displayed in place of each Control-l (^L) character.
If `pp^L-^L-string-function' is non-nil, then the string that function
returns is used instead of `pp^L-^L-string'.")

(custom-autoload 'pp^L-^L-string "pp-c-l" t)

(defalias 'pp^l 'pretty-control-l-mode)

(defvar pretty-control-l-mode nil "\
Non-nil if Pretty-Control-L mode is enabled.
See the command `pretty-control-l-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pretty-control-l-mode'.")

(custom-autoload 'pretty-control-l-mode "pp-c-l" nil)

(autoload 'pretty-control-l-mode "pp-c-l" "\
Toggle pretty display of Control-l (`^L') characters.
With ARG, turn pretty display of `^L' on if and only if ARG is positive.

\(fn &optional ARG)" t nil)

(autoload 'refresh-pretty-control-l "pp-c-l" "\
Reinitialize `pretty-control-l-mode', if on, to update the display.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("pp-c-l-pkg.el") (20362 27767 615019))

;;;***

(provide 'pp-c-l-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pp-c-l-autoloads.el ends here
