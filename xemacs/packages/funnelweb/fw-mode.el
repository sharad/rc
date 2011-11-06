
; A.B.Coates
; Department of Physics
; The University of Queensland  QLD  4072
; Australia
; Standard GNU copyleft provisions apply to this file.

(require 'switch-mode)

(provide 'fw-mode)


(defvar fw-TeX-mode 'tex-mode
  "*The default TeX-based foundation for FunnelWeb mode.")


(funcall fw-TeX-mode)


(defvar fw-untabify-before-saving-p t
  "*Whether to convert tabs to spaces before saving FunnelWeb
buffers.")

(defvar fw-tab-string "	"
  "Tab character as string.")

(defvar fw-tab-space-replacement "        "
  "*String of spaces for replacing tabs.")
(make-variable-buffer-local 'fw-tab-space-replacement)

(defvar fw-remove-trailing-spaces-before-saving-p t
  "*Whether to remove trailing spaces on lines before saving
FunnelWeb buffers.")

(defvar fw-space (string-to-char " ")
  "Space character, used in removing trailing spaces.")

(defvar fw-trailing-space-pattern " +$"
  "*Pattern used to identify trailing spaces on lines.")

(defvar fw-null-string ""
  "Empty string used for replacing trailing spaces.")

(defvar fw-switch-minor-mode-init 1
  "*Whether to activate the Switch minor mode automatically
on entering FunnelWeb mode.  A positive integer for yes.")

(defvar fw-buffer-p nil
  "Whether a buffer is a FunnelWeb buffer or not.")
(make-variable-buffer-local 'fw-buffer-p)
(switch-add-to-preservation-list 'fw-buffer-p)

(defvar fw-quote-style t
  "*Whether to map \" to a single character or use (La)TeX
mapping to `` or '' as appropriate.  Can be set to nil to
remove the (La)TeX mapping, or anything else to enable it.")

(defvar fw-auto-fill-mode 1
  "*Whether autofill-mode is automatically invoked for
FunnelWeb files.  Set to 0 for no, to a positive number
for yes.")

(defvar fw-command "fw"
  "*The command to run FunnelWeb on a file.
Any pre-options (fw-command-pre-options) will be appended
to this string, separated by a space, followed by the
filename, also separated by a space, and finally any
post-options (fw-command-post-options), again separated
by a space.")

(defvar fw-command-pre-options nil
  "*Options which go before the filename when
calling FunnelWeb.")
(make-variable-buffer-local 'fw-command-pre-options)

(defvar fw-command-post-options "+t +D"
  "*Options which go after the filename when
calling FunnelWeb.")
(make-variable-buffer-local 'fw-command-post-options)

(defvar fw-shell-cd-command "cd"
  "*Command to give to shell running FunnelWeb to
change directory.  The value of fw-directory will be
appended to this, separated by a space.")

(defvar fw-mode-syntax-table nil
  "Syntax table used while in FunnelWeb mode.")

(defvar fw-mode-map nil
  "Keymap for FunnelWeb mode.")

(defvar fw-close-definition-block-additive t
  "*Whether definitions should be closed using \"+=\" (t) or
\"==\" (nil).")
(make-variable-buffer-local
 'fw-close-definition-block-additive)

(defvar fw-close-definition-block-newline-suppress t
  "*Whether definitions should be closed using \"@-\".")
(make-variable-buffer-local
 'fw-close-definition-block-newline-suppress)

(defvar fw-close-definition-block-blank-line t
  "Whether definitions should be closed leaving
a blank line.")
(make-variable-buffer-local
 'fw-close-definition-block-blank-line)

(fset 'FW-mode 'fw-mode)
(fset 'funnelweb-mode 'fw-mode)
(fset 'FunnelWeb-mode 'fw-mode)


(defun fw-mode ()
  "Major mode for editing FunnelWeb files.  Built on top of
tex-mode.  Makes @} , @) , and @> display their matching
opening braces @{ , @( , or @< .

Use \\[fw-buffer] to run FunnelWeb on the current buffer
*without* saving it.
Use \\[fw-file] to be prompted to save the buffer to a file
first, before running FunnelWeb on the file.
Use \\[fw-tex] to run the (La)TeX output file through TeX or
LaTeX as appropriate.
Use \\[fw-print] to print a .dvi file.
Use \\[fw-show-print-queue] to show the print queue that
\\[fw-print] put your job on.
Commands from tex-mode (or a similar selected mode) are
mapped to operate on the (La)TeX file produced by FunnelWeb.

Key sequences:

see documentation for `switch-mode' for key sequences
relating to mode switching.

Mode variables:
fw-autofill-mode
        Whether autofill-mode is automatically invoked for
        FunnelWeb files.  Set to 0 for no, to a positive
        number for yes.
fw-TeX-mode
        The major (La)TeX mode on which fw-mode is based.
        Typically either 'tex-mode or 'latex-mode
        (or perhaps 'tex-init for Auc-TeX
         or 'html-mode for hypertext ...).
fw-directory
        Directory in which to create temporary files for
        TeX jobs run by \\[fw-buffer] or \\[fw-file].
fw-use-TeX-quote-style
        Set this to nil so the the \" key produces the
        normal \" character.  If set to true, the \" key
        works as for (La)TeX mode, producing either
        `` or ''.
fw-dvi-print-command
        Command string used by \\[fw-print] to print a
        .dvi file.
fw-show-queue-command
        Command string used by \\[fw-show-print-queue] to
        show the print queue that \\[fw-print] put your
        job on.

Entering FunnelWeb-mode calls the value of fw-mode-hook."
  (interactive)
  (funcall fw-TeX-mode)
  (hack-local-variables)
  (setq mode-name "FunnelWeb")
  (setq major-mode 'fw-mode)
  (auto-fill-mode fw-auto-fill-mode)
  (if fw-mode-map (use-local-map fw-mode-map))
  (setq fw-buffer-p t)
  (switch-mode fw-switch-minor-mode-init)
  (run-hooks 'fw-mode-hook))

(defun fw-untabify-save-buffer ()
  "Save the buffer, optionally removing tabs and trailing spaces
first if the buffer is a FunnelWeb buffer."
  (interactive)
  (if fw-buffer-p
      (progn
        (if fw-untabify-before-saving-p
            (progn
              (message "Removing tabs ...")
              (save-excursion
                (goto-char (point-min))
                (while (search-forward fw-tab-string nil t)
                  (replace-match fw-tab-space-replacement nil t)))))
        (if fw-remove-trailing-spaces-before-saving-p
            (progn
              (message "Removing trailing spaces ...")
              (save-excursion
                (goto-char (point-min))
                (while (< (point) (point-max))
                  (end-of-line)
                  (while (eq (preceding-char) fw-space)
                    (backward-delete-char 1))
                  (forward-line 1)))))))
  (save-buffer))
(if fw-mode-map
 (progn
  (defvar tex-mode-map nil)
  (if tex-mode-map
      (setq fw-mode-map tex-mode-map)
    (progn
      (defvar latex-mode-map nil)
      (if latex-mode-map
          (setq fw-mode-map latex-mode-map)
        (progn
          (defvar LaTeX-mode-map nil)
          (if LaTeX-mode-map
              (setq fw-mode-map LaTeX-mode-map)
            (progn
              (defvar TeX-mode-map nil)
              (if TeX-mode-map
                  (setq fw-mode-map TeX-mode-map))))))))))


(global-set-key "\C-x\C-s" 'fw-untabify-save-buffer)

