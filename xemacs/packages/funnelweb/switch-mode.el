
; A.B.Coates
; Department of Physics
; The University of Queensland  QLD  4072
; Australia
; Standard GNU copyleft provisions apply to this file.

(provide 'switch-mode)


(defvar switch-mode nil
  "Whether the region checking minor mode is active or not.")
(make-variable-buffer-local 'switch-mode)

(defvar switch-open-region-string "@{"
  "String defining the beginning of a code region.")
(make-variable-buffer-local 'switch-open-region-string)

(defvar switch-close-region-string "@}"
  "String defining the end of a code region.")
(make-variable-buffer-local 'switch-close-region-string)

(defvar switch-boundary-region-pattern "@\\({\\|}\\)"
  "Regular expression defining the beginning or end of a code
region.")
(make-variable-buffer-local 'switch-boundary-region-pattern)

(defvar switch-region-min nil
  "Start of current macro or text region, or nil.")
(make-variable-buffer-local 'switch-region-min)

(defvar switch-region-max nil
  "End of current macro or text region, or nil.")
(make-variable-buffer-local 'switch-region-max)

(defvar switch-not-in-region-check-routine-p t
  "Whether a region check is not in progress.")
(make-variable-buffer-local
 'switch-not-in-region-check-routine-p)

(defvar switch-force-region-check-p nil
  "Whether a region check should be forced or not,
regardless of whether it seems to be required or not.")
(make-variable-buffer-local 'switch-force-region-check-p)

(defvar switch-mode-prompt-p t
  "*Whether the user should be prompted for a new
major mode type each time point enters a code region.")
(make-variable-buffer-local 'switch-mode-prompt-p)

(defvar switch-current-mode nil
  "The current mode for a code region.")
(make-variable-buffer-local 'switch-current-mode)

(defvar switch-currently-in-region-p nil
  "Whether point is in a code region or not.")
(make-variable-buffer-local 'switch-currently-in-region-p)

(defvar switch-highlight-p nil
  "Whether to highlight regions as they are entered.
(This requires the `hilit19' library to be loaded.)")
(make-variable-buffer-local 'switch-highlight-p)

(defvar switch-preservation-list
  (list 'switch-current-mode 'switch-mode-prompt-p)
  "List of names of variables that need to be specially
preserved by switch-mode operations.")



(defun switch-convert-to-default-string ( object )
  "Convert an object into a default string for
name completion."
  (if object (prin1-to-string object)))

(defun make-switch-mode-list ()
  "Return a list of all commands ending in \"-mode\",
in the form of an alist."
  (let ((mode-list (apropos-internal "-mode$")))
    (mapcar 'switch-convert-to-alist-string mode-list)))

(defun switch-convert-to-alist-string ( object )
  "Convert an object to a string in its own list,
the format appropriate for an alist."
  (list (prin1-to-string object)))

(defun switch-search-forward-check ( regexp string )
  "Search forward for the given regexp, and if the matching
pattern is found and it matches the given string, return `1'
and the beginning of the pattern, else return `-1' and the
beginning of the pattern.  If the pattern is not found when
the end of the buffer is reached, return `0' and the end of
the buffer."
  (save-excursion
    (if (search-forward-regexp regexp (point-max) t)
        (if (string-equal
             string
             (buffer-substring
              (match-beginning 0)
              (match-end 0)))
            (list 1 (match-beginning 0))
          (list -1 (match-beginning 0)))
      (list 0 (point-max)))))

(defun switch-search-backward-check ( regexp string )
  "Search backward for the given regexp, and if the matching
pattern is found and it matches the given string, return `1'
and the end of the pattern, else return `-1' and the end of
the pattern.  If the pattern is not found when the beginning
of the buffer is reached, return `0' and the beginning of the
buffer."
  (save-excursion
    (if (search-backward-regexp regexp (point-min) t)
        (if (string-equal
             string
             (buffer-substring
              (match-beginning 0)
              (match-end 0)))
            (list 1 (match-end 0))
          (list -1 (match-end 0)))
      (list 0 (point-min)))))



(defvar switch-mode-list (make-switch-mode-list)
  "Return a list of strings of all possible major modes
from which the user can choose.")

(defvar switch-beep-on-mode-selection-p t
  "*Whether to beep when a mode selection is necessary.")



(defun switch-mode
  ( &optional set-mode-p
              no-check-p
              region-open-string
              region-close-string
              region-boundary-pattern )
  "With no arguments, toggle whether the region mode
checking is activated or not.  Optional first argument
SET-MODE-P can be used to definitely switch the region
mode checking on or off: a positive number for on, off
otherwise.
    Optional second argument NO-CHECK-P is nil for a
check of whether the point is in a code region or not
to be taken immediately.  With any other value, no check
is done.
    Optional third and fourth arguments REGION-OPEN-STRING
and REGION-CLOSE-STRING define the boundaries by which a
code region is recognised.
    Optional fourth argument REGION-BOUNDARY_PATTERN should
be a pattern equivalent to
    \\(REGION-OPEN-STRING\\|REGION-CLOSE-STRING\\)
i.e. a pattern which recognises either an opening or closing
pattern.

Key sequences:

\\C-x\\C-a key sequences are used for the \"Switch\" minor
mode, which allows modes to be swapped while editing a file.

\\C-x \\C-a t : toggle Switch minor-mode on and off
\\C-x \\C-a c : check whether in macro region or not
\\C-x \\C-a p : toggles whether the default macro region
                mode is used with or without prompting
\\C-x \\C-a s : set the current macro region mode"
  (interactive)
  (let ((switch-on-p
         (or (and (numberp set-mode-p)
                  (> set-mode-p 0))
             (not switch-mode))))
    (if switch-on-p
        (let ((old-switch-mode-value
               switch-mode))
          (setq switch-mode t
                switch-force-region-check-p t)
          (if (not (or no-check-p old-switch-mode-value))
              (switch-check-if-in-region)))
      (setq switch-mode nil))
    (if region-open-string
        (setq switch-open-region-string
              region-open-string))
    (if region-close-string
        (setq switch-close-region-string
              region-close-string))
    (if region-boundary-pattern
        (setq switch-boundary-region-pattern
              region-boundary-pattern))
    (force-mode-line-update t)))

(defun switch-post-command-hook-function ()
  "Function to check if the user is in a code definition
region or not."
  (if (and (not isearch-mode)
           switch-mode
           switch-not-in-region-check-routine-p
           (or (not switch-region-min)
               (not switch-region-max)
               (let ((the-point (point)))
                 (or
                  (< the-point switch-region-min)
                  (> the-point switch-region-max)))))
      (let ((switch-not-in-region-check-routine-p nil))
            (switch-check-if-in-region))))

(defun switch-check-if-in-region ()
  "Check if point is inside or outside a FunnelWeb macro
definition. If outside, switch to (Auc/La)TeX mode.
If inside, either ask the user for a mode, defaulting
to the last mode used in a macro definition, or directly
use the last mode chosen (depends on value of variable
switch-mode-prompt-p)."
  (interactive)
  (let ((backward-search-result
        (switch-search-backward-check
         switch-boundary-region-pattern
         switch-open-region-string))
        (forward-search-result
        (switch-search-forward-check
         switch-boundary-region-pattern
         switch-close-region-string))
        (backward-search-value)
        (forward-search-value))
    (setq backward-search-value
          (car backward-search-result))
    (setq forward-search-value
          (car forward-search-result))
    (setq switch-region-min
          (car (cdr backward-search-result)))
    (setq switch-region-max
          (car (cdr forward-search-result)))
    (if (not (or (< backward-search-value 0)
                 (< forward-search-value 0)
                 (and (eq backward-search-value 0)
                      (eq forward-search-value 0))))
        (if (or (not switch-currently-in-region-p)
                switch-force-region-check-p)
            (progn
              (switch-select-mode (not switch-mode-prompt-p))
              (if switch-current-mode
                  (progn
                    (message (concat "Changed to "
                                     (prin1-to-string
                                      switch-current-mode)))
                    (setq switch-currently-in-region-p t)))))
      (if switch-currently-in-region-p
          (let ((preservation-list
                 (switch-get-preservation-values)))
            (fw-mode)
            (switch-set-preservation-values
             preservation-list)
            (message "Changed to fw-mode")
            (setq switch-currently-in-region-p nil))))
    (setq switch-force-region-check-p nil)))

(defun switch-mode-prompt-toggle ()
  "Toggle whether the user is prompted for the major mode
each time a code region is entered."
  (interactive)
  (setq switch-mode-prompt-p (not switch-mode-prompt-p))
  (if switch-mode-prompt-p
      (message "Mode prompting switched on.")
    (message "Mode prompting switched off.")))

(defun switch-mode-force-prompt ()
  "Cause the user to be prompted for a mode type for
the current region, if a code region."
  (interactive)
  (let ((switch-force-region-check-p t))
    (switch-check-if-in-region)))

(defun switch-select-mode ( &optional no-prompt-p )
  "Interactively get the user to select a macro mode,
giving the last-used macro mode as a default, and allowing
the user to select from all possible major mode commands
with name completion.
    Optional parameter NO-PROMPT-P, if non-nil, stops
prompting from taking place unless the current mode
(switch-current-mode) is nil."
  (let ((old-switch-mode-value
         (if switch-mode 1 0)))
    (if (not (and no-prompt-p switch-current-mode))
        (progn
          (if switch-beep-on-mode-selection-p (beep))
          (let ((new-mode (completing-read
                           "Mode: "
                           switch-mode-list
                           nil
                           nil
                           (switch-convert-to-default-string
                            switch-current-mode)
                           nil)))
            (setq switch-current-mode
                  (car (read-from-string new-mode))))))
    (let ((preservation-list
           (switch-get-preservation-values))
          (region-min switch-region-min)
          (region-max switch-region-max))
      (message "Changing to %s"
               (prin1-to-string switch-current-mode))
      (if switch-current-mode (funcall switch-current-mode))
      (if (and switch-highlight-p region-min region-max)
          (progn
            (hilit-unhighlight-region region-min region-max)
            (hilit-highlight-region region-min region-max)))
      (switch-set-preservation-values preservation-list))
    (switch-mode old-switch-mode-value t)
    (force-mode-line-update)))

(defun update-switch-mode-list ()
  "Update the mode list used for the switch minor-mode."
  (interactive)
  (setq switch-mode-list (make-switch-mode-list)))

(defun switch-add-to-preservation-list ( var )
  "Add a variable to the switch-mode preservation list."
  (if (not (memq var switch-preservation-list))
      (setq switch-preservation-list
            (append switch-preservation-list (list var)))))

(defun switch-get-preservation-values ()
  "Returns a list of current values which can be used by
switch-set-preservation-values to restore the values of
the variables in the preservation list
(switch-preservation-list)."
  (if switch-preservation-list
      (mapcar
       'switch-preserve-form
       switch-preservation-list)))

(defun switch-preserve-form ( var )
  "Return a list of the variable name and its value."
  (list var (eval var)))

(defun switch-set-preservation-values ( vallist )
  "Reset the variables in the preservation value list
using the value list previously created by
switch-get-preservation-values."
  (mapcar 'switch-apply-set vallist))

(defun switch-apply-set ( varpair )
  "Given a list of the form (NAME . VALUE), assign the value
to the name."
  (apply 'set varpair))



(defvar switch-prefix nil
  "Switch minor-mode region check \\C-x\\C-a keymap.")

(define-prefix-command 'switch-prefix)

(global-set-key "\C-x\C-a" 'switch-prefix)

(define-key switch-prefix "c" 'switch-check-if-in-region)

(define-key switch-prefix "t" 'switch-mode)

(define-key switch-prefix "p" 'switch-mode-prompt-toggle)

(define-key switch-prefix "s" 'switch-mode-force-prompt)

(setq minor-mode-alist
      (reverse
       (append (reverse minor-mode-alist)
               (list (list
                      'switch-mode
                      " Switch")))))
(force-mode-line-update t)

(add-hook 'post-command-hook
          'switch-post-command-hook-function)

(run-hooks 'switch-mode-hook)
