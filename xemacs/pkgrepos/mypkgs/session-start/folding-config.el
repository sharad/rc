

;;(load "folding" 'nomessage 'noerror)
(deh-require-maybe folding
      (folding-mode-add-find-file-hook))

;; TODO:
;; Define a function that will give the folding marks for current mode from folding-mode-marks-alist.

;;;       (folding-add-to-marks-list 'php-mode    "//{"  "//}"  nil t)
;;;       (folding-add-to-marks-list 'prolog-mode "%{{{" "%}}}" nil t)
;;;       (folding-add-to-marks-list 'html-mode   "<!-- {{{ " "<!-- }}} -->" " -->" nil t)
;;;       (folding-add-to-marks-list 'lisp-mode   ";;{{{" ";;}}}" nil t)))

;; Universal code folding

;; set-selective-display is a simple, universal function which hides
;; code according to its indentation level. It can be used as a
;; fall-back for hs-toggle-hiding.

;; First, define a toggling function based on set-selective-display:

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

;; The above is based on jao’s quick and dirty code folding code. The
;; hiding level can be passed as an prefix argument, or is based on
;; the horizontal position of point. Calling the function again brings
;; the code back.

;; Now, define another function which calls hs-toggle-hiding if it’s
;; available, or else falls back on toggle-selective-display:

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          ;; (hs-show-all)
          (hs-show-block)
          )
    (toggle-selective-display column)))

;; This is more robust than the mere hs-toggle-hiding
;; function. Specifically, it will return the buffer to its original
;; state if something goes awry due to an uneven number of open and
;; close brackets (e.g., extensive use of #ifdef macro declarations).

;; Finally, set up key bindings and automatically activate
;; hs-minor-mode for the desired major modes:

;; see binding.el

;; Now we have (rudimentary) code folding for all modes, not just the
;; ones listed above.  Automatically Activating

;; Does anyone else have trouble viewing the text after this next
;; paragraph?

;; SteveWainstead contributes the following code for Emacs 20, which
;; automatically activates HideShow for Perl, Java, or Lisp sources.

;; hideshow for programming java-mode
(load-library "hideshow")

(add-element-to-lists 'hs-minor-mode pgm-langs)

;; Other Options

;; Here is a set of perhaps other useful options you can customize:

;; Hide the comments too when you do a 'hs-hide-all'
(setq hs-hide-comments nil)
;; Set whether isearch opens folded comments, code, or both
;; where x is code, comments, t (both), or nil (neither)
(setq hs-isearch-open 'x)
;; Add more here

;; Maintainer version also has spiffy new variable: hs-set-up-overlay
;; Extensions

;; The extension hideshow-org makes hideshow.el’s functionality behave
;; like org-mode’s. The code is located on github here. The
;; announcement and screencast of it is here.

;; To get +/- markers on foldable regions, have a look at
;; hideshowvis.el.

;; The answer to the question “How do I get it to expand upon a
;; goto-line? (like it does in search mode)” is by adding some advice.

;; advice is similar to a hook. It may be executed before or after an
;; Emacs function. It can affect both the parameters and the return
;; value of the function. See AdvisingFunctions.

;; For expansion on goto-line, adding the following code to your
;; .emacs file will do the trick:

    (defadvice goto-line (after expand-after-goto-line
                                activate compile)

        "hideshow-expand affected block when using goto-line in a collapsed buffer"
        (save-excursion
           (hs-show-block)))

;; Similar solutions can most likely be found for the find-tag and
;; revert-file problem questions below (unless you want to preserve
;; all expanded/compressed parts of the code in the latter case).

;; from: http://www.ibm.com/developerworks/cn/linux/l-plset/emacs.dat
;;{{{ load the folding mode: fold sections marked with folding marks.
;; I find the folding mode to be better than Emacs' outline mode,
;; which can be annoying in programs.  The folding mode is not
;; obtrusive, and meshes nicely with general comments.
;; (load "folding" 'nomessage 'noerror)
(folding-mode-add-find-file-hook)
;; (defun my-folding-load-hook ()
;;   "Folding setup."

;;   (folding-install)  ;; just to be sure

;;   (defvar folding-mode-marks-alist nil)

;;   (let* ((ptr (assq 'text-mode folding-mode-marks-alist)))
;;     (setcdr ptr (list "# {{{" "# }}}")))

;;   )
;; (add-hook 'folding-load-hook 'my-folding-load-hook)

;; from http://www.dotemacs.de/dotfiles/DavidJolley.emacs.html
(defun my-folding-mode-hook ()
  (interactive)
  (setq fold-behave-table
	'((close	fold-hide)
	  (open	fold-enter)
	  (up		fold-exit)
	  (other	fold-mouse-call-original)))
	 (define-key folding-mode-map [mouse-3] 'fold-mouse-context-sensitive))

;;; I like the keys the way they used to be...
(setq fold-default-keys-function 'fold-bind-backward-compatible-keys)

;
;;}}}



(defun hs-hide-all-but-at-point ()
  "Hide all but current"
  (interactive)                         ;assigned to key in keymap.el
  (hs-hide-all)
  (hs-show-block))


;; folding-add-to-marks-list

(defun folding-mark-info ()
  "folding-mark-info"
  (interactive)
  (apply #'message "mode %s: start %s, end %s"
   (assoc major-mode folding-mode-marks-alist)))



(deh-section "persistance"
  ;; http://stackoverflow.com/questions/2479977/emacs-persistent-folding-mode
  ;; http://stackoverflow.com/questions/2299133/how-to-execute-emacs-grep-find-link-in-the-same-window
  ;; http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
  (defvar omm-state nil
    "file local variable storing outline overlays")
  (defun omm-state-mode (&optional arg)
    "poor man's minor mode to re-apply the outline overlays "
    (interactive)
    (omm-re-enable-outline-state)
    (add-hook 'before-save-hook 'omm-state-save))
  (defun omm-get-all-overlays ()
    "return a list of outline information for all the current buffer"
    (save-excursion
      (let ((all-overlays (overlays-in (point-min) (point-max))))
        (mapcar (lambda (o)
                  (list (overlay-start o) (overlay-end o) (overlay-get o 'invisible)))
                (reverse all-overlays)))))
  (defun omm-re-enable-outline-state (&optional arg)
    "turn on outline-minor-mode and re-apply the outline information"
    (outline-minor-mode 1)
    (when (listp omm-state)
      (mapcar (lambda (p)
                (apply 'outline-flag-region p))
              omm-state)))
  (defun omm-state-save ()
    "save the outline state in a file local variable
Note: this just replaces the existing value, you need to start
it off by adding something like this to your file:

# Local Variables:
# omm-state:()
# mode:omm-state
# End:
"
    (ignore-errors
      (save-excursion
        (goto-char (point-max))
        (when (search-backward "omm-state:" nil t)
          (goto-char (match-end 0))
          (kill-sexp)
          (princ (omm-get-all-overlays) (current-buffer)))))
    nil))


(provide 'folding-config)

