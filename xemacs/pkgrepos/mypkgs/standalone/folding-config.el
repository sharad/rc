;;; -*- mode: emacs-lisp; -*-




(require 'macros-config)



(deh-require-maybe reveal
  (reveal-mode 1)

  (setq
   search-invisible 'open
   hs-isearch-open t))

;;(load "folding" 'nomessage 'noerror)
(deh-require-maybe folding
      (folding-mode-add-find-file-hook))

;; TODO:
;; Define a function that will give the folding marks for current mode from folding-mode-marks-alist.

;;;       (folding-add-to-marks-list 'php-mode    "//{"  "//}"  nil t)
;;;       (folding-add-to-marks-list 'prolog-mode "%{{{" "%}}}" nil t)
;;;       (folding-add-to-marks-list 'html-mode   "<!-- {{{ " "<!-- }}} -->" " -->" nil t)
;;;       (folding-add-to-marks-list 'lisp-mode   ";;{{{" ";;}}}" nil t)))




(deh-require-maybe hideshow
  ;; Universal code folding


  (setq hs-set-up-overlay
        (defun my-display-code-line-counts (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put ov 'display
                         (propertize
                          ;; (format " ... <%d>"
                          ;;         (count-lines (overlay-start ov)
                          ;;                      (overlay-end ov)))
                          (make-string
                           (max 3 (1+ (/ (count-lines (overlay-start ov) (overlay-end ov)) 10)))
                           ?.)
                          'face 'font-lock-type-face))
            (overlay-put ov 'help-echo
                         (buffer-substring;; -no-properties
                          (overlay-start ov) (overlay-end ov))))))


  ;; For expansion on ‘goto-line’, adding the following code to your InitFile will do the trick:
  (defadvice goto-line (after
                        expand-after-goto-line
                        activate
                        compile)
    "hideshow-expand affected block when using goto-line in a collapsed buffer"
    (save-excursion
      (hs-show-block)))

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
            (hs-show-block))
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
  ;; (add-element-to-lists 'hs-hide-initial-comment-block  pgm-langs)

  (require 'session-config)

  (add-hook 'lotus-enable-desktop-restore-interrupting-feature
            '(lambda ()
              (add-hook 'find-file-hook 'hs-hide-initial-comment-block)
              (add-hook 'find-file-hook '(lambda () (ignore-errors (hs-hide-level 2))))))

  ;; Other Options

  ;; Here is a set of perhaps other useful options you can customize:

  ;; Hide the comments too when you do a 'hs-hide-all'
  (setq hs-hide-comments nil)
  ;; Set whether isearch opens folded comments, code, or both
  ;; where x is code, comments, t (both), or nil (neither)
  (setq hs-isearch-open 't)
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

  ;; Hiding all leaf nodes in a file

  ;; When I work on larger source files, I often have a good idea of the
  ;; spatial arrangement of classes and methods in the file but find no
  ;; suitable facilities in Emacs that allow me to make use of that
  ;; knowledge. What I think is missing is a kind of bird’s eye view of
  ;; the file where only the first line of class, method, or function
  ;; definition is shown. Here is an example illustrating what I have in
  ;; mind using Python:

  ;; class Meter(Canvas):
  ;;     def __init__(self, **opts):
  ;;     def update(self):

  ;; class Controller:
  ;;     def __init__(self):

  ;; def util():

  ;; This is the skeleton of the file but the bodies of the methods and
  ;; functions are hidden. HideShow has ‘hs-hide-level’ which hides all
  ;; code blocks at a specified nesting level but in the example above
  ;; this would either hide the content of the methods or the content of
  ;; the function and the complete content of the classes. What is
  ;; needed is a way to hide blocks that do not contain nested blocks,
  ;; that is, the leaf nodes. Here is code that provides the function
  ;; ‘hs-hide-leafs’ which does exactly that:

  (defun hs-hide-leafs-recursive (minp maxp)
    "Hide blocks below point that do not contain further blocks in
    region (MINP MAXP)."
    (when (hs-find-block-beginning)
      (setq minp (1+ (point)))
      (funcall hs-forward-sexp-func 1)
      (setq maxp (1- (point))))
    (unless hs-allow-nesting
      (hs-discard-overlays minp maxp))
    (goto-char minp)
    (let ((leaf t)
          pos)
      (while (progn
               (forward-comment (buffer-size))
               (and (< (point) maxp)
                    (re-search-forward hs-block-start-regexp maxp t)))
        (setq pos (match-beginning hs-block-start-mdata-select))
        (if (hs-hide-leafs-recursive minp maxp)
            (save-excursion
              (goto-char pos)
              (hs-hide-block-at-point t)))
        (setq leaf nil))
      (goto-char maxp)
      leaf))

  (defun hs-hide-leafs ()
    "Hide all blocks in the buffer that do not contain subordinate
    blocks.  The hook `hs-hide-hook' is run; see `run-hooks'."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (message "Hiding blocks ...")
       (save-excursion
         (goto-char (point-min))
         (hs-hide-leafs-recursive (point-min) (point-max)))
       (message "Hiding blocks ... done"))
     (run-hooks 'hs-hide-hook))))

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
(defvar fold-behave-table)
(defun my-folding-mode-hook ()
  (interactive)
  (setq fold-behave-table
	'((close	fold-hide)
	  (open	        fold-enter)
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

# [//] Local Variables:
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
