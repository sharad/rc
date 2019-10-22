;;; misc-utils.el --- Misc utils                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'dot-emacs-helper)
(require 'basic-macros)
(require 'basic-utils) ;; global-set-key-if-unbind





(deh-section "autoconfig"
  (unless user-emacs-directory
    (error "user-emacs-directory is not set"))

  ;; (locate-user-emacs-file)

  (defun auto-config-file (file-path)
    (make-directory
     (if (file-name-directory file-path)
         (expand-file-name
          (file-name-directory file-path)
          (expand-file-name ".cache/autoconfig" user-emacs-directory))
       (expand-file-name ".cache/autoconfig" user-emacs-directory))
     t)
    (expand-file-name file-path (expand-file-name ".cache/autoconfig" user-emacs-directory)))

  (defun simple-no-final-slash (s)
    ;; Remove optional final slash from string S
    (let ((l (1- (length s))))
      (if (and (> l 0) (eq (aref s l) ?/))
          (substring s 0 l)
        s)))

  (defun auto-config-dir (dir-path &optional create-it)
    (unless user-emacs-directory
      (error "user-emacs-directory is not set"))
    (make-directory
     (if (if create-it dir-path (file-name-directory (simple-no-final-slash dir-path)))
         (expand-file-name
          (if create-it dir-path (file-name-directory (simple-no-final-slash dir-path)))
          (expand-file-name "autoconfig" user-emacs-directory))
       (expand-file-name "autoconfig" user-emacs-directory))
     t)
    (expand-file-name dir-path (expand-file-name "autoconfig" user-emacs-directory))))

(deh-featurep pcache
  (setq pcache-directory (auto-config-dir "var/pcache/" t)))

(deh-featurep abbrev
  (setq abbrev-file-name (auto-config-file "abbrev/abbrev_defs")))

;; create an indirect buffer
(defun indirect-buffer ()
  "Edit stuff in this buffer in an indirect buffer.
The indirect buffer can have another major mode."
  (interactive)
  (let ((buffer-name (generate-new-buffer-name "*indirect*")))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))))

;; Here is another function which I started using so often I bound it to
;; a key. Now I can use C-c i to insert the current date.

;; Insertion of Dates.
(deh-section "insert date time"
  (deh-require insert-time-string)
  (defun insert-date-string ()
    "Insert a nicely formated date string."
    (interactive)
    (insert (format-time-string "%Y-%m-%d")))
  ;; org-time-stamp
  )


(if (not running-xemacs)
   (menu-bar-mode -10))

(tool-bar-mode -10)



;; geometry of the opened window, I have a big screen, please adapt to yours.
;; (setq default-frame-alist
;;       '(
;;         (top . 000)
;;         (left . 1000)
;;         (width . 150)
;;         (height . 90)))
;;}}}

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(column-number-mode 1)
(setq font-lock-maximum-decoration     ;; for setting the level of
      '((c-mode . 3) (c++-mode . 3)))  ;; fontification.
; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)
;; (tool-bar-mode 0)                       ;no buttons, thanks
;; (display-time-mode 1)			;not available in XEmacs.


;show white spaces (in "pale turquoise", see "custom-set-faces" below)
;at the end of lines
(setq-default show-trailing-whitespace t)


;; (c-set-style "gnu")
;; (load "~/emacslib/loaddefs.el" t t t )
;; (run-with-idle-timer 5 t 'zone)
;; (run-with-idle-timer 12  t 'toggle-read-only 10)
;; (zone)
;; (run-with-idle-timer 20 t 'vc-toggle-read-only 10)

;;Resuming Emacs with Arguments
;;You can specify ordinary arguments for Emacs when you resume it
;;after a suspension. To prepare for this, put the following code in
;;your `.emacs' file (see section Hooks):
(add-hook 'suspend-hook 'resume-suspend-hook)
;; timestamps rule
;;;Turn on time-stamp updating. Timestamp must be in first 8 lines of file and look like:
;;; Time-stamp: <>
(add-hook 'write-file-hooks 'time-stamp)
;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq time-stamp-format                 ; format of the stamp
;;       ;;use describe-variable for info
;;       "[%f] modified by Sharad on %:a, %:y.%02m.%02d at %02H:%02M:%02S on %s"
;;       time-stamp-active t               ; update timestamps
;;       time-stamp-warn-inactive t)       ; warn if unable

;;For allowing/prohibiting #file and file~
(setq auto-save-list-file-name t)       ;need it

;; The default flyspell behavior is to highlight incorrect words.
;; With no argument, this command toggles Flyspell mode.
;; With a prefix argument ARG, turn Flyspell minor mode on iff ARG is positive.
;; Consider using the `ispell-parser' to check your text.  For instance
;; consider adding:

(deh-require-maybe ispell
 (add-hook 'tex-mode-hook                ;for tex
          (function (lambda () (setq ispell-parser 'tex)))))

(add-hook 'text-mode-hook
          (lambda () (flyspell-mode 1)))
(add-hook 'fundamental-mode-hook
          (lambda () (flyspell-mode 1)))

;; peace
;; (setq ring-bell-function (lambda () (message "bell !!")))
(setq ring-bell-function nil)

;; Make minibuffer larger if there is more to see
(when (functionp 'resize-minibuffer-mode)
  (resize-minibuffer-mode 1))
;; Allow marking of the region using cursor keys and the shift key and
;; other stuff, not highlight selected region.
;;(pc-selection-mode)

;; The following line will do away with the scrollbar! Scrollbars are
;; for sissies, anyway. Seriously, though, this can be usefull if you
;; want to choose on which side of the the frame scrollbars ought to
;; be. Try (set-scroll-bar-mode 'left) and (set-scroll-bar-mode
;; 'right) instead.
;; No scrollbar
(when (functionp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))
;; No beeping
;; (setq visible-bell t)
;; I don't like VB
(setq visible-bell nil)

;; fix missing INSERT key to toggle overwrite-mode on the console
(global-set-key-if-unbind [insertchar] 'overwrite-mode)
;; The following function allows filename completion in the buffer
;; (file name completion in the minibuffer is usually available if you
;; have to enter a file name in the minibuffer).
;; from comint.el: filename completion
(autoload 'comint-dynamic-complete-filename
  "comint" "Complete filenames." t)



;; simple math using calculator
(autoload 'calculator "calculator" "Simple calculator." t)

;; (defun calculator-buffer ()
;;   (if (get-buffer "*caluclator*")
;;       ()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mouse wheel: try it with S and C
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun up-one () (interactive) (scroll-up 1)) ;key set in keymap.el
(defun down-one () (interactive) (scroll-down 1)) ;key set in keymap.el

(unless (featurep 'xemacs)
    (progn
      (global-set-key [S-mouse-4] 'down-one)
      (global-set-key [S-mouse-5] 'up-one)))

(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))

(unless (featurep 'xemacs)
    (progn
      (global-set-key-if-unbind [C-mouse-4] 'down-a-lot)
      (global-set-key-if-unbind [C-mouse-5] 'up-a-lot)))
;;;;;;;

;; MAN PAGE FOR SYS CALLS
(global-set-key-if-unbind  [(f4)]
		 (lambda ()
		   (interactive)
		   (manual-entry (current-word))))

;;customization of major modes
(setq auto-mode-alist
      (append (list
               '("\\.mpl$" . maplev-mode)
               '("\\.cfg$" . python-mode)
               '("\\.py$" . python-mode)
               '("\\.php$" . php-mode)
	      ;; '("\\.crm$" . crm114-mode)
	      ;; '("/etc/apache2/.*\\.\\(conf$\\|load$\\)" . apache-mode)
	      ;; '("\\.r$" . R-mode)
	      ;; '("\\.ahk$"  ahk-mode)
               `("\\.html$" . ,(if (functionp 'html-helper-mode) 'html-helper-mode 'html-mode))
               `("\\.html.tmpl$" . ,(if (functionp 'html-helper-mode) 'html-helper-mode 'html-mode))
               '("Makefile" . makefile-mode)
               )
      auto-mode-alist))

;; remember www.maruko.ca/i18n/
;; (if (not running-xemacs) nil ))

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Terminal-Coding.html
;; (set-keyboard-coding-system nil)

;; (xrequire 'elscreen)

;; ;; From Eric Richmond
;; (if (not running-xemacs)
;;     ( progn (autoload 'server-edit "server" nil t)
;;           (server-edit))
;;   (server-edit))



(when (functionp 'zone-leave-me-alone)
  (zone-leave-me-alone))

(xrequire 'redo)


;; Amazing http://www.emacswiki.org/cgi-bin/wiki?EmacsNiftyTricks
(setq hcz-set-cursor-color-color "")
(setq hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "black"
             (if overwrite-mode "red" "coral"))))
                 ;;"blue"))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)
;; throwing warnnings
;; In hcz-set-cursor-color-according-to-mode:
;; misc.el:234:31:Warning: reference to free variable
;;     `hcz-set-cursor-color-color'
;;     `hcz-set-cursor-color-buffer'
;;     `hcz-set-cursor-color-color'
;;     `hcz-set-cursor-color-buffer'
;;     `whitespace-auto-cleanup'




;; delete trailing save before sving. Is it good :-)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(remove-hook 'before-save-hook 'untabify)
; (remove-hook 'before-save-hook 'delete-trailing-whitespace)

;; To customize the color of the background, use the code
;; below. Command list-colors-display displays all available colors.
;; (set-face-background 'hl-line "IndianRed") ;; using the color name
;; (set-face-background 'hl-line "#8b4513")   ;; using html code
;;}}

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(put 'narrow-to-page 'disabled nil)

(put 'set-goal-column 'disabled nil)

(deh-require-maybe flymake               ;excellent
  (add-hook 'find-file-hook 'flymake-find-file-hook))


;; from http://www.dotemacs.de/dotfiles/DavidJolley.emacs.html
;; Load whitespace.el library. Nukes trailing whitespace from the ends
;; of lines, and deletes excess newlines from the ends of buffers.
;;
;; get it from: http://www.dsmit.com/lisp/
(deh-require-maybe whitespace
  (setq whitespace-auto-cleanup t)
  (when (functionp 'whitespace-global-mode)
    (whitespace-global-mode 1)))



;; see http://cl-cookbook.sourceforge.net/.emacs for common lisp
;;     http://sugarshark.com/elisp/init/lisp.el.html

(if (file-directory-p *work-dir*)
    (cd *work-dir*))


(deh-require-maybe smart-operator)


(deh-require-maybe thingatpt)
(deh-require-maybe thingatpt+)

(when nil
  ;; BUG for new system
 (deh-require-maybe (progn help+ help-fns+ help-macro+ help-mode+)
  )

(deh-require-maybe (progn ido-load-library kill-ring-ido kill-ring-search)
  ))


(deh-section "Close all frame"
  (defun close-all-frames ()
    (interactive)
    (dolist (f (frame-list))
      (delete-frame f t))))

(defun hindi-devanagari-itrans ()
  (interactive)
  (if (string-equal current-input-method "devanagari-itrans")
      (set-input-method nil nil)
      (set-input-method "devanagari-itrans" nil)))


(deh-require-maybe imenu-tree
  (setq imenu-tree-windata '(frame right 0.2 delete))

  )

(when nil
  ;; BUG
  (deh-require-maybe (progn pastie pastbin paste2)
    ))
















(progn ;; deh-section "General"

  (defun general-disable-startup-setting ()
    (setq pabbrev-read-only-error nil)
    (setq
     enable-p4-login nil
     tramp-mode nil
     ido-mode nil)
    (deh-featurep epa
      (if (fboundp 'epa-file-disable)
          (epa-file-disable))))

  (defun general-enable-startup-setting ()
    (setq pabbrev-read-only-error nil)

    (setq enable-p4-login t
          tramp-mode t
          ido-mode 'both)
    (login-to-perforce)
    ;; (update-ssh-agent t) ;; should be called when tramp file accessed. - see how it will work in case lotus-desktop-session-restore.
    ;;test
    (deh-featurep epa
      (if (fboundp 'epa-file-disable)
          (epa-file-enable)))
    (deh-featurep (and light-symbol hilit-chg)
      (add-element-to-lists '(lambda ()
                              (light-symbol-mode 1)
                              (highlight-changes-visible-mode t)
                              (highlight-changes-mode t)) pgm-langs)))

  (add-hook 'lotus-enable-startup-interrupting-feature-hook 'general-enable-startup-setting)
  (add-hook 'lotus-disable-startup-interrupting-feature-hook 'general-disable-startup-setting))













(progn

  (progn

    (progn

      (progn
(deh-section "crontab-mode"
  (autoload 'crontab-mode "crontab-mode.el" "Major mode for editing your crontab file." t)
  (eval-after-load "crontab-mode"
    '(progn
      (add-hook 'crontab-mode-hook
       '(lambda () (setq crontab-apply-after-save t)))
      (defvar crontab-default-file "~/.crontab")
      (defadvice crontab-mode (after set-buffer-file-name activate)
       (when (and crontab-default-file
                  (file-exists-p crontab-default-file)
                  (null buffer-file-name))
         (setq
          buffer-file-name crontab-default-file
          default-directory (file-name-directory buffer-file-name)))))))

(deh-section "centered-cursor-mode"
  (defun centered-cursor-stay-same-pos ()
    (interactive)
    (unless (ad-find-advice 'ccm-first-start 'before 'reset-ccm-vpos)
      (defadvice ccm-first-start (before reset-ccm-vpos (animate) activate)
        (setq ccm-vpos nil) t))
    (setq ccm-vpos-init
          '(or ccm-vpos
            (1- (count-lines (window-start) (point)))))
    (ad-enable-advice 'ccm-first-start 'before 'reset-ccm-vpos)
    (ad-activate #'ccm-first-start)
    (ad-update #'ccm-first-start))

  (defun centered-cursor-unstay-same-pos ()
    (interactive)
    (unless (ad-find-advice 'ccm-first-start 'before 'reset-ccm-vpos)
      (defadvice ccm-first-start (before reset-ccm-vpos (animate) activate)
        (setq ccm-vpos nil) t))
    (setq ccm-vpos-init
          (default-value 'ccm-vpos-init))
    (ad-disable-advice 'ccm-first-start 'before 'reset-ccm-vpos)
    (ad-activate #'ccm-first-start)
    (ad-update #'ccm-first-start))

  (defun centered-cursor-toggle-stay-same-pos ()
    (interactive)
    (if (eq ccm-vpos-init
            (default-value 'ccm-vpos-init))
        (centered-cursor-stay-same-pos)
        (centered-cursor-unstay-same-pos)))

  (defalias 'toggle-ccm-stay 'centered-cursor-toggle-stay-same-pos)
  (defalias 'ccm-stay 'centered-cursor-stay-same-pos)
  (defalias 'ccm-unstay 'centered-cursor-unstay-same-pos)

  (defalias 'ccm 'centered-cursor-mode))
)

(progn
  (progn
   (deh-section "Emacs Code Jump"
     ;; http://lists.gnu.org/archive/html/help-gnu-emacs/2009-09/msg00669.html

     (defun elisp-disassemble (function)
       (interactive (list (function-called-at-point)))
       (disassemble function))

     (defun elisp-pp (sexp)
       (with-output-to-temp-buffer "*Pp Eval Output*"
         (pp sexp)
         (with-current-buffer standard-output
           (emacs-lisp-mode))))

     (defun elisp-macroexpand (form)
       (interactive (list (form-at-point 'sexp)))
       (elisp-pp (macroexpand form)))

     ;; (defun elisp-macroexpand-all (form)
     ;;   (interactive (list (form-at-point 'sexp)))
     ;;   (elisp-pp (cl-macroexpand-all form)))

     (defun elisp-macroexpand-all (form)
       (interactive (list (form-at-point 'sexp)))
       (elisp-pp (macroexpand-all form)))

     (defun elisp-push-point-marker ()
       (require 'etags)
       (cond ((featurep 'xemacs)
              (push-tag-mark))
             (t (ring-insert find-tag-marker-ring (point-marker)))))

     (defun elisp-pop-found-function ()
       (interactive)
       (cond ((featurep 'xemacs) (pop-tag-mark nil))
             (t (pop-tag-mark))))

     (defun elisp-find-definition (name)
       "Jump to the definition of the function (or variable) at point."
       (interactive (list (thing-at-point 'symbol)))
       (cond (name
              (let ((symbol (intern-soft name))
                    (search (lambda (fun sym)
                              (let* ((r (save-excursion (funcall fun sym)))
                                     (buffer (car r))
                                     (point (cdr r)))
                                (cond ((not point)
                                       (error "Found no definition for %s in %s"
                                              name buffer))
                                      (t
                                       (switch-to-buffer buffer)
                                       (goto-char point)
                                       (recenter 1)))))))
                (cond ((fboundp symbol)
                       (elisp-push-point-marker)
                       (funcall search 'find-function-noselect symbol))
                      ((boundp symbol)
                       (elisp-push-point-marker)
                       (funcall search 'find-variable-noselect symbol))
                      (t
                       (message "Symbol not bound: %S" symbol)))))
             (t (message "No symbol at point"))))

     (defun elisp-bytecompile-and-load ()
       (interactive)
       (or buffer-file-name
           (error "The buffer must be saved in a file first"))
       (require 'bytecomp)
       ;; Recompile if file or buffer has changed since last compilation.
       (when  (and (buffer-modified-p)
                   (y-or-n-p (format "save buffer %s first? " (buffer-name))))
         (save-buffer))
       (let ((filename (expand-file-name buffer-file-name)))
         (with-temp-buffer
           (byte-compile-file filename t))))

     (when nil
       (defvar elisp-extra-keys
         '(((kbd "C-c d")   'elisp-disassemble)
           ((kbd "C-c m")   'elisp-macroexpand)
           ((kbd "C-c M")   'elisp-macroexpand-all)
           ((kbd "C-c C-c") 'compile-defun)
           ((kbd "C-c C-k") 'elisp-bytecompile-and-load)
           ((kbd "C-c C-l") 'load-file)
           ((kbd "C-c p")   'pp-eval-last-sexp)
           ((kbd "M-.")     'elisp-find-definition)
           ((kbd "M-,")     'elisp-pop-found-function)
           ((kbd "C-c <")   'list-callers)))

       (dolist (binding elisp-extra-keys)
         (let ((key (eval (car binding))) (val (eval (cadr binding))))
           (define-key emacs-lisp-mode-map key val)
           (define-key lisp-interaction-mode-map key val))))




     (deh-section "Emacs jump"
       ;; with output following C-h v or C-h f I use `jump-to-form' -
       ;; bound here to f12:

       (defun jump-to-form ()
         (interactive)
         (if (featurep 'xemacs)
             (progn
               (forward-char 1)
               (let ((name (symbol-atpt))
                     (file (progn (search-forward "\"" nil t 1)(thing-at-point
                                                                'filename))))
                 (forward-char 1)
                 (help-find-source-or-scroll-up (point))
                 (switch-to-buffer (current-buffer))
                 (kill-new name)
                 (search-forward name)))
             (other-window 1)
             (forward-button 1)
             ;; (find-file (filename-atpt))
             (push-button)))))
   )

  (progn
    (deh-section "New"
      '(deh-require-maybe (progn
                            ipa         ;problem resetting current-idle-time
                            org-pua
                            )
        ;;http://www.emacswiki.org/emacs/InPlaceAnnotations
        )
      (deh-require-maybe alert
        ;;http://www.emacswiki.org/emacs/alert.el
        )
      (deh-require-maybe org-pua
        ;;http://www.emacswiki.org/emacs-es/org-pua.el
        ))
    )
)
)

;; check here
(progn

(deh-require-maybe point-stack
  (global-set-key-if-unbind '[(f5)] 'point-stack-push)
  (global-set-key-if-unbind '[(f6)] 'point-stack-pop)
  (global-set-key-if-unbind '[(f7)] 'point-stack-forward-stack-pop)

  (global-set-key-if-unbind (kbd "s-<right>") 'point-stack-push)
  (global-set-key-if-unbind (kbd "s-<left>") 'point-stack-pop)
  (global-set-key-if-unbind (kbd "s-<left>") 'point-stack-forward-stack-pop))

(deh-require-maybe byte-code-cache)

(when nil                               ;obslete
  (deh-require-maybe lazy-lock
  ;; http://www.opensource.apple.com/source/emacs/emacs-54/emacs/lisp/lazy-lock.el
  ))

(deh-section "misc"
  ;; from: http://www.zerny.dk/emacs/dot-emacs.el
  (defun cycle-windows()
    "cycle the buffer of the windows in cyclic ordering"
    (interactive)
    (mapcar  (lambda(window)
               (let ((next-window-buffer (window-buffer (next-window window 0))))
                 (set-window-buffer (next-window window 0) (window-buffer window))
                 (set-window-buffer window next-window-buffer))) (butlast (window-list nil 0)))))

(deh-section "examine"
  (defun display-expression-value (var)
    (interactive "sexp: ")
    (let ((buf (get-buffer-create "*output*")))
      (if (or t (window-splittable-p (selected-window) ))
          (split-window-sensibly (selected-window)))
      (switch-to-buffer-other-window buf)
      (switch-to-buffer buf)
      ;; (pp (eval (read var)) buf)
      (with-current-buffer buf
        (prin1 (eval (read var)) buf)
        (terpri)))))
)
)

(deh-section "vim"
  ;; viper-mode
  ;; viper-go-away
  (defun open-xvim ()
    (interactive)
    (if (buffer-file-name)
        (let* ((file (file-truename (buffer-file-name)))
               (xcmd
                (if (file-remote-p file)
                    "DISPLAY=localhost:10.0 xterm -e"
                    "xterm -e"))
               (vimcmd "vim")
               (filename (file-name-localname file))
               (pos (format "'+normal %dG%d|'" (current-line) (current-column)))
               (cmd (mapconcat 'identity
                               (list xcmd vimcmd pos filename "&") " ")))
          (message cmd)
          ;; DONE: shell-command-no-output in background
          (shell-command-no-output cmd))
        (error "No file is associated with buffer %s" (current-buffer)))))


(deh-require-maybe ielm
  ;; * Trying from the interactive emacs lisp mode (M-x ielm)
  (defalias 'repl 'ielm)
  (add-hook 'lotus-enable-startup-interrupting-feature-hook
            '(lambda ()
              (let ((default-directory "~/"))
                (ielm)))
            t))


(deh-section "vimrc"
  ;; http://stackoverflow.com/questions/4236808/syntax-highlight-a-vimrc-file-in-emacs
  (define-generic-mode 'vimrc-generic-mode
      '()
    '()
    '(("^[\t ]*:?\\(!\\|ab\\|map\\|unmap\\)[^\r\n\"]*\"[^\r\n\"]*\\(\"[^\r\n\"]*\"[^\r\n\"]*\\)*$"
       (0 font-lock-warning-face))
      ("\\(^\\|[\t ]\\)\\(\".*\\)$"
       (2 font-lock-comment-face))
      ("\"\\([^\n\r\"\\]\\|\\.\\)*\""
       (0 font-lock-string-face)))
    '("/vimrc\\'" "\\.vim\\(rc\\)?\\'")
    '((lambda ()
        (modify-syntax-entry ?\" ".")))
    "Generic mode for Vim configuration files.")
  (add-to-list 'auto-mode-alist '("vimrc\\'" . vimrc-generic-mode)))

(deh-require-maybe wtf
;; wtf.el provides the ability to look up the definitions of popular
;; conversational and computing acronyms.
)


(defun get-string-from-file (filePath)
  ;; http://ergoemacs.org/emacs/elisp_read_file_content.html
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman 〔zzbba…@aol.com〕”. 2010-09-02

(deh-require-maybe ini-mode
  (autoload 'ini-mode "ini-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.ini\\'" . ini-mode)))

(deh-require-maybe any-ini-mode
  (add-to-list 'auto-mode-alist '(".*\\.ini$" . any-ini-mode))
  (add-to-list 'auto-mode-alist '(".*\\.conf$" . any-ini-mode)))

)

(provide 'misc-utils)
;;; misc-utils.el ends here
