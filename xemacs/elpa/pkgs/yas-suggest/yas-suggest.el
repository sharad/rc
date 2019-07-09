;;; yas-suggest.el --- Emacs Database Interface

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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

;; Commentary:

(provide 'yas-suggest)


(require yasnippet)


;; from http://www.lothar.com/blog/2-emacs/
(defun char-isalpha-p (thechar)
  "Check to see if thechar is a letter"
  (and (or (and (>= thechar ?a)
                (<= thechar ?z))
           (and (>= thechar ?A)
                (<= thechar ?Z)))))

(defun char-isnum-p (thechar)
  "Check to see if thechar is a number"
  (and (>= thechar ?0) (<= thechar ?9)))

(defun char-isalnum-p (thechar)
  (or (char-isalpha-p thechar)
      (char-isnum-p thechar)))


;; /usr/share/emacs23/site-lisp/dictionaries-common/flyspell.el
(defun yas/expandable-at-point ()
  "Return non-nil if a snippet can be expanded here."
  ;; (car (yas/current-key))
  (yas/current-key-1))

(defvar yas-overlays nil)

(make-variable-buffer-local 'yas-overlays)

;; (defun showthiscommand ()
;;   (message "this-command %s" this-command))
;; (remove-hook 'post-command-hook 'showthiscommand)

(defun yas/current-key-1 ()
  "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
  (let ((start (1- (point)))
        (end   (1- (point)))
        (syntaxes yas/key-syntaxes)
        syntax
        done
        templates)
    ;; (message "xstart %d end %d" start end)
    (while (and (not done) syntaxes)
      (setq syntax   (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
        (backward-char)
        (skip-syntax-backward syntax)
        (setq start (point))
        (skip-syntax-forward syntax)
        (setq end (point)))
      ;; (message "start %d end %d" start end)
      (setq templates
            (mapcan #'(lambda (table)
                        (yas/fetch table (buffer-substring-no-properties start end)))
                    (yas/get-snippet-tables)))
      (if templates
          (setq done t)
        (setq start end)))
    (list templates
          start
          end)))

(defun yas/current-key-0 ()
  "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
  (let ((start (point))
        (end   (point))
        (syntaxes yas/key-syntaxes)
        syntax
        done
        templates)
    (while (and (not done) syntaxes)
      (setq syntax   (car syntaxes))
      (setq syntaxes (cdr syntaxes))
      (save-excursion
        (skip-syntax-backward syntax)
        (setq start (point))
        (skip-syntax-forward syntax)
        (setq end (point)))
      ;; (message "start %d stop %d" start end)
      (setq templates
            (mapcan #'(lambda (table)
                        (yas/fetch table (buffer-substring-no-properties start end)))
                    (yas/get-snippet-tables)))
      (if templates
          (setq done t)
        (setq start end)))
    (list templates
          start
          end)))

(defun yas-check-word-p ()
  "Return t when the word at `point' has to be checked.
The answer depends of several criteria.
Mostly we check word delimiters."
  (let ((rr (random 100)))
    (and
     (not (<= (- (point-max) 1) (point-min)))
     (> (current-column) 1)
     ;; (let ((cc (char-after (point)))
     ;;       (pc (char-after (1- (point)))))
     ;;   (and pc (not (char-isalnum-p pc))
     ;;        (or (null cc) (not (char-isalnum-p cc)))))
     (let ((pc (char-after (1- (point)))))
       (and pc
            (char-isalnum-p pc)))
     (let ((con (yas/current-key-0)))
       (when (consp con) (cdr con))))))

(defun yas-post-command-hook ()
  "The `post-command-hook' used by flyspell to check a word on-the-fly."
  (interactive)
  (when yas/minor-mode
    (with-local-quit
      (let ((rr (random 100)))
          ;; (message "Hi1 %d %s" rr yas-overlays)
        (when (> (length yas-overlays) 2)
          ;; (message "Hi2 %d" rr)
          (delete-overlay (pop yas-overlays)))
        ;; (message "Hi3 %d" rr)
        ;; (message "Hi4 %d %d %sxx" rr (length (this-command-keys)) (this-command-keys))
        (when (and
               (> (length (this-command-keys)) 0)
               (or
                (eq (aref (this-command-keys) 0) ?\ )
                (eq this-command #'self-insert-command)
                (eq this-command #'tempo-x-space)))
          ;; (message "Hi5 %d" rr)
          (let ((deactivate-mark nil) ;; Prevent anything we do from affecting the mark.
                (positions (yas-check-word-p)))
            ;; (message "Hi6 %d" rr)
            ;; (message "Hi7 %d (append positions '(highlight highlight)): %s %s" rr positions '(highlight highlight))
            (when positions
              ;; (message "Hi8 %d (append positions '(highlight highlight)): %s %s" rr positions '(highlight highlight))
              (let ((yas-overlay (apply 'make-yas-overlay
                                        (append positions '(highlight highlight)))))
                ;; (message "Hi9 %d" rr)
                ;; (push yas-overlay yas-overlays)
                (setq yas-overlays (nconc yas-overlays (list yas-overlay)))
                ;; (message "%s %d" yas-overlay (length yas-overlays))
                ))))))))

(defun make-yas-overlay (beg end face mouse-face)
  "Allocate an overlay to highlight an incorrect word.
BEG and END specify the range in the buffer of that word.
FACE and MOUSE-FACE specify the `face' and `mouse-face' properties
for the overlay."
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'mouse-face mouse-face)
    (overlay-put overlay 'flyspell-overlay t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'help-echo "mouse-2: yas/expansion at point")
    ;; (overlay-put overlay 'keymap yas-mouse-map)
    ;; (when (eq face 'flyspell-incorrect)
    ;;   (and (stringp flyspell-before-incorrect-word-string)
    ;;        (overlay-put overlay 'before-string
    ;;                     flyspell-before-incorrect-word-string))
    ;;   (and (stringp flyspell-after-incorrect-word-string)
    ;;        (overlay-put overlay 'after-string
    ;;                     flyspell-after-incorrect-word-string)))
    overlay))


(define-minor-mode yas-suggest-mode
  "Prepare for working with collarative office project."
  :init-value 1
  :lighter " yas/sugg" ;; " rl"
  :global nil
  (if yas-suggest-mode
      (add-hook 'post-command-hook (function yas-post-command-hook) t)
    (progn
      (dolist (o yas-overlays)
        (delete-overlay o))
      (setq yas-overlays nil)
      (remove-hook 'post-command-hook (function yas-post-command-hook) t))))

(defun yas-suggest-activate ()
  (yas-suggest-mode 1))

;;;###autoload
(add-element-to-lists 'yas-suggest-activate pgm-langs)
;; (remove-element-from-lists 'yas-suggest-activate pgm-langs)

;; yas-suggest.el ends here
