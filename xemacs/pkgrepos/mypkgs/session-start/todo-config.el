;;; todo-config.el --- dev config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad>
;; Keywords: internal, internal

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

(deh-section "todo"
  ; http://stackoverflow.com/a/2242801/341107
  ;;This command will do something like you want.

  (defun annotate-todo ()
    "put fringe marker on TODO: lines in the curent buffer"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "TODO:" nil t)
        (let ((overlay (make-overlay (- (point) 5) (point))))
          (overlay-put overlay 'before-string (propertize "A"
                                                          'display '(left-fringe right-triangle)))))))

  ;; You can customize the bitmap as desired.

  ;; To get this to apply to all files, you could add it to the 'find-file-hooks

  (add-hook 'find-file-hooks 'annotate-todo))

;;{{
;; http://emacs-fu.blogspot.in/2008/12/highlighting-todo-fixme-and-friends.html
 (lambda ()
   (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
;;}}


;;{{http://emacswiki.org/emacs/AlessandroPiras#toc9
(require 'fixme-mode)
(defun get-same-mode-buffers ()
  "Get all the buffers having the same Major mode as the current
buffer"
  (interactive)
  (remove-if-not (lambda (buf)
		   (eql (with-current-buffer buf major-mode)
			major-mode))
		 (buffer-list)))

(defun fixme-occur ()
  "Finds all the occurrences of the fixme keywords in all buffers
having the same major mode as the current buffer"
  (interactive)
  (multi-occur (get-same-mode-buffers)
	       fixme-keyword-re-string))
(fixme-mode 1)
;;}}


(deh-require-maybe fixme-mode
  ;; Just do as hi-lock; add this to the end of fic-mode:

  (font-lock-fontify-buffer)

  ;; And I suggest removing the lighter. It is not very important to
  ;; show it and there is a lack of room in the modeline.

  ;; This highlighting of FIXMEs etc. can be done with
  ;; WcheckMode. Here’s an example configuration:

  (defvar my-highlight-words
    '("FIXME" "TODO" "BUG"))

  ;; Ensure that the variable exists.
  (defvar wcheck-language-data nil)

  (push '("FIXME"
          (program . (lambda (strings)
                       (let (found)
                         (dolist (word my-highlight-words found)
                           (when (member word strings)
                             (push word found))))))
          (face . highlight)
          (read-or-skip-faces
           (nil)))
        wcheck-language-data)

  ;; Now ‘wcheck-change-language’ to FIXME and turn on ‘wcheck-mode’
  ;; (a minor mode).

  (deh-require-maybe ficme-mode)
  (deh-require-maybe myfixme)
  (deh-require-maybe rfringe)

  (deh-require-todo wcheck-mode
    ()
    (message "elpa pkg wcheck-mode"))

  (unless (locate-library "soap-client")
    (load-file "/usr/share/emacs/24.2/lisp/net/soap-client.elc"))
  (deh-require-maybe org-jira)

  (deh-require-maybe todostack
    ;; beautiful
    (setq todostack-save-file (auto-config-file "todostacksave/todostacksave.el"))
    (add-hook 'kill-emacs-hook        'todostack-save)
    (add-hook 'emacs-startup-hook     'todostack-load)
    (add-hook 'todostack-post-op-hook 'todostack-save))

  (deh-require-maybe todo-mode
    ;; famous
    ;; https://groups.google.com/forum/?fromgroups=#!msg/gnu.emacs.sources/7v7Wlnocr8o/bSUKTMEdL4QJ

    (setq
     todo-file-do   (auto-config-file "todo-mode/todo-do")
     todo-file-done (auto-config-file "todo-mode/todo-done")))

  (deh-require-maybe sidebrain
    ;;http://www.emacswiki.org/emacs/SideBrain
    ;;http://sidebrain.sourceforge.net/manual/index.html
    ;; (trace-function #'sidebrain-read-todo-from-comments)
    ;; it is in dev-config.el
    )

  (deh-require-maybe todoo
    ;; part of emacs-goodies-el
    ;; http://www.emacswiki.org/emacs/ToDoo
    (setq todoo-file-name (auto-config-file "todoo/todo"))
    (defun todoo-or-close-todoo()
      (interactive)
      (if (eq major-mode 'todoo-mode)
          (call-interactively 'todoo-save-and-exit)
          (call-interactively 'todoo))))

  (deh-require-maybe taskjuggler-mode
    ;; http://www.skamphausen.de/cgi-bin/ska/taskjuggler-mode
    ;; http://www.emacswiki.org/emacs/Taskjuggler
    )

  (deh-section "todo in dir"
    ;; http://www.datenterrorist.de/index.php?itemid=1437
    (defun grep-todos-in-dir (dir &optional not-recursive)
      "Grep recursively for TODO comments in the given directory"
      (interactive "Ddirectory:")
      (let ((recur "-r"))
        (if not-recursive
            (setq recur ""))
        (grep (concat "grep -nH -I " recur " -E \"[\\#\\/\\-\\;\\*]\s*TODO|FIXME|XXX:?\" " dir " 2>/dev/null")))
      (enlarge-window 7))

    (global-set-key-if-unbind [f4] 'grep-todos-in-dir)))


(provide 'todo-config)
;;; dev-config.el ends here
