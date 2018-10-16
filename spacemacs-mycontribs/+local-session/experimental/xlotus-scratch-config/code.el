;;
;; scratch.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Fri Jun 11 17:18:56 2010 Sharad Pratap
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


;; (defn (key-binding key t))

;; (setq x (read-key-sequence "safds: "))
;; (funcall (key-binding x t))



(defun dividebymb ()
  "divide by 1 mb"
  (interactive)
  (let* ((number (number-at-point))
         (res (/ number 1048576)))
    (when res
        (forward-word)
        (insert ?\  (number-to-string res) "MB"))))

(deh-section "scratch mode"

  (require 'autoinsert+)

  (defvar mjmode-scratch-directory (auto-config-dir "scratchs" t))

  (defun mjmode-scratch (&optional new name mjmode)
    (interactive
     (let* ((new current-prefix-arg)
            (mjmode (ido-completing-read "mode: "
                                         (all-completions "" obarray '(lambda (i)
                                                                       (and
                                                                        (commandp i)
                                                                        (string-match "[.-]*-mode" (symbol-name i)))))
                                         nil
                                         t
                                         (if major-mode (symbol-name major-mode))))
            (name (read-from-minibuffer
                   "buffer: "
                   (concat "*"
                           (if (string-match "\\(.+[.-]+.+\\)-mode" mjmode)
                               (match-string 1 mjmode)
                               mjmode)
                           "-scratch*"))))
       (list new name mjmode)))

    (let* (;; (name (if (and new (get-buffer name))
           ;;           (replace-regexp-in-string "\\**\\'" "" name)
           ;;           name))
           (basename (replace-regexp-in-string "\\`\\**" ""
                                               (replace-regexp-in-string "\\**\\'" "" name)))
           (file (expand-file-name basename mjmode-scratch-directory))
           (buf (or
                 (get-buffer name)
                 (find-file-noselect-1 (get-buffer-create name) file nil nil file
                                       (if (file-exists-p file)
                                           (nthcdr 10 (file-attributes file))))))
           (prev-default-directory default-directory))
      (with-current-buffer buf
        (setq default-directory prev-default-directory)
        (if buffer-file-name
            (unless (string-equal buffer-file-name file)
              (error "different file %s is set for buffer %s." buffer-file-name buf))
          (setq buffer-file-name file)))
      ;; (switch-to-buffer buf t)
      (switch-to-buffer buf)
      (funcall (intern mjmode))
      (auto-insert+)))

  (defalias 'scratch 'mjmode-scratch)

  (defvar mjmode-scratch-mode-map
    (let ((map (make-sparse-keymap)))
      ;; (set-keymap-parent map lisp-mode-map)
      ;; (set-keymap-parent map mjmode-mode-map)
      map))

  ;; (defun mjmode-scratch ()
  ;;   (interactive)
  ;;   (mjmode-switch-to-scratch-buffer))

  (defun mjmode-switch-to-scratch-buffer ()
    (set-buffer (slime-scratch-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t)))

  (defvar mjmode-scratch-file nil)

  (defun mjmode-scratch-buffer ()
    "Return the scratch buffer, create it if necessary."
    (or (get-buffer (slime-buffer-name :scratch))
        (with-current-buffer (if slime-scratch-file
                                 (find-file slime-scratch-file)
                                 (get-buffer-create (slime-buffer-name :scratch)))
          (rename-buffer (slime-buffer-name :scratch))
          (lisp-mode)
          (use-local-map slime-scratch-mode-map)
          (slime-mode t)
          (current-buffer))))

  ;; (slime-define-keys slime-scratch-mode-map
  ;;   ("\C-j" 'slime-eval-print-last-expression))


  (defun run-current-buffer (&optional arg)
    "Runs the compilation of the current file.
Assumes it has the same name, but without an extension"
    (interactive "P")
    (let* ((file
            (or ;; buffer-file-name
                (let ((tf (make-temp-file (symbol-name major-mode) nil ".c")))
                  (write-region nil nil tf)
                  tf)))
           (default-directory (file-name-directory file))
           (exe (file-name-sans-extension file))
           (output (concat (symbol-name major-mode) "-output")))
      (when (compile (concat "g++ -std=gnu++0x -o " exe " " file))
        (sleep-for 0 1000)
        (if (file-exists-p exe)
            (progn
              ;; (unless buffer-file-name (delete-file file))
              (delete-file file)
              (shell-command exe output output)
              (if (bufferp (get-buffer output))
                  (switch-to-buffer-other-window output))
              (unless arg (delete-file exe)))
            (message "file %s not got created" exe))))))

(provide 'scratch-config)
