;;; shell-config.el --- shell

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords:

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

(require 'tree)
(eval-when-compile '(require 'tree))

(deh-require-maybe oneliner

  (defvar buf nil)

  (load-library "oneliner")
  (require 'utils-config)
  (require 'tramp-config)

  (setq oneliner-temp-dir-name (if (getenv "TMPDIR")
                                   (expand-file-name (getenv "TMPDIR"))
                                   (expand-file-name "temp" (getenv "HOME"))
                                   ;; (error "test")
                                   )
        oneliner-debug t
        oneliner-shell-type 'zsh
        oneliner-sync-default-directory-after-prompt t)


  (defadvice oneliner (around shell-call-shell-file-name activate)
    ;; I can not make (env ESELL) to zsh, becasue of tramp
    ;; see in tramp-config.el
    ;; oneliner choose shell-type by shell-file-name
    "make (shell) to prefer shell-file-name"
    (let ((explicit-shell-file-name shell-file-name))
      ad-do-it))

  (defadvice oneliner-for-dir (around shell-call-shell-file-name activate)
    ;; I can not make (env ESELL) to zsh, becasue of tramp
    ;; see in tramp-config.el
    ;; oneliner choose shell-type by shell-file-name
    "make (shell) to prefer shell-file-name"
    (let ((explicit-shell-file-name shell-file-name))
      ad-do-it))


  ;; (defun cd-tramp-absolute (dir &optional base-directory)
  ;;   (let* ((tramp-prefix "\\`/[^/]+[@:][^:/]+:")
  ;;          (base-directory (or base-directory default-directory))
  ;;          (prefix (if (string-match tramp-prefix base-directory)
  ;;                      (match-string 0 base-directory)))
  ;;          (dir (concat  prefix dir)))
  ;;     (cd-absolute dir)))


  (defun cd-tramp-absolute (dir &optional base-directory)
    (let* ((base-directory (or base-directory default-directory))
           (prefix (tramp-file-prefix base-directory))
           (dir (concat  prefix dir)))
      (cd-absolute dir)))



  (defun get-tramp-env (variable)
    (with-temp-buffer
      (process-file "bash" nil t nil "-c" (concat "echo $" variable) )
      (trim-string (buffer-string))))


  (defun shell-process-cd (arg) ;; redefining shell.el.gz function
    (let ((new-dir (cond ((zerop (length arg)) (concat comint-file-name-prefix
                                                       (get-tramp-env "HOME")))
                         ((string-equal "-" arg) shell-last-dir)
                         (t (shell-prefixed-directory-name arg)))))
      (setq shell-last-dir default-directory)
      (shell-cd new-dir)
      (shell-dirstack-message)))


  (defun oneliner-tramp-send-cd (arg &optional dir)
    "Change directory of *Oneliner shell* to current buffer's `default-directory'."
    (interactive "p")
    (message "Hello")
    (let ((curdir (or dir default-directory)))
      (oneliner-invisible-command-exec
       (concat "cd "
               (file-name-localname curdir)))
      (when (called-interactively-p 'any) ;;(interactive-p)
        (message "Send to %s buffer 'cd %s'" (buffer-name oneliner-shell-buffer)
                 curdir))))


  (defvar oneliners-list nil "Multiple oneliners")

  ;; (defadvice tramp-open-connection-setup-interactive-shell
  ;;     (after start-oneliner last (p vec) activate)
  ;;   (let ((prefix (tramp-connection-prefix vec)))
  ;;     (unless (member prefix oneliners-list)
  ;;       (push prefix oneliners-list)
  ;;       (let ((oneliner-suffix prefix))
  ;;         (oneliner)))))

  ;; (defadvice tramp-open-connection-setup-interactive-shell
  ;;     (after start-oneliner last (p vec) activate)
  ;;   (save-window-excursion
  ;;     ;; check if save-excrusion is required.
  ;;     (oneliner-for-dir
  ;;      (file-name-directory
  ;;       (tramp-connection-file vec)))))


(when nil
  (defadvice tramp-open-connection-setup-interactive-shell
      (after start-oneliner last (p vec) disable)
    ;; see function tramp-sh-file-name-handler in tramp-sh.el
    (unless (and tramp-locked (not tramp-locker)) ;; (or tramp-locked tramp-locker)
      (let* ((prefix (tramp-connection-prefix vec))
             (file (tramp-connection-file vec))
             (dir (if (file-directory-p file)
                      file
                      (file-name-directory file)))
             (onelinerbuf (make-oneliner-shell-buffer-name dir)))
        (save-window-excursion
          (unless (member prefix oneliners-list)
            (push prefix oneliners-list)
            (oneliner-for-dir dir))
          (if (bufferp onelinerbuf)
              (with-current-buffer onelinerbuf
                (oneliner-tramp-send-cd dir))))))
    (message "start-oneliner: tramp-locked %s tramp-locker %s" tramp-locked tramp-locker)
    ad-return-value)

  (add-hook 'lotus-enable-startup-interrupting-feature-hook
            '(lambda ()
              (ad-enable-advice 'tramp-open-connection-setup-interactive-shell 'after 'start-oneliner)
              (ad-update 'tramp-open-connection-setup-interactive-shell)
              (ad-activate 'tramp-open-connection-setup-interactive-shell))
            t)

  (when nil
    (ad-remove-advice 'tramp-open-connection-setup-interactive-shell 'after 'start-oneliner)
    (ad-update 'tramp-open-connection-setup-interactive-shell)
    (ad-activate 'tramp-open-connection-setup-interactive-shell)))
)
;; checkout: http://snarfed.org/why_i_run_shells_inside_emacs


(deh-require-maybe eshell
  )



(provide 'shell-config)
;;; shell-config.el ends here
