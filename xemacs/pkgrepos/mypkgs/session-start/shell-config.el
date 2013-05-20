;;; shell-config.el --- shell

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@merunetworks.com>
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


(deh-require-maybe oneliner
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


  (defun cd-tramp-absolute (dir &optional base-directory)
    (let* ((tramp-prefix "\\`/[^/]+[@:][^:/]+:")
           (base-directory (or base-directory default-directory))
           (prefix (if (string-match tramp-prefix base-directory)
                       (match-string 0 base-directory)))
           (tdir (concat  prefix dir)))
      (cd-absolute tdir)))

  (require 'utils-config)

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
      (shell-dirstack-message))))

;; checkout: http://snarfed.org/why_i_run_shells_inside_emacs




(provide 'shell-config)
;;; shell-config.el ends here
