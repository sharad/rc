;;
;; basic.el
;; Login : <s@taj>
;; Started on  Sun Jun  6 11:18:12 2010 Sharad Pratap
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

;;{{{ start: http://lcavwww.epfl.ch/~ridolfi/personal/linuxstuff/.emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			Basic Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;====================================================================
;; MACROS
;; Some macros.


;;{{{ start: http://emacs-fu.blogspot.com/2008/12/using-packages-functions-only-if-they.html

(eval-when-compile
  (require 'cl))
;; If you place the macros somewhere in the beginning of your
;; .emacs, you can use them as follows (just some examples):
;; change cursor color based on mode (insert/overwrite)
(when (require-maybe 'cursor-chg)  ; Load this library
  (change-cursor-mode 1) ; On for overwrite/read-only/input mode
  (toggle-cursor-type-when-idle 1)) ; On when idle
;; and
(when-available 'set-fringe-mode  ; emacs22+
  (set-fringe-mode 2))            ; don't have too much space left of col1
;;}}}




;;;;;;; My loading function ;;;;;;;
(defvar *desuffix* "sharad"
  "User custom elisp feature name suffix
Note it should be a unique name so *desuffix*-feature
alkready should not exist.")

;; (defun delete-suffix (suffix string)
;;   (if (and suffix (string-match (concat "^" suffix "-") string))
;;       (substring string (+ 1 (length suffix)))
;;       string))




;; (eq 'aa (intern "aa"))

;; (user-require 'gnus)

; (intern "asdfsdaf")

;; (defun user-require (feature &optional suffix)
;;   (let ((file (concat (delete-suffix
;;                         (symbol-name suffix)
;;                         (delete-suffix *desuffix* (symbol-name feature))) ".el"))
;;         (load-path '("~/.xemacs/session-start.d" "~/.gnus.d")))
;;     (require feature file)))

(defvar *user-module-loaded* nil "sadfsd")
(defvar *user-load-path* `("~/.xemacs/session-start.d"
                           "~/.gnus.d"
                           "~/.xemacs/secure"
                           "~/.xemacs/info"
                           "~/.osetup/info/common/elisp"
                           ,(concat "~/.osetup/info/hosts/" (system-name) "/elisp")) "sadfsd")

(defun load-dir-files (dir)
  (let (load-file-with-errors)
   (when (file-directory-p dir)
     (byte-recompile-directory dir 0)
     (mapc '(lambda (f)
             (if (not (ignore-errors (load-file f)))
                 (push f load-file-with-errors)))
           (directory-files dir t "^[a-zA-Z0-9-]+\.elc$"))
     (if load-file-with-errors
         (mapc 'load-file
               load-file-with-errors)
         t))))

(defun require-dir-libs (dir)
  (let (load-lib-with-errors)
    (when (file-directory-p dir)
      (byte-recompile-directory dir 0)
      (mapc (lambda (lib)
              (let ((feature (if (string-match "\\(.\+\\)\.el" lib)
                                 (intern (match-string 1 lib)))))
                (if feature
                  (unless
                      (and
                       (message "now loading %s.el" feature)
                       (ignore-errors (require feature)))
                    (push feature load-lib-with-errors)))))
            (directory-files dir nil "^[a-zA-Z0-9-]+\.el$"))
      (if load-lib-with-errors
          (mapc '(lambda (f)
                  (message "now loading file with error %s.el" f)
                  (require f))
                load-lib-with-errors)
          t))))


;; (defun package-dir-setup (package-dir)
;;     (when (file-directory-p package-dir)
;;       (mapc #'(lambda (path)
;;                 (add-to-list 'load-path path))
;;             (directory-files package-dir t "[a-zA-Z]+"))
;;       (mapc #'byte-recompile-directory
;;             (directory-files package-dir t "[a-zA-Z]+"))))


(defun afind-if (fun list) ;; anaphoric
  (let ((result
         (funcall fun (car list))))
    (if result
        result
        (if list (afind-if fun (cdr list))))))

(eval-when-compile

(defun user-find-load-file (filename paths)
  (flet ((delete-trailing-slash (path)
           path)
         (get-latest-file (file)
           (let ((files (mapcar #'(lambda (su)
                                    (if (string-match (concat "." su "$") file)
                                        file
                                        (concat file "." su)))
                                '("el" "elc"))))
             (cond
               ((and
                 (> (length files) 1)
                 (every #'file-exists-p files))
                (reduce #'(lambda (f1 f2)
                            (if (file-newer-than-file-p f1 f2)
                                f1 f2))
                        files))
               ((some #'file-exists-p files) (car (remove-if-not #'file-exists-p files)))
               (t nil)))))
    (afind-if
     #'(lambda (dir)
         (get-latest-file (concat (delete-trailing-slash dir) "/" filename)))
     paths)))

(defun user-require (feature &optional file)
  (let ((file-to-load (let ((file (and file (car (file-expand-wildcards file t)))))
                        (if file
                            file
                            (let ((filename (symbol-name feature)))
                              (user-find-load-file filename *user-load-path*))))))

    (if file-to-load
        (load file-to-load)
        (progn
          (message "no such file to load.")
          nil))))



(defun user-provide (feature)
  (acons feature 1 *user-module-loaded*))

;; (user-require 'plan)

;; (user-find-load-file "plan" *user-load-path*)

(defun get-latest-file (file)
  (let ((files (mapcar #'(lambda (su)
                           (if (string-match (concat "." su "$") file)
                               file
                               (concat file "." su)))
                       '("el" "elc"))))
    (cond
      ((and
        (> (length files) 1)
        (every #'file-exists-p files))
       (reduce #'(lambda (f1 f2)
                   (if (file-newer-than-file-p f1 f2)
                       f1 f2))
               files))
      ((some #'file-exists-p files) (car (remove-if-not #'file-exists-p files)))
      (t nil)))))

;; (get-latest-file "~/.xemacs/session-start.d/plan")

;; (string-match ".el$" "sdfgdsg.el")

;;;;;;; My loading function ;;;;;;;


;; (GNUEmacs
;;  (Xlaunch
;;   (define-key global-map [(delete)]    "\C-d") ))

;; (XEmacs
;; (if (eq window-system 'x)
;;      (global-set-key (read-kbd-macro "DEL") 'delete-char)
;;    (or (global-set-key "[3~" 'delete-char))
;;    ))
;;  ;By default we starting in text mode.
;; (setq initial-major-mode
;;      (lambda ()
;;         (text-mode)
;;         (turn-on-auto-fill)
;; 	(font-lock-mode)
;; 	))

;;}}}

(defun add-element-to-lists (element lists)
  (dolist (list lists)
          (add-hook (intern (concat (symbol-name list) "-mode-hook")) element)))

(defvar pgm-langs
  '(java
    c
    c++
    perl
    lisp
    emacs-lisp
    cperl
    js
    espresso
    ruby
    sh
    python) "Langauge modes.")

(defvar mode-used '(org planner)  "Modes used.")

(setq mode-used (append mode-used pgm-langs))


;;{{ Pathname Utilities
(deh-section "Pathname Utilities"
  (defun  pathname-end-with-/ (path)
    "Check if path name end with /"
    (equal (elt path (- (length path) 1)) ?/))

  (defun pathname-delete-trailing-/ (path)
    (if (pathname-end-with-/ path)
        (pathname-delete-trailing-/ (subseq path 0 (- (length path) 2)))
        path))

  (defun pathname-equal (p1 p2)
    "Pathname equality"
    (apply #'string-equal
           (mapcar #'pathname-delete-trailing-/ p1 p2)))

  ;; (testing
  ;;  (pathname-delete-trailing-/ "/sdfsd/sdgfdg////"))
  )

;;{{
(deh-section "startup-inperrupting-feature"
  (defvar enable-p4-login nil "test")

  (defun sharad/disable-startup-inperrupting-feature ()
    (interactive)
    (setq
     enable-p4-login nil
     tramp-mode nil
     ido-mode nil)
    (deh-featurep epa
      (if (fboundp 'epa-file-disable)
          (epa-file-disable)))
    )

  (defun sharad/enable-startup-inperrupting-feature ()
    (interactive)
    (setq
     enable-p4-login t
     tramp-mode t
     ido-mode 'both)

    (deh-featurep epa
      (if (fboundp 'epa-file-disable)
          (epa-file-enable)))

    (deh-featurep (and light-symbol hilit-chg)
      (add-element-to-lists '(lambda ()
                              (light-symbol-mode 1)
                              (highlight-changes-visible-mode t)
                              (highlight-changes-mode t)) pgm-langs)))


  (defun sharad/enable-startup-inperrupting-feature-in-frame-once (frame)
    (sharad/enable-startup-inperrupting-feature)
    (remove-hook 'after-make-frame-functions #'sharad/enable-startup-inperrupting-feature-in-frame-once))

  (add-hook 'after-make-frame-functions #'sharad/enable-startup-inperrupting-feature-in-frame-once))
;;}}

;;{{
(deh-section "login-session-inperrupting-feature"
  (defun sharad/enable-login-session-inperrupting-feature ()
    (interactive)
    ;; (setenv "DISPLAY" ":1")
    (unless (shell-command-no-output "p4 user -o")
      (shell-command-no-output "zenity --password | p4 login"))
    (update-ssh-agent t))

  (defun sharad/enable-login-session-inperrupting-feature-in-frame-once (frame)
    ;; run and disable.
    (when (< (length (frame-list)) 3)
      (sharad/enable-login-session-inperrupting-feature))
    (remove-hook 'after-make-frame-functions #'sharad/enable-login-session-inperrupting-feature-in-frame-once)
    (message "removed sharad/enable-login-session-inperrupting-feature-in-frame-once"))

  (add-hook 'after-make-frame-functions #'sharad/enable-login-session-inperrupting-feature-in-frame-once)

  (defun sharad/disable-login-session-inperrupting-feature-in-frame (f)
    (when (< (length (frame-list)) 3) ;last frame then add.
      (add-hook 'after-make-frame-functions #'sharad/enable-login-session-inperrupting-feature-in-frame-once)
      (message "added sharad/enable-login-session-inperrupting-feature-in-frame-once")))

  (add-hook 'delete-frame-functions #'sharad/disable-login-session-inperrupting-feature-in-frame))
;;}}


;;{{
;; (deh-section "per frame session"

;;   (defvar spec-id nil "spec id")

;;   (defun set-this-frame-id (frame)
;;     (message "in set-this-frame-id: %s aa" spec-id)
;;     (when spec-id
;;       (message "(frame-parameter frame 'frame-spec-id) to %s" spec-id)
;;       (modify-frame-parameters frame (list (cons 'frame-spec-id spec-id)))
;;       (elscreen-restore spec-id))
;;     (setq spec-id nil))

;;   (add-hook 'after-make-frame-functions #'set-this-frame-id)

;;   (defun save-frame-session (frame)
;;     (message "in save-frame-session:")
;;     (when  (frame-parameter frame 'frame-spec-id)
;;       (message "saved the session for %s"
;;                (frame-parameter frame 'frame-spec-id))
;;       (elscreen-store (frame-parameter frame 'frame-spec-id))))

;;   (add-hook 'delete-frame-functions #'save-frame-session))

;;}}

(defalias 'make-local-hook 'ignore)
