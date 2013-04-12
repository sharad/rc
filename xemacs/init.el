;; ensure we elc files.



;; (add-to-list 'load-path "/usr/share/emacs/23.3/lisp/emacs-lisp")
;; (add-to-list 'load-path "/usr/share/emacs/23.3/lisp")

(defvar old-messages-buffer-max-lines 100 "To keep all startup detail.")
(defvar *emacs-in-init* t "Emacs is in init.")
(setq *emacs-in-init* t)

(setq old-messages-buffer-max-lines messages-buffer-max-lines
      messages-buffer-max-lines 2000)

(eval-when-compile
  (require 'cl nil nil))

(load-file "~/.xemacs/macros.el")
(load-file "~/.xemacs/utils.el")
(load-file "~/.xemacs/basic.el")

(add-to-list 'load-path "/usr/local/share/emacs/23.3/site-lisp")

(progn
  (progn                                ;add to loadpath
    (defun package-dir-add-to-loadpath (package-dir)
      (when (file-directory-p package-dir)
        (mapc #'(lambda (path)
                  (add-to-list 'load-path path))
              (directory-files package-dir t "[a-zA-Z]+"))))



    ;; (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/world")
    (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/mypkgs")
    (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/elpa")
    (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/world/misc")
    (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/world/gits")

    (mapc
     '(lambda (dir)
       (add-to-list 'load-path dir))
     `("~/.xemacs/pkgrepos/autoinstalled/auto-install"
       "~/.osetup/info/common/elisp"
       ,(concat "~/.osetup/info/hosts/" (system-name) "/elisp"))))

  (progn                                ;byte compile
    (defun package-dir-byte-compile (package-dir)
      (when (file-directory-p package-dir)
        (mapc #'(lambda (dir)
                  (byte-recompile-directory dir 0))
              (directory-files package-dir t "[a-zA-Z]+"))))

    ;; (package-dir-byte-compile "~/.xemacs/pkgrepos/world")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/mypkgs")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/elpa")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/world/misc")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/world/gits")

    (mapc
     '(lambda (dir)
       (byte-recompile-directory dir 0))
     `("~/.xemacs/pkgrepos/autoinstalled/auto-install"
       "~/.osetup/info/common/elisp"
       ,(concat "~/.osetup/info/hosts/" (system-name) "/elisp")))))

(require 'dot-emacs-helper nil nil)

(when (require 'cl nil) ; a rare necessary use of REQUIRE
  ; http://a-nickels-worth.blogspot.in/2007/11/effective-emacs.html
  (defvar *emacs-load-start* (current-time)))
;;

(require 'general-testing)
(irequire 'common-info)



(when (file-exists-p (setq custom-file "~/.xemacs/custom.el"))
  (load-file custom-file))

(when (file-exists-p (setq custom-override-file "~/.xemacs/hand-custom.el"))
  (load-file custom-override-file))




(when (xrequire 'server)
  (setq server-use-tcp t
        server-name (or (getenv "EMACS_SERVER_NAME") server-name))
  (setq server-host (system-name))
  (if (functionp 'server-running-p)
      (when (not (server-running-p))
	(server-start)))
  (message (concat "SERVER: " server-name))
  (when (server-running-p "general")
    (message (concat "YES SERVER: " server-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load all files present in ~/\.xemacs/session-start\.d directory.
(defconst *work-dir* "~/\.\./paradise")

(require-dir-libs "~/\.xemacs/pkgrepos/mypkgs/session-start")

(with-current-buffer "*Messages*"
  (copy-to-buffer "*emacs-startup-log*" (point-min) (point-max))
  ;; (with-temp-file file body)
  (write-region (point-min) (point-max) "~/.emacs.d/startup.log")
  ;; (write-current-buffer "*emacs-startup-log*"
  ;;                       )
  (setq messages-buffer-max-lines 2000
        ;; old-messages-buffer-max-lines
        ))

(setq *emacs-in-init* nil)              ;how to ensure it will run.
;;end



;; (when (boundp '*emacs-load-start*)
;;   ;; http://a-nickels-worth.blogspot.in/2007/11/effective-emacs.html
;;     (message "My .emacs loaded in %ds"
;;              (destructuring-bind (hi lo ms) (current-time)
;;                (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*))))))


;; (message "My .emacs loaded in %s" (emacs-init-time))


;; (sharad/enable-startup-inperrupting-feature)
