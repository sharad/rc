;; ensure we elc files.


(defvar old-messages-buffer-max-lines 100 "To keep all startup detail.")
(defvar *emacs-in-init* t "Emacs is in init.")
(defvar user-emacs-directory "~/.emacs.d")
(setq user-emacs-directory "~/.emacs.d")
(setq *emacs-in-init* t)
(add-hook 'after-init-hook
          (lambda ()
            (setq *emacs-in-init* nil)
            (ad-disable-advice 'server-create-window-system-frame 'around 'nocreate-in-init)))
(setq
 old-messages-buffer-max-lines messages-buffer-max-lines
 messages-buffer-max-lines 2000)


(eval-when-compile
  (require 'cl nil nil))

(eval-after-load "server"
  '(progn
    ;; server-auth-dir (auto-config-dir "server" t)
    (defadvice server-create-window-system-frame
     (around nocreate-in-init activate)
     "remove-scratch-buffer"
     (if *emacs-in-init*
         (message "loading init now.")
         ad-do-it))))

(require 'macros-config      "~/.xemacs/pkgrepos/mypkgs/session-start/macros-config.el")
(require 'basic-utils-config "~/.xemacs/pkgrepos/mypkgs/session-start/basic-utils-config.el")

(deh-section "General"

  (deh-section "loadpath"                                ;add to loadpath

    (add-to-list 'load-path "/usr/local/share/emacs/23.3/site-lisp") ;; need it for gtags.el gtags over tramp

    (defun package-dir-add-to-loadpath (package-dir &optional recursive)
      (when (file-directory-p package-dir)
        (mapc
         (if recursive
             (lambda (path)
               (add-to-list 'load-path path)
               (let ((default-directory path))
                 (normal-top-level-add-subdirs-to-load-path)))
             (lambda (path)
               (add-to-list 'load-path path)))
         (remove-if-not
	  'file-directory-p
	  (directory-files package-dir t "[a-zA-Z]+")))))




    (mapc
     '(lambda (dir)
       (add-to-list 'load-path dir t))  ;auto-install at end as they are generally outdated.
     `("~/.xemacs/pkgrepos/autoinstalled/auto-install"
       "~/.osetup/info.d/common/elisp"
       ,(concat "~/.osetup/info.d/hosts/" (system-name) "/elisp"))))

    ;; (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/world")
    (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/mypkgs")
    (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/elpa")
    (dolist (d (remove-if-not
                'file-directory-p
                (directory-files "~/.xemacs/pkgrepos/world/" t "[a-zA-Z]+")))
      (package-dir-add-to-loadpath d t))

  (deh-section "byte-compile"                                ;byte compile
    (defun package-dir-byte-compile (package-dir)
      (when (file-directory-p package-dir)
        (mapc #'(lambda (dir)
                  (ignore-errors (byte-recompile-directory dir 0)))
              (directory-files package-dir t "[a-zA-Z]+"))))

    ;; (package-dir-byte-compile "~/.xemacs/pkgrepos/world")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/mypkgs")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/elpa")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/world/misc")
    (package-dir-byte-compile "~/.xemacs/pkgrepos/world/gits")
    (package-dir-add-to-loadpath "/usr/local/share/emacs/site-lisp")

    (mapc
     '(lambda (dir)
       (byte-recompile-directory dir 0))
     `("~/.xemacs/pkgrepos/autoinstalled/auto-install"
       "~/.osetup/info.d/common/elisp"
       ,(concat "~/.osetup/info.d/hosts/" (system-name) "/elisp")))))


(when (require 'cl nil) ; a rare necessary use of REQUIRE
  ; http://a-nickels-worth.blogspot.in/2007/11/effective-emacs.html
  (defvar *emacs-load-start* (current-time)))

(defconst *work-dir* "~/\.\./paradise")
;;

(require 'dot-emacs-helper nil nil)
(require 'basic-config)
(require 'general-testing)
(require 'common-info nil t)
(require 'auto-load-config)
(require 'macros-config)
(require 'basic-utils-config)


(deh-section "custom setup"
 (defvar custom-override-file "~/.xemacs/hand-custom.el" "Hand Custom elisp")

 (when (file-exists-p (setq custom-file "~/.xemacs/custom.el"))
  (load-file custom-file))

 (when (file-exists-p custom-override-file)
   (load-file custom-override-file)))

(deh-require-maybe server
  (setq
   ;; server-auth-dir (auto-config-dir "server" t)
   server-use-tcp t
   server-name (or (getenv "EMACS_SERVER_NAME") server-name))
  (setq server-host (system-name))
  (if (functionp 'server-running-p)
      (when (not (server-running-p))
        (condition-case e
            (server-start)
          ('error
           (progn
             (message "Error: %s, now trying to run with tcp." e)
             (let ((server-use-tcp nil))
               (setq server-use-tcp nil)
               (server-start)))))))
  (message (concat "SERVER: " server-name))
  (when (server-running-p "general")
    (message (concat "YES SERVER: " server-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load all files present in ~/\.xemacs/session-start\.d directory.


(require-dir-libs "~/\.xemacs/pkgrepos/mypkgs/session-start")

;; (load-file "~/.xemacs/wrapper.el")
(require 'wrappers-config)

(progn
  (put-file-in-rcs (auto-config-file "startup/startup.log"))
  (with-current-buffer "*Messages*"
    (setq messages-buffer-max-lines 2000
          ;; old-messages-buffer-max-lines
          )
    ;; (append-to-buffer "*xxemacs-startup-log*" (point-min) (point-max))
    (copy-to-buffer "*emacs-startup-log*" (point-min) (point-max)))

  ;; (with-current-buffer "*emacs-startup-log*"
  ;;   ;; (with-temp-file file body)
  ;;   (set-buffer-file-coding-system
  ;;    (if (coding-system-p 'utf-8-emacs)
  ;;        'utf-8-emacs
  ;;        'emacs-mule))
  ;;   (write-region (point-min) (point-max) "~/.emacs.d/startup.log" t)
  ;;   (put-file-in-rcs "~/.emacs.d/startup.log"))
  )



;; (redefine-function-remembered 'server-create-window-system-frame)
(setq *emacs-in-init* nil)              ;how to ensure it will run.
(ad-disable-advice 'server-create-window-system-frame 'around 'nocreate-in-init)
;;end



;; (when (boundp '*emacs-load-start*)
;;   ;; http://a-nickels-worth.blogspot.in/2007/11/effective-emacs.html
;;     (message "My .emacs loaded in %ds"
;;              (destructuring-bind (hi lo ms) (current-time)
;;                (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*))))))


;; (message "My .emacs loaded in %s" (emacs-init-time))




;; (sharad/enable-startup-inperrupting-feature)

(message "emacs ~/.xemacs/init.el loaded")

;; (notify "Emacs" "Loaded Completely :)")
