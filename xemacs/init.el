;; ensure we elc files.



;; (add-to-list 'load-path "/usr/share/emacs/23.3/lisp/emacs-lisp")
;; (add-to-list 'load-path "/usr/share/emacs/23.3/lisp")

(defvar old-messages-buffer-max-lines 100 "To keep all startup detail.")
(defvar *emacs-in-init* t "Emacs is in init.")
(setq *emacs-in-init* t)
(add-hook 'after-init-hook
          (lambda ()
            (setq *emacs-in-init* nil)
            (ad-disable-advice 'server-create-window-system-frame 'around 'nocreate-in-init)))
(setq old-messages-buffer-max-lines messages-buffer-max-lines
      messages-buffer-max-lines 2000)


(eval-when-compile
  (require 'cl nil nil))

(load-file "~/.xemacs/macros.el")
(load-file "~/.xemacs/utils.el")
(load-file "~/.xemacs/basic.el")

(eval-after-load "server"
  '(defadvice server-create-window-system-frame
      (around nocreate-in-init activate)
    "remove-scratch-buffer"
    (if *emacs-in-init*
        (message "loading init now.")
        ad-do-it)))


(add-to-list 'load-path "/usr/local/share/emacs/23.3/site-lisp") ;; need it for gtags.el gtags over tramp

(deh-section "General"

  (deh-section "loadpath"                                ;add to loadpath

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



    ;; (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/world")
    (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/mypkgs")
    (package-dir-add-to-loadpath "~/.xemacs/pkgrepos/elpa")
    (dolist (d (remove-if-not
                'file-directory-p
                (directory-files "~/.xemacs/pkgrepos/world/" t "[a-zA-Z]+")))
      (package-dir-add-to-loadpath d t))


    (mapc
     '(lambda (dir)
       (add-to-list 'load-path dir))
     `("~/.xemacs/pkgrepos/autoinstalled/auto-install"
       "~/.osetup/info/common/elisp"
       ,(concat "~/.osetup/info/hosts/" (system-name) "/elisp"))))

  (deh-section "byte-compile"                                ;byte compile
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

(progn
  (put-file-in-rcs "~/.emacs.d/startup.log")
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
