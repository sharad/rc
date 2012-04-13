
(defvar use-slime-config 'ubuntu "Config")

(defvar quicklisp-path (if (string-equal (system-name) "spratap")
                         "/atlantic/home/s/res/share/common-lisp/quicklisp"
                         "/all/res/share/common-lisp/quicklisp")
  "Quicklisp path.")

(defvar
 available-slime-configs
 `(
   ,(if (file-directory-p quicklisp-path)
        `(quicklisp .
                    ((slime-path . ,(expand-file-name
                                     (concat (car (last (directory-files
                                                         (concat quicklisp-path "/dists/quicklisp/software" ) t "slime[a-zA-Z0-9-]+cvs"))) "/")))
                     (slime-backend . "swank-loader.lisp"))))
   (ubuntu .
           ((slime-path . ,(expand-file-name
                            (concat "/usr/share/"
                                    (if (boundp 'flavor) (symbol-name flavor)
                                        "emacs")
                                    "/site-lisp/slime")))
            (slime-backend . "/usr/share/common-lisp/source/slime/swank-loader.lisp"))))
  "Available")

(setf use-slime-config 'ubuntu)

(testing
 (setq b 1)
 (assoc 'a `(nil (a . ,b))))

(defun get-slime-config-ele-from-configs (config ele)
  "Get slime config."
  (cdr (assoc ele (cdr (assoc config available-slime-configs)))))

(defun get-slime-config (ele)
  "Get slime confiog."
  (get-slime-config-ele-from-configs use-slime-config ele))

(defun display-slime-config ()
  "Display"
  (interactive)
  (dolist (v (list slime-path slime-backend)
           (format "%s: %s" (symbol-name v) v))))

(when t
    (setq inferior-lisp-program "sbcl"
      ;; I am setting up port other than 4005 so stumpwm will not be interfered with
      ;; other slime invokation.
      slime-port 4005
      slime-backend (get-slime-config 'slime-backend)
      slime-path (get-slime-config 'slime-path)
      swank-loader-full-path
      (if (file-name-absolute-p slime-backend)
          slime-backend
          (expand-file-name slime-backend slime-path))))

(when t

  (setq w3m-command "/usr/bin/w3m" ; Ubuntu or Debian version
        cltl2-url "file:///usr/share/doc/cltl/clm/node1.html"
        hyperspec-prog (expand-file-name "hyperspec.el" slime-path)
        ;; hyperspec-path "/usr/share/doc/HyperSpec/")
        hyperspec-path "/usr/share/doc/hyperspec/")

  ;; settings for Common Lisp development:
  (setq lisp-indent-function 'common-lisp-indent-function
                                        ;slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        slime-startup-animation nil
        common-lisp-hyperspec-root (concat "file://" hyperspec-path)
        common-lisp-hyperspec-symbol-table (concat hyperspec-path "Data/Map_Sym.txt")
        w3m-default-homepage common-lisp-hyperspec-root
        browse-url-browser-function 'w3m
        w3m-symbol 'w3m-default-symbol
        w3m-key-binding 'info
        w3m-coding-system 'utf-8
        w3m-default-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8

        ;; error in process filter: if: slime-eval-in-emacs disabled for
        ;; security.Set slime-enable-evaluate-in-emacs true to enable it.
        slime-enable-evaluate-in-emacs t))


;;{{
;; Donot compile
;; (byte-recompile-directory slime-path 0)

;; (expand-file-name slime-path)

;; (let ((package-dir (concat "/usr/share/"
;;                            (if (boundp 'flavor) (symbol-name flavor)
;; 			     "emacs")
;;                            "/site-lisp/slime")))
;;   (setq qload-path nil)
;;   (when (file-directory-p package-dir)
;;         (setq qload-path (cons package-dir
;; 	                      (cons (concat package-dir "/contrib")
;; 			            qload-path)))))

;; (setq pl '(a b c))

;; (cons 'x (cons 'z pl))

;; (add-to-list  'pl 'p)

;; pl

(when t
  ;; This is the problem point take care of it well.
  (setq load-path
        (cons (concat (pathname-delete-trailing-/ slime-path) "/contrib") load-path))
  ;; (add-to-list 'load-path (pathname-delete-trailing-/ slime-path))
  )




(deh-section "Slime"
  ;; Contrib packages aren't loaded by default. You have to modify
  ;; your setup a bit so that Emacs knows where to find them and which
  ;; of them to load. Generally, you should call slime-setup with the
  ;; list of package-names that you want to use. For example, a setup
  ;; to load the slime-scratch and slime-editing-commands packages
  ;; looks like:
  (when nil ;;; I do not need it. -thanks.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; quicklisp-slime-helper of qlisp installed
    ;; slime-helper.el installed in "/all/res/share/common-lisp/quicklisp/slime-helper.el"
    ;;
    ;; To use, add this to your ~/.emacs:
    ;;
    ;;   (load (expand-file-name "/all/res/share/common-lisp/quicklisp/slime-helper.el"))
    ;;   ;; Replace "sbcl" with the path to your implementation
    ;;   (setq inferior-lisp-program "sbcl")
    ;;
    ;;
    ;; ("quicklisp-slime-helper")

    ;; CL-USER>  (ql:quickload "quicklisp-slime-helper")
    ;; To load "quicklisp-slime-helper":
    ;;   Load 1 ASDF system:
    ;;     quicklisp-slime-helper
    ;; ; Loading "quicklisp-slime-helper"
    ;; ..................................................
    ;; [package quicklisp-slime-helper]
    ;; slime-helper.el installed in "/all/res/share/common-lisp/quicklisp/slime-helper.el"

    ;; To use, add this to your ~/.emacs:

    ;; uncomment here
    (let ((helper (expand-file-name (concat quicklisp-path "/slime-helper.el"))))
      (if (file-exists-p helper)
          (load helper)))

    ;; Replace "sbcl" with the path to your implementation
    ;; (setq inferior-lisp-program "sbcl")

    ;; ("quicklisp-slime-helper")
    ;; CL-USER>
    )


  ;; (require 'slime)
  ;; next two lines only apply to Autumn 2007/post-2.0 versions of SLIME from CVS:
  ;; (slime-setup)


  ;; After starting SLIME, the commands of both packages should be available.

  ;; The REPL and slime-fancy modules deserve special mention. Many
  ;; users consider the REPL (see REPL) essential while slime-fancy (see
  ;; slime-fancy) loads the REPL and almost all of the popular
  ;; contribs. So, if you aren't sure what to choose start with:

  ;; (dolist (p '(slime-fancy slime-scratch slime-editing-commands slime-repl slime-asdf))
  ;;   (xrequire p))

  ;; (slime-setup '(inferior-slime slime-fancy slime-asdf))


  (when t
    (xrequire 'slime-autoloads))

  ;; (let ((slime-autoloads (concat slime-path "slime-autoloads.elc")))
  ;;   (if (file-exists-p slime-autoloads)
  ;;       (load slime-autoloads)
  ;;       (load "/usr/share/emacs/site-lisp/slime/slime-autoloads.el")))



  (deh-require-maybe 'slime-autoloads
    ;; (slime-setup '(slime-scratch slime-editing-commands))
    ;; (slime-setup '(slime-repl)) ; repl only
    ;; If you like what you see try this:
    ;; almost everything

    ;; producing error
    ;; (slime-setup '(slime-fancy))
    ;; (slime-setup '(slime-fancy slime-scratch slime-editing-commands slime-repl slime-asdf)))

    (when t
      ;; it is done in /etc/emacs/site-start.d/50slime.el
      ;; do not knows doing it second time would create some problem.
          (eval-after-load "slime"
            '(progn
              ;; (slime-setup '(slime-repl inferior-slime slime-fancy slime-scratch slime-editing-commands slime-asdf))
              (slime-setup '(slime-fancy))
              ;; (slime-setup '(slime-fancy slime-scratch slime-editing-commands slime-repl slime-asdf))
              ;; (slime-setup '(slime-fancy slime-asdf slime-banner))
              ;; (slime-setup '(slime-asdf slime-banner))
              ;; (xrequire 'slime-repl)
               (setq slime-complete-symbol*-fancy t)
               (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol))))

    ;; (when (functionp 'slime-bind-editing-commands)
    ;;   (add-hook 'slime-mode-hook 'slime-bind-editing-commands))

    ))

;;}}


;;{{
;; C-c C-d h slime-hyperspec-lookup SYMBOL-NAME
;; M-C-x evaluates current form via SLIME
;; M-C-q re-indents form following cursor
;; C-c C-d h on symbol or function retrieves HyperSpec entry
;; M-x describe-mode for more
;; http://cl-cookbook.sourceforge.net/emacs-ide.html

(when nil
  (global-set-key [(shift f1)]
		'(lambda ()
		   (interactive)
		   (let ((browse-url-browser-function
                          'browse-url-w3)
                         (common-lisp-hyperspec-root
                          (concat "file://" hyperspec-path))
                         (common-lisp-hyperspec-symbol-table
                          (concat common-lisp-hyperspec-root
                                  "Data/Map_Sym.txt"))
                         (hyperspec-prog (expand-file-name "hyperspec.el" slime-path)))
		     (load-library hyperspec-prog)
		     (common-lisp-hyperspec
                      (thing-at-point 'symbol))))))


;;}}



(defun slime-connect-ignore-version (&optional host port)
  (interactive)
  (let ((host (or host "localhost"))
        (port (or port 4005))
        (sharad-slime-ignore-version t))
    (setq sharad-slime-ignore-version t)
    (slime-connect host port)))


(user-provide 'slime)


