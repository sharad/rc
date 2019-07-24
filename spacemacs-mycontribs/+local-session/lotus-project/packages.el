;;; packages.el --- lotus-project layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-project-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-project/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-project/pre-init-PACKAGE' and/or
;;   `lotus-project/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-projectS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-project-packages
  '(
    ;; (PACKAGE :location local)
    eproject
    ;; eproject-ruby
    ;; eproject-another
    (mk-project :location local) ;; https://github.com/mattkeller/mk-project
    (projman :location local) ;; http://www.emacswiki.org/emacs/ProjmanMode
    project-root
    (fsproject :location local)
    ;; perspective   ;; ecb bug
    (workspaces :location local)
    ;; ide-skel
    (project-buffer-mode :location local)
    (project-buffer-file :location local)
    (iproject :location local)
    (project-buffer-occur :location local)
    (project-buffer-mode+ :location local)
    (sln-mode :location local)
    term-projectile
    )
  "The list of Lisp packages required by the lotus-project layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-project/init-eproject ()
  (use-package eproject
      :defer t
      :config
      (progn
        (use-package desktop
            :defer t
            :config
            (progn
              (add-to-list
               'desktop-minor-mode-handlers
               '(eproject-mode . (lambda (desktop-buffer-locals)
                                   (eproject-maybe-turn-on)))))))))

(defun lotus-project/init-mk-project ()
  (use-package mk-project
      :defer t
      :config
      (progn
        )))

(defun lotus-project/init-projman ()
  (use-package projman
      :defer t
      :config
      (progn
        )))

(defun lotus-project/init-project-root ()
  (use-package project-root
      :defer t
      :config
      (progn
        )))

(defun lotus-project/init-fsproject ()
  (use-package fsproject
      :defer t
      :config
      (progn
        (progn

          ;; How to use it In order to use it, you can either create your own
          ;; command, or call ‘fsproject-create-project’ from your init.el.  I
          ;; haven’t found a satisfied way to create a uniform command for
          ;; this file, that’s why there is none.  Here is an example of a
          ;; command with an implementation of an action handler:

          (defun my-action-handler(action project-name project-path platform configuration)
            "project action handler."
            (let ((make-cmd (cond ((eq action 'build) "")
                                  ((eq action 'clean) "clean")
                                  ((eq action 'run)   "run")
                                  ((eq action 'debug) "debug"))))
              (compile
               (concat "make -j16 -C " (file-name-directory project-path)
                       " -f " (file-name-nondirectory project-path)
                       " " make-cmd))))

          (autoload 'fsproject-create-project "fsproject")

          (defun fsproject-new(root-folder)
            (interactive "sRoot folder: ")
            (let ((regexp-project-name  "[Mm]akefile")
                  (regexp-file-filter   '("\\.cpp$" "\\.h$" "\\.inl$" "\\.mak$" "Makefile"))
                  (ignore-folders       '("build" "docs" "bin"))
                  (pattern-modifier     nil)
                  (build-configurations '("debug" "release"))
                  (platforms            '("Linux")))
              (fsproject-create-project root-folder
                                        regexp-project-name
                                        regexp-file-filter
                                        'my-action-handler
                                        ignore-folders
                                        pattern-modifier
                                        build-configurations
                                        platforms)))

          ;; And if you want to have only have a source and include folder
          ;; inside each projects:

          (autoload 'fsproject-create-project "fsproject")

          (defun fsproject-new(root-folder)
            (interactive "sRoot folder: ")
            (let ((regexp-project-name  "[Mm]akefile")
                  (regexp-file-filter   '("\\.cpp$" "\\.h$" "\\.inl$" "\\.mak$" "Makefile"))
                  (ignore-folders       '("build" "docs" "bin"))
                  (pattern-modifier     '(("^\\(?:.*/\\)?\\([a-zA-Z0-9_]*\\.cpp\\)$" . "source/\\1")
                                          ("^\\(?:.*/\\)?\\([a-zA-Z0-9_]*\\.\\(?:h\\|inl\\)\\)$" . "include/\\1")))
                  (build-configurations '("debug" "release"))
                  (platforms            '("Linux")))
              (fsproject-create-project root-folder
                                        regexp-project-name
                                        regexp-file-filter
                                        'my-action-handler
                                        ignore-folders
                                        pattern-modifier
                                        build-configurations
                                        platforms)))))))

(defun lotus-project/init-workspaces ()
  (use-package workspaces
      :defer t
      :config
      (progn
        )))

(defun lotus-project/init-project-buffer-mode ()
  (use-package project-buffer-mode
      :defer t
      :config
      (progn
        (progn

          ;; check
          ;; project-buffer-mode-p-go-to-attached-project-buffer
          ;; project-buffer-mode-p-register-project-to-file
          ;; project-buffer-mode-p-link-buffers-to-current-project

          (defvar project-buffer-current-buf-project nil "current-project-buffer")

          (defun buffer-mode (buffer-or-string)
            "Returns the major mode associated with a buffer."
            (with-current-buffer buffer-or-string
              major-mode))

          (defun project-buffer-mode-buffer-list ()
            (loop for b in (buffer-list)
               if (eq 'project-buffer-mode (buffer-mode b))
               collect b))


          (defun project-buffer-select-pbuffer (&optional current)
            (let* ((pblist (project-buffer-mode-buffer-list))
                   (pb
                    (cond
                      ((null pblist)
                       (error "No project buffer exists.") nil)
                      ((and
                        (eq t current)
                        (eq 'project-buffer-mode (buffer-mode (current-buffer))))
                       (current-buffer))
                      ((eq (length pblist) 1)
                       (car pblist))
                      (t
                       (ido-completing-read "project buffer: " (mapcar #'buffer-name pblist))))))
              pb))

          (defun project-buffer-mode-get-projects (pb)
            (if pb
                (with-current-buffer pb
                  project-buffer-projects-list)
                (error "no buffer provided.")))

          (defun project-select (pb)
            (interactive
             (let* ((pb (project-buffer-select-pbuffer)))
               (list pb)))
            (if pb
                (let* ((projects (project-buffer-mode-get-projects pb))
                       (project (cond
                                  ((null projects) (error "No project exists.") nil)
                                  ((eq (length projects) 1)
                                   (car projects))
                                  (t (ido-completing-read "project: " projects)))))
                  project)))


          (defmacro with-project-buffer (pbuf &rest body)
            `(with-timeout (10 (message "Not able to do it."))
               (with-current-buffer ,pbuf
                 (if (progn ,@body) t))))


          (defun project-buffer-set-master-project-no-status (project &optional pb)
            (interactive
             (let* ((pb (project-buffer-select-pbuffer t))
                    (project (project-select pb)))
               (list pb project)))
            (let ((pb (or pb (current-buffer))))
              (if (with-project-buffer pb
                    (project-buffer-set-master-project project-buffer-status project))
                  (setq project-buffer-current-buf-project (cons pb project)))))

          (defun project-buffer-get-master-project ()
            (car project-buffer-master-project))

          (defun project-buffer-jump-to-project (&optional force)
            (interactive "P")
            (unless (and (or force (null project-buffer-current-buf-project))
                         (null
                          (project-buffer-set-master-project-no-status
                           (cdr project-buffer-current-buf-project)
                           (project-buffer-select-pbuffer))))
              (switch-to-buffer (car project-buffer-current-buf-project))))

          (progn ;; "iproject"

            (defun iproject-choose-main-file (project-type)
              (when (nth 1 project-type)
                (let* ((project-main-file nil)
                       (project-filter (nth 1 project-type))
                       (project-predicate (lambda (filename)
                                            (and (not (string-equal filename "./"))
                                                 (not (string-equal filename "../"))
                                                 (or (file-directory-p filename)
                                                     (some '(lambda (item) (string-match item filename)) project-filter))))))
                  (while (or (not project-main-file)
                             (file-directory-p project-main-file)
                             (not (funcall project-predicate project-main-file)))
                    (let ((def-dir (and project-main-file (file-directory-p project-main-file) project-main-file)))
                      (setq project-main-file
                            (read-file-name "Project Main File: " def-dir nil t nil project-predicate))))
                  project-main-file)))

            (defun iproject-choose-root-folder (project-main-file)
              (let ((project-root-folder nil)
                    (def-dir (if project-main-file
                                 (file-name-directory project-main-file)
                                 default-directory)))
                (while (or (not project-root-folder)
                           (= (length project-root-folder) 0))
                  (setq project-root-folder (read-directory-name "File Search - Root Folder: " def-dir def-dir t)))
                (unless (string-equal (substring project-root-folder -1) "/")
                  (setq project-root-folder (concat project-root-folder "/")))
                project-root-folder))

            (defun iproject-add-files-to-project (project &optional root-folder file-filter base-virtual-folder)
              "Add extra files to the current project."
              (interactive)
              (unless project-buffer-status (error "Not in project-buffer buffer"))
              (let ((current-project (or
                                      project
                                      (project-buffer-get-master-project)
                                      (project-buffer-get-current-project-name))))
                (unless current-project (error "No current project found"))
                (when (called-interactively-p 'interactive)
                  ;; Read the root-folder:
                  (unless root-folder
                    (while (or (not root-folder)
                               (= (length root-folder) 0))
                      (setq root-folder (read-directory-name "File Search - Root Folder: " nil nil t)))
                    (unless (string-equal (substring root-folder -1) "/")
                      (setq root-folder (concat root-folder "/"))))
                  ;; Read the file-filter:
                  (unless file-filter
                    (setq file-filter (iproject-choose-file-filter)))
                  ;; Read the base-virtual-path:
                  (unless base-virtual-folder
                    (let* ((def-string (if iproject-last-base-directory-choosen
                                           (concat " [default " (iproject-shorten-string iproject-last-base-directory-choosen 9) "]")
                                           "")))
                      (setq base-virtual-folder (read-from-minibuffer (format "Enter the base directory in the project%s: " def-string)
                                                                      nil nil nil 'iproject-last-base-directory-history)))))

                (let (file-list user-data project-settings)
                  ;; Collect the project's file
                  (setq file-list (iproject-collect-files root-folder (nth 1 file-filter) iproject-ignore-folder))

                  ;; Make sure the base-virtual-folder doesn't start with a '/' and end with one:
                  (when (and (> (length base-virtual-folder) 0)
                             (string-equal (substring base-virtual-folder 0 1) "/"))
                    (setq base-virtual-folder (substring base-virtual-folder 1)))
                  (unless (or (= (length base-virtual-folder) 0)
                              (string-equal (substring base-virtual-folder -1) "/"))
                    (setq base-virtual-folder (concat base-virtual-folder "/")))

                  ;; Update the project settings:
                  (setq project-settings (cons (list base-virtual-folder root-folder (nth 1 file-filter) iproject-ignore-folder)
                                               (project-buffer-get-project-settings-data current-project)))
                  (project-buffer-set-project-settings-data current-project project-settings)

                  ;; Add each individual files to the project:
                  (iproject-add-file-list-to-current-project current-project base-virtual-folder root-folder file-list)))))

          (when nil

            (project-buffer-mode-get-projects (car (project-buffer-mode-buffer-list)))

            (defmacro with-current-project-mode-buffer (pmb &rest body)
              `(with-current-buffer ,pmb
                 ,body))

            (defun test-pjb ()
              (project-buffer-get-project-settings-data ))

            (project-buffer-get-current-project-name)
            (save-excursion
              (with-current-buffer (car (project-buffer-mode-buffer-list))
                (project-buffer-set-master-project-no-status (project-select))))


            (with-current-buffer "*scratch*"
              test)

            (with-current-buffer (car (project-buffer-mode-buffer-list))
              (buffer-local-variables))

            (with-current-buffer (car (project-buffer-mode-buffer-list))
              project-buffer-status)



            (with-current-buffer (car (project-buffer-mode-buffer-list))
              (project-buffer-mode t)
              (project-buffer-set-master-project-no-status "Sipfd Over Ssl"))

            (progn
              (switch-to-buffer (car (project-buffer-mode-buffer-list)))
              (project-buffer-mode t)
              (project-buffer-set-master-project-no-status "Sipfd Over Ssl"))


            (with-current-buffer (car (project-buffer-mode-buffer-list))
              project-buffer-status))))))

(defun lotus-project/init-project-buffer-file ()
  (use-package project-buffer-file
      :defer t
      :config
      (progn
        (progn
          (pbm-file-enable)))))

(defun lotus-project/init-iproject ()
  (use-package iproject
      :defer t
      :config
      (progn
        (progn
          ;; http://www.emacswiki.org/emacs/IProject
          (iproject-key-binding)
          (add-hook '*lotus-after-init-hook*
                    '(lambda ()
                      (iproject-key-binding)))))))

(defun lotus-project/init-project-buffer-occur ()
  (use-package project-buffer-occur
      :defer t
      :config
      (progn
        (progn
          ;; http://www.emacswiki.org/emacs/ProjectBufferOccur
          ))))

(defun lotus-project/init-project-buffer-mode+ ()
  (use-package project-buffer-mode+
      :defer t
      :config
      (progn
        (progn
          ;; http://www.emacswiki.org/emacs/ProjectBufferModePlus
          (project-buffer-mode-p-setup)))))

(defun lotus-project/init-sln-mode ()
  (use-package sln-mode
      :defer t
      :config
      (progn
        (progn
          (autoload 'find-sln "sln-mode")))))

(defun lotus-project/init-term-projectile()
  (use-package term-projectile
    :defer t
    :config
    (progn
      (progn
        ))))

;;; packages.el ends here
