;;; project-config.el --- project management

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

(require 'init-config "~/.emacs.d/init-config.el")

(deh-require-maybe (progn
                     eproject
                     ;; eproject-ruby
                     ;; eproject-another
                     mk-project ;; https://github.com/mattkeller/mk-project
                     projman ;; http://www.emacswiki.org/emacs/ProjmanMode
                     project-root)


  (add-to-list 'desktop-minor-mode-handlers '(eproject-mode . (lambda (desktop-buffer-locals)
                                                                (eproject-maybe-turn-on)))))

(defun directory-files-and-attributes-only-child (dir &optional full)
  (cddr (directory-files-and-attributes dir full)))

(deh-require-maybe fsproject

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
                                platforms))))

(deh-require-maybe (progn
                     ;; perspective   ;; ecb bug
                     workspaces
                     ;; ide-skel
                     ))

(eval-after-load "project-buffer-mode"
  '(progn

    (deh-require-maybe iproject
      ;; http://www.emacswiki.org/emacs/IProject
      (iproject-key-binding)
      (add-hook '*lotus-after-init-hook*
                '(lambda ()
                  (iproject-key-binding))))

    (deh-require-maybe project-buffer-occur
      ;; http://www.emacswiki.org/emacs/ProjectBufferOccur
      (define-key project-buffer-mode-map [(control ?f)] 'project-buffer-occur))

    (deh-require-maybe project-buffer-mode+
      ;; http://www.emacswiki.org/emacs/ProjectBufferModePlus
      (project-buffer-mode-p-setup))

    (deh-require-maybe project-buffer-occur
      ;; http://www.emacswiki.org/emacs/ProjectBufferOccur
      (define-key project-buffer-mode-map [(control ?f)] 'project-buffer-occur))

    (autoload 'find-sln "sln-mode")))

(deh-require-maybe project-buffer-mode

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

  (deh-section "iproject"

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

  (testing

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
     project-buffer-status)))

(deh-require-maybe project-buffer-file
  (pbm-file-enable))








(provide 'project-config)
;;; project-config.el ends here
