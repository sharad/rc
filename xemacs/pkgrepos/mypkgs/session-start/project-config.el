;;; project-config.el --- project management

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


(deh-require-maybe (progn
                     eproject
                     ;; eproject-ruby
                     ;; eproject-another
                     mk-project ;; https://github.com/mattkeller/mk-project
                     projman ;; http://www.emacswiki.org/emacs/ProjmanMode
                     project-root)


  (add-to-list 'desktop-minor-mode-handlers '(eproject-mode . (lambda (desktop-buffer-locals)
                                                                (eproject-maybe-turn-on))))



  )

(defun directory-files-and-attributes-only-child (dir &optional full)
  (cddr (directory-files-and-attributes dir full)))

(eval-after-load "project-buffer-mode"
  '(progn

    (deh-require-maybe iproject
      ;; http://www.emacswiki.org/emacs/IProject
      (iproject-key-binding)
      (add-hook '*sharad/after-init-hook*
                '(lambda ()
                  (iproject-key-binding)))
      )

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

  )

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



(defvar project-buffer-current-buf-project nil "current-project-buffer")

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
     major-mode))

(defun project-buffer-mode-buffer-list ()
  (loop for b in (buffer-list)
       if (eq 'project-buffer-mode (buffer-mode b))
       collect b))


(defun project-buffer-select-pbuffer ()
  (let* ((pblist (project-buffer-mode-buffer-list))
          (pb
           (cond
             ((null pblist) (error "No project buffer exists.") nil)
             ((eq (length pblist) 1) (car pblist))
             (t (ido-completing-read "project buffer: " (mapcar #'buffer-name pblist))))))

    pb))

(defun project-buffer-mode-get-projects (pb)
  (if pb
      (with-current-buffer pb
        project-buffer-projects-list)
      (error "no buffer provided.")))

(defun project-select (&optional pb)
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


(defun project-buffer-set-master-project-no-status (&optional pb)
  (interactive
   (let* ((pb (project-buffer-select-pbuffer)))
     (list pb)))
  (if pb
      (let ((project (project-select pb)))
        (if (with-project-buffer pb
              (project-buffer-set-master-project project-buffer-status project))
            (setq project-buffer-current-buf-project (cons pb project))))
      (error "no buffer provided.")))


(defun project-buffer-jump-to-project (&optional force)
  (interactive "P")
  (unless (and (or force (null project-buffer-current-buf-project))
               (null
                (project-buffer-set-master-project-no-status
                 (project-buffer-select-pbuffer))))
      (switch-to-buffer (car project-buffer-current-buf-project))))

(when nil

 (project-buffer-mode-get-projects (car (project-buffer-mode-buffer-list)))

 (defmacro with-current-project-mode-buffer (pmb &rest body)
   `(with-current-buffer ,pmb
      ,body))



 (defun
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
   project-buffer-status))



(deh-section "pbmode"

  (defun iprj-get-buffer (file)
    (let* ((filename file)
           (buffer-name (concat "ipb:" (file-name-sans-extension
                                        (file-name-nondirectory filename))))
           (pmbuff
            (if (file-exists-p filename)
                (or
                 (let ((list (buffer-list)) found)
                   (while (and (not found) list)
                     (with-current-buffer (car list)
                       (if (and
                            filename
                            (file-truename filename)
                            project-buffer-file-name
                            (or
                             (string= project-buffer-file-name (file-truename filename))
                             (string= project-buffer-file-name filename)))
                           (setq found (car list))))
                     (setq list (cdr list)))
                   found)
                 (let ((buffer (generate-new-buffer buffer-name)))
                   (with-current-buffer buffer
                     (project-buffer-mode t)
                     (project-buffer-raw-load filename t t)
                     (setq project-buffer-file-name filename))
                   buffer))

                (let ((buffer (generate-new-buffer buffer-name)))
                  (with-current-buffer buffer
                    (cd (ido-read-directory-name "Root Folder: "))
                    (project-buffer-mode)
                    ;; local variables:
                    (make-local-variable 'iproject-last-project-type-choosen)
                    (make-local-variable 'iproject-last-filter-type-choosen)
                    (make-local-variable 'iproject-last-file-filter-query-mode-choosen)
                    (make-local-variable 'iproject-last-file-filter-regexp-choosen)
                    (make-local-variable 'iproject-last-file-extension-list-choosen)
                    (make-local-variable 'iproject-platform-list)
                    (make-local-variable 'iproject-build-configuration-list)
                    ;; register the local variable to be saved:
                    (add-to-list 'project-buffer-locals-to-save 'iproject-last-project-type-choosen)
                    (add-to-list 'project-buffer-locals-to-save 'iproject-last-filter-type-choosen)
                    (add-to-list 'project-buffer-locals-to-save 'iproject-last-file-filter-query-mode-choosen)
                    (add-to-list 'project-buffer-locals-to-save 'iproject-last-file-filter-regexp-choosen)
                    (add-to-list 'project-buffer-locals-to-save 'iproject-last-file-extension-list-choosen)
                    (add-to-list 'project-buffer-locals-to-save 'iproject-platform-list)
                    (add-to-list 'project-buffer-locals-to-save 'iproject-build-configuration-list)
                    ;; ask for the platform list:
                    (setq iproject-platform-list            (split-string (read-from-minibuffer "Enter the list of platforms separated by spaces: "
                                                                                                (if iproject-platforms-history (car iproject-platforms-history) (format "%s" system-type))
                                                                                                nil nil 'iproject-platforms-history)))
                    (setq iproject-build-configuration-list (split-string (read-from-minibuffer "Enter the list of build configurations separated by spaces: "
                                                                                                (if iproject-build-configurations-history (car iproject-build-configurations-history) "release debug")
                                                                                                nil nil 'iproject-build-configurations-history)))
                    ;;
                    (iproject-setup-local-key)
                    (setq project-buffer-file-name filename)
                    (add-hook 'project-buffer-post-load-hook 'iproject-setup-local-key nil t)
                    (add-hook 'project-buffer-action-hook    'iproject-action-handler  nil t)
                    (add-hook 'project-buffer-refresh-hook   'iproject-refresh-handler nil t))
                  buffer))))
      pmbuff))

  (define-derived-mode iprjbuf-mode fundamental-mode
    "Create a iproject buffer named NAME with a `default-directory' set to ROOT-FOLDER."
    (let* ((file-name buffer-file-name)
          (pmbuff (iprj-get-buffer file-name)))
      ;; (run-with-timer  4 nil `(lambda ()
      ;;                           (switch-to-buffer ,pmbuff)
      ;;                           (kill-buffer (get-file-buffer ,file-name))))
      ))


  (add-to-list 'auto-mode-alist '("\\.pb\\'" . iprjbuf-mode)))








(provide 'project-config)
;;; project-config.el ends here
