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
                     eproject-ruby
                     eproject-another
                     mk-project ;; https://github.com/mattkeller/mk-project
                     projman ;; http://www.emacswiki.org/emacs/ProjmanMode
                     project-root))


(defun directory-files-and-attributes-only-child (dir &optional full)
  (cddr (directory-files-and-attributes dir full)))

(eval-after-load "project-buffer-mode"
  '(progn

    (deh-require-maybe iproject
      ;; http://www.emacswiki.org/emacs/IProject
      (iproject-key-binding))

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

(deh-require-maybe project-buffer-mode)

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
                     perspective
                     workspaces
                     ide-skel))


(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
     major-mode))

(defun project-buffer-mode-buffer-list ()
  (loop for b in (buffer-list)
       if (eq 'project-buffer-mode (buffer-mode b))
       collect b))

(defun project-buffer-mode-get-projects (pb)
  (if pb
      (with-current-buffer pb
        project-buffer-projects-list)
      (error "no buffer provided.")))




;; (testing
;;  (project-buffer-mode-get-projects (car (project-buffer-mode-buffer-list))))

;; (defmacro with-current-project-mode-buffer (pmb &rest body)
;;   `(with-current-buffer ,pmb
;;      ,body))



;; (defun
;;     (project-buffer-get-project-settings-data ))

;; (project-buffer-get-current-project-name)

(provide 'project-config)
;;; project-config.el ends here
