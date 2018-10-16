;;; org-clock-utils-lotus.el --- copy config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

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

;;; Code:

(require 'org)
(require 'org-timer)
(require 'org-clock)
(require 'startup-hooks)
(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))


(defun org-file-clockable-refile ()
  )

(defun org-file-scheduled-refile ()
  )


;; http://orgmode.org/tmp/worg/org-hacks.html#sec-1_5_1
;; Org Agenda and Task Management
;; Make it easier to set org-agenda-files from multiple directories
;;     Matt Lundin

(defun my-org-list-files (dirs ext)
  "Function to create list of org files in multiple subdirectories.
This can be called to generate a list of files for
org-agenda-files or org-refile-targets.

DIRS is a list of directories.

EXT is a list of the extensions of files to be included."
  (let ((dirs (if (listp dirs)
                  dirs
                (list dirs)))
        (ext (if (listp ext)
                 ext
               (list ext)))
        files)
    (mapc
     (lambda (x)
       (mapc
        (lambda (y)
          (setq files
                (append files
                        (file-expand-wildcards
                         (concat (file-name-as-directory x) "*" y)))))
        ext))
     dirs)
    (mapc
     (lambda (x)
       (when (or (string-match "/.#" x)
                 (string-match "#$" x))
         (setq files (delete x files))))
     files)
    files))

(defvar my-org-agenda-directories '("~/org/")
  "List of directories containing org files.")
(defvar my-org-agenda-extensions '(".org")
  "List of extensions of agenda files")

(setq my-org-agenda-directories '("~/org/" "~/work/"))
(setq my-org-agenda-extensions '(".org" ".ref"))

(defun my-org-set-agenda-files ()
  (interactive)
  (setq org-agenda-files (my-org-list-files
                          my-org-agenda-directories
                          my-org-agenda-extensions)))

(my-org-set-agenda-files)

;; The code above will set your "default" agenda files to all files
;; ending in ".org" and ".ref" in the directories "~/org/"
;; and "~/work/". You can change these values by setting the
;; variables my-org-agenda-extensions and my-org-agenda-directories.
;; The function my-org-agenda-files-by-filetag uses these two
;; variables to determine which files to search for filetags (i.e.,
;; the larger set from which the subset will be drawn).

;; You can also easily use my-org-list-files to "mix and match"
;; directories and extensions to generate different lists of agenda
;; files. Restrict org-agenda-files by filetag

;;     Matt Lundin

;; It is often helpful to limit yourself to a subset of your agenda files. For
;; instance, at work, you might want to see only files related to work (e.g.,
;; bugs, clientA, projectxyz, etc.). The FAQ has helpful information on
;; filtering tasks using filetags and custom agenda commands. These solutions,
;; however, require reapplying a filter each time you call the agenda or writing
;; several new custom agenda commands for each context. Another solution is to
;; use directories for different types of tasks and to change your agenda files
;; with a function that sets org-agenda-files to the appropriate directory. But
;; this relies on hard and static boundaries between files.

;; The following functions allow for a more dynamic approach to selecting a subset of files based on filetags:

(defun my-org-agenda-restrict-files-by-filetag (&optional tag)
  "Restrict org agenda files only to those containing filetag."
  (interactive)
  (let* ((tagslist (my-org-get-all-filetags))
         (ftag (or tag
                   (completing-read "Tag: "
                                    (mapcar 'car tagslist)))))
    (org-agenda-remove-restriction-lock 'noupdate)
    (put 'org-agenda-files 'org-restrict (cdr (assoc ftag tagslist)))
    (setq org-agenda-overriding-restriction 'files)))

(defun my-org-get-all-filetags ()
  "Get list of filetags from all default org-files."
  (let ((files org-agenda-files)
        tagslist x)
    (save-window-excursion
      (while (setq x (pop files))
        (set-buffer (find-file-noselect x))
        (mapc
         (lambda (y)
           (let ((tagfiles (assoc y tagslist)))
             (if tagfiles
                 (setcdr tagfiles (cons x (cdr tagfiles)))
               (add-to-list 'tagslist (list y x)))))
         (my-org-get-filetags)))
      tagslist)))

(defun my-org-get-filetags ()
  "Get list of filetags for current buffer"
  (let ((ftags org-file-tags)
        x)
    (mapcar
     (lambda (x)
       (org-substring-no-properties x))
     ftags)))

;; Calling my-org-agenda-restrict-files-by-filetag results in a prompt with all
;; filetags in your "normal" agenda files. When you select a tag,
;; org-agenda-files will be restricted to only those files containing the
;; filetag. To release the restriction, type C-c C-x >
;; (org-agenda-remove-restriction-lock).



(provide 'org-clock-utils-lotus)
;;; org-clock-utils-lotus.el ends here
