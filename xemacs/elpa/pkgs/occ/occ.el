;;; occ.el --- occ               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <>
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

;;

;;; Code:

(provide 'occ)


(require 'switch-buffer-functions)


(require 'occ-main)
(require 'occ-commands)
(require 'occ-test)


;; ;;;###autoload
;; (defun occ-switch-buffer-run-curr-ctx-timer-function (prev next)
;;   (occ-run-curr-ctx-timer))

;;;###autoload
(defun occ-add-after-save-hook-fun-in-org-mode ()
  (add-hook 'after-save-hook 'occ-after-save-hook-fun t t))


;;;###autoload
(defun occ-set-global-tsk-collection-spec (spec)
  (setq
   occ-global-tsk-collection      nil
   occ-global-tsk-collection-spec spec))

(defun occ-reset-global-tsk-collection ()
  (occ-debug :debug "resetting global-tsk-collection")
  (occ-reset-collection-object))


;;;###autoload
(defun occ-initialize ()
  "occ-initialize"
 (setq *occ-tsk-previous-ctx* (occ-make-ctx-at-point)))

;;;###autoload
(defun occ-uninitialize ()
  "occ-uninitialize")

(defmacro occ-find-library-dir (library)
  `(file-name-directory
    (or
     "~/.xemacs/elpa/pkgs/occ/occ.el"
     (locate-library ,library)
     "")))

(defun occ-get-version (here full message)
  "Show the Occ version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (let ((occ-dir (ignore-errors (occ-find-library-dir "occ")))
        (save-load-suffixes (when (boundp 'load-suffixes) load-suffixes))
        (load-suffixes (list ".el"))
        (occ-install-dir
         (ignore-errors (occ-find-library-dir "occ-loaddefs"))))
    (unless (and
             (fboundp 'occ-release)
             (fboundp 'occ-git-version))
      (org-load-noerror-mustsuffix (concat occ-dir "occ-version")))
    (let* ((load-suffixes save-load-suffixes)
           (release (occ-release))
           (git-version (occ-git-version))
           (version (format "Org mode version %s (%s @ %s)"
                            release
                            git-version
                            (if occ-install-dir
                                (if (string= occ-dir occ-install-dir)
                                    occ-install-dir
                                  (concat "mixed installation! "
                                          occ-install-dir
                                          " and "
                                          occ-dir))
                              "org-loaddefs.el can not be found!")))
           (version1 (if full version release)))
      (when here (insert version1))
      ;; (when message (message "%s" version1))
      version1)))

;;;###autoload
(defun occ-reload-lib (uncompiled)
  "Reload all Occ Lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions."
  (require 'loadhist)
  (let* ((occ-dir     (occ-find-library-dir "occ"))
         ;; (contrib-dir (or (occ-find-library-dir "org-contribdir") occ-dir))
         ;; (feature-re "^\\(org\\|ob\\|ox\\)\\(-.*\\)?")
         (feature-re "^\\(occ\\|okk\\)\\(-.*\\)?")
         (remove-re (format "\\`%s\\'"
                            (regexp-opt '("org" "org-loaddefs" "occ-version"))))
         (feats (delete-dups
                 (mapcar 'file-name-sans-extension
                         (mapcar 'file-name-nondirectory
                                 (delq nil
                                       (mapcar 'feature-file
                                               features))))))
         (lfeat (append
                 (sort
                  (setq feats
                        (delq nil (mapcar
                                   (lambda (f)
                                     (if (and (string-match feature-re f)
                                              (not (string-match remove-re f)))
                                         f nil))
                                   feats)))
                  'string-lessp)
                 (list "occ-version" "occ")))
         (load-suffixes (when (boundp 'load-suffixes) load-suffixes))
         (load-suffixes (if uncompiled (reverse load-suffixes) load-suffixes))
         load-uncore load-misses)
    (setq load-misses
          (delq 't
                (mapcar (lambda (f)
                          (or (org-load-noerror-mustsuffix (concat occ-dir f))
                              ;; (and (string= occ-dir contrib-dir)
                              ;;      (org-load-noerror-mustsuffix (concat contrib-dir f)))
                              (and (org-load-noerror-mustsuffix (concat (occ-find-library-dir f) f))
                                   (add-to-list 'load-uncore f 'append)
                                   't)
                              f))
                        lfeat)))
    (when load-uncore
      (occ-message "The following feature%s found in load-path, please check if that's correct:\n%s"
               (if (> (length load-uncore) 1) "s were" " was") load-uncore))
    (if load-misses
        (occ-message "Some error occurred while reloading Org feature%s\n%s\nPlease check *Messages*!\n%s"
                 (if (> (length load-misses) 1) "s" "") load-misses (occ-version nil 'full))
      (occ-message "Successfully reloaded Org\n%s" (occ-version nil 'full)))))


;;; occ.el ends here
