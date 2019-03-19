;;; occ.el --- occ               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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
(require 'occ-test)


;;;###autoload
(defun occ-switch-buffer-run-curr-ctx-timer-function (prev next)
  (occ-run-curr-ctx-timer))

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
(defun occ-run-curr-ctx-timer ()
  (interactive)
  (progn
    (setq *occ-last-buff-sel-time* (current-time))
    (when *occ-buff-sel-timer*
      (cancel-timer *occ-buff-sel-timer*)
      (setq *occ-buff-sel-timer* nil))
    (setq *occ-buff-sel-timer*
          ;; distrubing while editing.
          ;; run-with-timer
          (run-with-idle-timer
           (1+ *occ-tsk-current-ctx-time-interval*)
           nil
           'occ-clock-in-curr-ctx-if-not))))

;;;###autoload
(defun occ-insinuate ()
  (interactive)
  (lwarn 'occ :debug "occ-insinuate: begin")
  (message "occ-insinuate: begin")
  (progn
    (setq occ-global-tsk-collection        nil)
    ;; (add-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer t)
    ;; (add-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer t)
    ;; (add-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer t)
    (add-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
    (add-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode))
  (dolist (prop (cl-method-sig-matched-arg '(occ-readprop (`((head ,val) occ-ctx) val)) nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (push propstr org-use-property-inheritance))))
  (org-clock-load) ;; newly added
 (lwarn 'occ :debug "occ-insinuate: finish")
 (message "occ-insinuate: finish"))


;;;###autoload
(defun occ-uninsinuate ()
  (interactive)
  (lwarn 'occ :debug "occ-uninsinuate: begin")
  (message "occ-uninsinuate: begin")
  (progn
    (setq occ-global-tsk-collection            nil)
    ;; (setq buffer-list-update-hook nil)

    ;; (remove-hook 'buffer-list-update-hook     'occ-run-curr-ctx-timer)
    ;; (remove-hook 'elscreen-screen-update-hook 'occ-run-curr-ctx-timer)
    ;; (remove-hook 'elscreen-goto-hook          'occ-run-curr-ctx-timer)
    ;; (remove-hook 'after-save-hook             'occ-after-save-hook-fun t)
    (remove-hook 'switch-buffer-functions #'occ-switch-buffer-run-curr-ctx-timer-function)
    (remove-hook 'org-mode-hook           #'occ-add-after-save-hook-fun-in-org-mode))
  (dolist (prop (cl-method-sig-matched-arg '(occ-readprop (`((head ,val) occ-ctx) val)) nil))
    (let ((propstr
           (upcase (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))))
      (unless (member propstr org-use-property-inheritance)
        (delete propstr org-use-property-inheritance))))
 (lwarn 'occ :debug "occ-insinuate: finish")
 (message "occ-insinuate: finish"))


(defmacro occ-find-library-dir (library)
  `(file-name-directory
    (or
     "~/.xemacs/elpa/pkgs/occ/occ.el"
     (locate-library ,library)
     "")))

(defun occ-version (&optional here full message)
  "Show the Occ version.
Interactively, or when MESSAGE is non-nil, show it in echo area.
With prefix argument, or when HERE is non-nil, insert it at point.
In non-interactive uses, a reduced version string is output unless
FULL is given."
  (interactive (list current-prefix-arg t (not current-prefix-arg)))
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
      (when message (message "%s" version1))
      version1)))

;;;###autoload
(defun occ-reload (&optional uncompiled)
  "Reload all Occ Lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions."
  (interactive "P")
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
      (message "The following feature%s found in load-path, please check if that's correct:\n%s"
               (if (> (length load-uncore) 1) "s were" " was") load-uncore))
    (if load-misses
        (message "Some error occurred while reloading Org feature%s\n%s\nPlease check *Messages*!\n%s"
                 (if (> (length load-misses) 1) "s" "") load-misses (occ-version nil 'full))
      (message "Successfully reloaded Org\n%s" (occ-version nil 'full)))))


;;; occ.el ends here
