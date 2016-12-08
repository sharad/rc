;;
;; bookmark.el
;; Login : <s@taj>
;; Started on  Thu Jan 20 23:10:32 2011 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;


(defvar configuration|common|bookmark-config|package-list nil)

;;;###autoload
(defun configuration|common|bookmark-config|saveplace|config ()
  (setq save-place-file (auto-config-file "autoconfig/save-place/places")))

;;;###autoload
(defun configuration|common|bookmark-config|saveplace|init ()
    (use-package saveplace
      :defer t
      :config
      (configuration|common|bookmark-config|saveplace|config)))
(push 'saveplace configuration|common|bookmark-config|package-list)

;;;###autoload
(defun configuration|common|bookmark-config|breadcrumb|config ()
  (setq bc-bookmark-file (auto-config-file "breadcrumb/breadcrumbe.el"))
  (autoload 'bc-set               "breadcrumb" "Set bookmark in current point."   t)
  (autoload 'bc-previous          "breadcrumb" "Go to previous bookmark."         t)
  (autoload 'bc-next              "breadcrumb" "Go to next bookmark."             t)
  (autoload 'bc-local-previous    "breadcrumb" "Go to previous local bookmark."   t)
  (autoload 'bc-local-next        "breadcrumb" "Go to next local bookmark."       t)
  (autoload 'bc-goto-current      "breadcrumb" "Go to the current bookmark."      t)
  (autoload 'bc-list              "breadcrumb" "List all bookmarks in menu mode." t)
  (autoload 'bc-clear             "breadcrumb" "Clear all bookmarks."             t))

;;;###autoload
(defun configuration|common|bookmark-config|breadcrumb|init ()
    (use-package breadcrumb
      :defer t
      :config
      (configuration|common|bookmark-config|breadcrumb|config)))
(push 'breadcrumb configuration|common|bookmark-config|package-list)

;;;###autoload
(defun configuration|common|bookmark-config|bookmark|config ()
  (setq bookmark-file (auto-config-file "bookmark/bookmarks.el"))
  (setq bookmark-default-file (auto-config-file "bookmark/bookmarks.el"))
  ;; from http://emacswiki.org/emacs/BookMarks#toc4
  ;; Access File Bookmarks from `C-x C-f'

  ;; If you hit ‘C-x C-f’ and then realize that you want a file that is bookmarked, you can get to the bookmark this way:
  (defun bookmark-to-abbrevs ()
    "Create abbrevs based on `bookmark-alist'."
    (dolist (bookmark bookmark-alist)
      (let* ((name (car bookmark))
             (file (bookmark-get-filename name)))
        (define-abbrev global-abbrev-table name file))))
  ;; Put Last-Selected Bookmark on Top

  ;; Using this method you’ll find frequently used bookmarks easily (cho-seiri-hou in Japanese). – rubikitch

  (defadvice bookmark-jump (after bookmark-jump activate)
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (delq latest bookmark-alist))
      (add-to-list 'bookmark-alist latest)))


  ;; Syncing Bookmarks with zsh

  ;; zsh has a feature called ‘cd-able-vars’ which is similar to
  ;; bookmarks but limited to directories on the local machine. Here’s
  ;; some code to convert emacs’ bookmarks into code for zsh. First add
  ;; this to your .zshrc:

  ;;     setopt cd_able_vars
  ;;     [[ -r ~/.zsh.bmk ]] && source ~/.zsh.bmk

  ;; Add this to your .emacs:

  (defadvice bookmark-write-file
      (after local-directory-bookmarks-to-zsh-advice activate)
    (local-directory-bookmarks-to-zsh))

  (defun local-directory-bookmarks-to-zsh ()
    (interactive)
    (when (and (require 'tramp nil t)
               (require 'bookmark nil t))
      (set-buffer (find-file-noselect "~/.zsh.bmk" t t))
      (delete-region (point-min) (point-max))
      (insert "# -*- mode:sh -*-\n")
      (let (collect-names)
        (mapc (lambda (item)
                (let ((name (replace-regexp-in-string "-" "_" (car item)))
                      (file (cdr (assoc 'filename
                                        (if (cddr item) item (cadr item))))))
                  (when (and (not (tramp-tramp-file-p file))
                             (file-directory-p file))
                    (setq collect-names (cons (concat "~" name) collect-names))
                    (insert (format "%s=\"%s\"\n" name (expand-file-name file) name)))))
              bookmark-alist)
        (insert ": " (mapconcat 'identity collect-names " ") "\n"))
      (let ((backup-inhibited t)) (save-buffer))
      (kill-buffer (current-buffer))))
  )

;;;###autoload
(defun configuration|common|bookmark-config|bookmark|init ()
    (use-package bookmark
      :defer t
      :config
      (configuration|common|bookmark-config|bookmark|config)))
(push 'bookmark configuration|common|bookmark-config|package-list)

;;;###autoload
(defun configuration|common|bookmark-config|bookmark+|config ()
           ;; available in elpa
  (setq bmkp-bmenu-state-file    (auto-config-file "bookmark+/emacs-bmk-bmenu-state.el")
        bmkp-bmenu-commands-file (auto-config-file "bookmark+/emacs-bmk-bmenu-commands.el"))

  (remove-hook 'kill-emacs-hook 'bookmark-bmenu-save))

;;;###autoload
(defun configuration|common|bookmark-config|bookmark+|init ()
    (use-package bookmark+
      :defer t
      :config
      (configuration|common|bookmark-config|bookmark+|config)))
(push 'bookmark+ configuration|common|bookmark-config|package-list)

;;;###autoload
(defun configuration|common|bookmark-config|bm|config ()
  (setq bm-repository-file (auto-config-file "bm/bm-repository")))

;;;###autoload
(defun configuration|common|bookmark-config|bm|init ()
    (use-package bm
      :defer t
      :config
      (configuration|common|bookmark-config|bm|config)))
(push 'bm configuration|common|bookmark-config|package-list)

;;;###autoload
(defun configuration|common|bookmark-config|linkd|config ()
                                        ;excellent
  ;; http://dto.github.com/notebook/linkd.html
  ;; http://www.emacswiki.org/emacs/LinkdMode

  (setq linkd-use-icons t
        linkd-icons-directory "~/.xemacs/pkgrepos/world/misc/linkd/icons/") ;; or wherever you put it
  )

;;;###autoload
(defun configuration|common|bookmark-config|linkd|init ()
    (use-package bm
      :defer t
      :config
      (configuration|common|bookmark-config|linkd|config)))
(push 'linkd configuration|common|bookmark-config|package-list)

;;;###autoload
(defun configuration|common|bookmark-config|config ()

  )

;;;###autoload
(defun configuration|common|bookmark-config|init ()
  (configuration|common|bookmark-config|config))

;;;###autoload
(defun configuration|common|bookmark-config|packages ()
  configuration|common|bookmark-config|package-list
  ;; '(saveplace breadcrumb bookmark bookmark+ bm linkd)
  )




(provide 'bookmark-config)
