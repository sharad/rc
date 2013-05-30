;;; dev-config.el --- dev config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad>
;; Keywords: internal, internal

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




(deh-require-maybe which-func
  (which-function-mode 1)
  (defface which-func
      '((((class color) (min-colors 88) (background dark)) (:foreground "Green")))
    "which-face")


  (defun copy-current-function ()
    (interactive)
    (let ((fun-name (which-function)))
      (if fun-name
          (kill-new fun-name)
          (message "Not able to get function.")))))

(deh-require-maybe 'member-functions
  ;; for C++ mode
  )

(deh-require-maybe devel-notes
  ;; http://www.emacswiki.org/emacs/DevelNotes
  (global-set-key "\C-cza" 'develnotes-add-annotation)
  (global-set-key "\C-czv" 'develnotes-visit-file)
  (global-set-key "\C-czt" 'develnotes-add-TODO)
  (global-set-key "\C-czf" 'develnotes-add-FIXME))

(deh-require-maybe sidebrain
;; (when nil
  ;;http://www.emacswiki.org/emacs/SideBrain
  ;;http://sidebrain.sourceforge.net/manual/index.html
  ;; (add-hook 'find-file-hook
  ;;           #'sidebrain-read-todo-from-comments)

  ;; (add-hook 'sharad/enable-login-session-inperrupting-feature-hook
  ;;           #'(lambda ()
  ;;               (add-hook 'find-file-hook
  ;;                         #'sidebrain-read-todo-from-comments)) t)

  ;; (add-hook 'sharad/disable-login-session-inperrupting-feature-hook
  ;;           #'(lambda ()
  ;;               (remove-hook 'find-file-hook
  ;;                            #'sidebrain-read-todo-from-comments)) t)


  ;; (xml-parse-file sidebrain-file-name)
  ;; (defun filter-ws (tree)
  ;;   (if (listp tree)
  ;;       (mapcar 'tree tree)
  ;;       tree))

  ;; xmllint --noblanks ~/.sidebrain.xml | tr -d '\n' |  sed 's/>[[:space:]]\+</></g'
  ;; (xml-parse-file "~/x.xml" )
  ;; (defun filter-ws (tree)
  ;;   (if (listp tree)
  ;;       (mapcar 'tree tree)
  ;;       tree))
  )


(deh-require-maybe subword
  ;; (global-subword-mode 1)
  (add-element-to-lists '(lambda nil (subword-mode 1)) pgm-langs))




(eval-after-load "simple-call-tree+"
  '(progn
    (defun left-word (&optional arg)
      (interactive)
      (backward-word arg))

    (defun right-word (&optional arg)
      (interactive)
      (forward-word arg))))

(eval-after-load "simple-call-tree"
    '(progn
      (defun left-word (&optional arg)
        (interactive)
        (backward-word arg))

      (defun right-word (&optional arg)
        (interactive)
        (forward-word arg))))


  (eval-after-load 'outline
    '(progn
      (require 'outline-magic)
      (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))


(deh-require-maybe (and simple-call-tree+ fm outline outline-magic)
  )


;; remove-useless-whitespace
(deh-require-maybe develock
                  ;for now
 (ad-remove-advice 'indent-region 'around 'remove-useless-whitespace)
 (ad-activate 'indent-region)
 (ad-update  'indent-region))


(deh-section "gprof"
 (autoload 'gprof-mode "gprof" "Switch to the gprof major mode" t)
 (add-to-list 'auto-mode-alist '("\\.g?prof\\'" . gprof-mode)))

(require 'utils-config)

;; load cclookup when compile time
(eval-when-compile (require 'cclookup))
(deh-require-maybe cclookup
  ;; taesoo.org/proj/cclookup.html
  ;; http://www.cppreference.com/wiki
  ;; http://github.com/tsgates/cclookup
  (setq cclookup-program "cclookup")




  ;; add cclookup to your loadpath, ex) "~/.lisp/addons/cclookup"
  (setq cclookup-dir (find-library-directory "cclookup")
        ;; set executable file and db file
        cclookup-program (concat cclookup-dir "/cclookup.py")
        cclookup-db-file (concat cclookup-dir "/cclookup.db"))



  ;; to speedup, just load it on demand
  (autoload 'cclookup-lookup "cclookup"
    "Lookup SEARCH-TERM in the C++ Reference indexes." t)
  (autoload 'cclookup-update "cclookup"
    "Run cclookup-update and create the database at `cclookup-db-file'." t))


(deh-require-maybe ya-cppref
  ;; git@github.com:whitypig/ya-cppref.git
  (setq ya-cppref-path-to-doc-root "/usr/share/doc/c-cpp-reference/"
        ;; "/usr/local/doc/cpp/reference/"
        ))


(deh-require-maybe devhelp-index
  ;; https://github.com/martine/devhelp-index
  ;; TODO: Add completion
  ;; http://askubuntu.com/questions/287526/how-to-add-more-languages-docs-to-devhelp-java-etc
  ;; refrese devhelp
  (defun devhelp-make-index ()
    (interactive)
    "Read a search query from the minibuffer."
    (let ((dir (find-library-directory "devhelp-index")))
      (shell-command (concat dir "/devhelp-index.py")))))

(deh-require-maybe eassist
  ;; http://www.emacswiki.org/emacs/eassist.el
  )


(provide 'dev-config)
;;; dev-config.el ends here
