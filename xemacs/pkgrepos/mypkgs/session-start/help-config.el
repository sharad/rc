;;; help-config.el --- Help Config

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar configuration|common|help-config|package-list nil)

;;;###autoload
(defun configuration|common|help-config|popup-pos-tip|config ()
  ;; http://www.emacswiki.org/emacs/PosTip
  (use-package popup-pos-tip
      :defer t
      :config
      (progn

        (progn ;; "http://www.emacswiki.org/emacs/PosTip#toc7"
          (defadvice popup-menu-show-quick-help
              (around pos-tip-popup-menu-show-quick-help () activate)
            "Show quick help using `pos-tip-show'."
            (if (eq window-system 'x)
                (let ((doc (popup-menu-document
                            menu (or item
                                     (popup-selected-item menu)))))
                  (when (stringp doc)
                    (pos-tip-show doc nil
                                  (if (popup-hidden-p menu)
                                      (or (plist-get args :point)
                                          (point))
                                      (overlay-end (popup-line-overlay
                                                    menu (+ (popup-offset menu)
                                                            (popup-selected-line menu)))))
                                  nil 0)
                    nil))
                ad-do-it)))

        (progn ;; popup-pos-tip
          (deh-section "http://www.emacswiki.org/emacs/PosTip#toc8"
            (defadvice popup-tip
                (around popup-pos-tip-wrapper (string &rest args) activate)
              (if (eq window-system 'x)
                  (apply 'popup-pos-tip string args)
                  ad-do-it))))

        (progn ;; "http://www.emacswiki.org/emacs/PosTip#toc3"
          (defun my-describe-function (function)
            "Display the full documentation of FUNCTION (a symbol) in tooltip."
            (interactive (list (function-called-at-point)))
            (if (null function)
                (pos-tip-show
                 "** You didn't specify a function! **" '("red"))
                (pos-tip-show
                 (with-temp-buffer
                   (let ((standard-output (current-buffer))
                         (help-xref-following t))
                     (prin1 function)
                     (princ " is ")
                     (describe-function-1 function)
                     (buffer-string)))
                 nil nil nil 0)))
          (define-key help-map (kbd ";") 'my-describe-function))

        (use-package sdic-inline
            :defer t
            :config
            (progn
              (sdic-inline-mode t)))

        (use-package sdic-inline-pos-tip
            :defer t
            :config
            (progn
              (progn ;; "http://www.emacswiki.org/emacs/PosTip#toc8"
                ;; *Change the following lines according to your environment*
                (setq sdic-inline-eiwa-dictionary "/usr/share/dict/gene.sdic")
                (setq sdic-inline-waei-dictionary "/usr/share/dict/jedict.sdic")

                ;; The following is optional. Uncomment if necessary.
                ;; (mapc (lambda (mode)
                ;;         (add-to-list 'sdic-inline-enable-modes mode))
                ;;       '(help-mode Info-mode))
                ;; (mapc (lambda (face)
                ;;         (add-to-list 'sdic-inline-enable-faces face))
                ;;       '(font-lock-doc-face))

                (setq sdic-inline-search-func 'sdic-inline-search-word-with-stem)
                (setq sdic-inline-display-func 'sdic-inline-pos-tip-show)

                (define-key sdic-inline-map "\C-c\C-p" 'sdic-inline-pos-tip-show)

                (sdic-inline-mode 1)))))))
;; (defun configuration|common|help-config|popup-pos-tip|config () ...)




;;;###autoload
(defun configuration|common|help-config|popup-pos-tip|init ()
  (configuration|common|help-config|popup-pos-tip|config))
;; (defun configuration|common|help-config|popup-pos-tip|init () ...)

(push 'popup-pos-tip configuration|common|help-config|package-list)
(push sdic-inline-pos-tip configuration|common|help-config|package-list)



;;;###autoload
(defun configuration|common|help-config|config ()
  )
;; (defun configuration|common|help-config|config () ...)



;;;###autoload
(defun configuration|common|help-config|init ()
  (configuration|common|help-config|config))
;; (defun configuration|common|help-config|init () ...)

(push 'LIB1 configuration|common|help-config|package-list)
;;;###autoload
(defun configuration|common|help-config|packages ()
  configuration|common|help-config|package-list)
;; (defun configuration|common|help-config|packages () ...)



(provide 'help-config)
;;; help-config.el ends here
