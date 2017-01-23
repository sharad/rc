;; automatic text, at least adapt author!


;;{{ from: http://www.emacswiki.org/emacs/AutoInsertMode
;; I use Yasnippet for initial skeletons:




;; (deh-require-maybe  text-language
;;   (add-element-to-lists 'text-language-mode text-langs)
;;   (add-element-to-lists 'text-language-guess-mode text-langs))

;;;###autoload
(defun configuration|common|autotext-config|template|config ()
  )

;;;###autoload
(defun configuration|common|autotext-config|template|init ()
    (use-package template
      :defer t
      :config
      (configuration|common|autotext-config|template|config)))

;;;###autoload
(defun configuration|common|autotext-config|autoinsert+|config ()
  ;; (add-hook 'find-file-hooks 'auto-insert+)

  ;; (auto-insert+-mode 1)


  )

;;;###autoload
(defun configuration|common|autotext-config|autoinsert+|init ()
    (use-package autoinsert+
      :defer t
      :config
      (configuration|common|autotext-config|autoinsert+|config))

    (auto-insert+-mode 1))

;;;###autoload
(defun configuration|common|autotext-config|auto-yasnippet|config ()
  ;;   (global-set-key (kbd "H-w") 'create-auto-yasnippet)
  ;;   (global-set-key (kbd "H-y") 'expand-auto-yasnippet)
  )

;;;###autoload
(defun configuration|common|autotext-config|auto-yasnippet|init ()
    (use-package auto-yasnippet
      :defer t
      :config
      (configuration|common|autotext-config|auto-yasnippet|config)))

;;;###autoload
(defun configuration|common|autotext-config|packages ()
  '(template
    template-simple
    autoinsert+
    yasnippet
    auto-yasnippet))

(provide 'autotext-config)
