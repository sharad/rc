
(when (configuration-layer/package-usedp 'gtags)
  (defun spacemacs/gtags-enable ()
    (progn ;; "Keybinding: Gtags"
      ;; ref: http://www.emacswiki.org/cgi-bin/wiki/EmacsTags
      ;; Completion
      ;; You can use M-x complete-tag to get simple (ie context
      ;; free) symbol name Completion. This works like other types of
      ;; completion in emacs, if there are multiple possibilities a window will
      ;; be opened showing them all. This used to be bound to M-TAB by default
      ;; but as many window managers use this to switch between windows, I tend
      ;; to use M-RET instead.


      ;; TODO: find some other binding as this is used in org-mode also by spacemacs

      ;; (global-set-key (kbd "M-<return>") 'complete-tag)
      ))

  (defun spacemacs/package-disable ()
    (progn ;; "Keybinding: Gtags"
      (progn ;; "Keybinding: Gtags"
        ;; ref: http://www.emacswiki.org/cgi-bin/wiki/EmacsTags
        ;; Completion
        ;; You can use M-x complete-tag to get simple (ie context
        ;; free) symbol name Completion. This works like other types of
        ;; completion in emacs, if there are multiple possibilities a window will
        ;; be opened showing them all. This used to be bound to M-TAB by default
        ;; but as many window managers use this to switch between windows, I tend
        ;; to use M-RET instead.

        ;; TODO: find some other binding as this is used in org-mode also by spacemacs

        ;; (global-unset-key (kbd "M-<return>"))
        )))

  (spacemacs/gtags-enable))
