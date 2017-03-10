
(when (configuration-layer/package-usedp 'db)
  (defun spacemacs/db-enable ()
    (progn ;; "Keybinding: Db"
      ;;{{ db
      (global-set-key-if-unbind [f12] 'enter-db-mode)))

  (defun spacemacs/db-disable ()
    (progn ;; "Keybinding: Db"
      (global-unset-key-if-bound [f12] 'enter-db-mode)))

  (spacemacs/db-enable))
