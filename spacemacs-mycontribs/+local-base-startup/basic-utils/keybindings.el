
(when (configuration-layer/package-usedp 'elscreen)
  (defun spacemacs/elscreen-enable ()
    (deh-section "Keybinding: Elscreen"
                 ;;{{ elscreen
                 ;; (global-set-key [C-z c] 'elscreen-create)
                 (global-set-key-if-unbind [s-right] 'elscreen-next)
                 (global-set-key-if-unbind [s-left]  'elscreen-previous)
                 (global-set-key-if-unbind [H-right] 'elscreen-move-right)
                 (global-set-key-if-unbind [H-left]  'elscreen-move-left)
                 (global-set-key-if-unbind [M-H-right]    'elscreen-swap)
                 ;; (global-set-key-if-unbind [H-down]  'elscreen-previous)
                 ;;}}
                 ))
  (defun spacemacs/elscreen-disable ()
    ))



