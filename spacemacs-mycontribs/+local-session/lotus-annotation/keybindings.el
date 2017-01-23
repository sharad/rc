
(when (configuration-layer/package-usedp 'annot)
  (defun spacemacs/annot-enable ()
    (progn ;; "Keybinding: Elscreen"
        (define-key ctl-x-map "a"    nil)
        (define-key ctl-x-map "\C-a" nil)
        (define-key ctl-x-map "r"    nil)
        (define-key ctl-x-map "w"    nil)
        (define-key ctl-x-map "A"    nil)))
  (defun spacemacs/annot-disable ()
    (progn ;; "Keybinding: Elscreen"
        (define-key ctl-x-map "a"    'annot-edit/add)
        (define-key ctl-x-map "\C-a" 'annot-edit/add)
        (define-key ctl-x-map "r"    'annot-remove)
        (define-key ctl-x-map "w"    'annot-add-image)
        (define-key ctl-x-map "A"    'annot-convert)))

  (spacemacs/annot-enable))
