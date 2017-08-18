
(when (configuration-layer/package-usedp 'eudc)
  (defun spacemacs/eudc-enable ()
    (progn ;; "Keybinding: Eudc"

      ;;{{ eudc
      (use-package message
          :defer t
          :config
          (progn
            (define-key message-mode-map (kbd "H-c TAB") 'lotus-enz-eudc-expand-inline)))
      (use-package sendmail
          :defer t
          :config
          (progn
            (define-key mail-mode-map (kbd "H-c TAB") 'lotus-enz-eudc-expand-inline)))
      (use-package post
          :defer t
          :config
          (progn
            (define-key post-mode-map (kbd "H-c TAB") 'lotus-enz-eudc-expand-inline)))))

  (defun spacemacs/eudc-disable ()
    (progn ;; "Keybinding: Eudc"
      (define-key message-mode-map (kbd "H-c TAB") nil)
      (define-key mail-mode-map    (kbd "H-c TAB") nil)
      (define-key post-mode-map    (kbd "H-c TAB") nil)))

  (spacemacs/eudc-enable))
