
(when (configuration-layer/package-usedp 'calfw-howm)
  (defun spacemacs/calfw-howm-enable ()
    (progn ;; "Keybinding: Calfw-Howm"
      ;;{{ calfw-howm
      (use-package calfw-howm
          :defer t
          :config
          (progn
            (define-key howm-mode-map (kbd "M-c") 'cfw:open-howm-calendar)
            (define-key howm-mode-map (kbd "M-C") 'cfw:elscreen-open-howm-calendar)))))

  (defun spacemacs/calfw-howm-disable ()
    (progn ;; "Keybinding: Calfw-Howm"
      ;;{{ calfw-howm
      (define-key howm-mode-map (kbd "M-c") nil)
      (define-key howm-mode-map (kbd "M-C") nil)))

  (spacemacs/calfw-howm-enable))
