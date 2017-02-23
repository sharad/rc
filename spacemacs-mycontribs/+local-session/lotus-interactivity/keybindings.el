

(when (configuration-layer/package-usedp 'ido)
  (defun spacemacs/ido-enable ()
    (progn ;; "Keybinding: Elscreen"
      ;;{{ elscreen
      (define-key global-map [(kbd "C-xb")] 'ido-switch-buffer)
      (global-set-key-if-unbind "\C-xb" 'ido-switch-buffer)))

  (defun spacemacs/ido-disable ()
    (progn ;; "Keybinding: Elscreen"
      (define-key global-map [(kbd "C-xb")] nil)
      (global-unset-key-if-bind "\C-xb" 'ido-switch-buffer)))

  (spacemacs/ido-enable))

(when (configuration-layer/package-usedp 'swiper)
  (defun spacemacs/swiper-enable ()
    (progn ;; "Keybinding: Elscreen"
      ;;{{ elscreen
      (global-set-key-if-unbind (kbd "C-s") 'swiper)
      (global-set-key-if-unbind (kbd "C-c C-r") 'ivy-resume)))

  (defun spacemacs/swiper-disable ()
    (progn ;; "Keybinding: Elscreen"
      (global-unset-key-if-bind (kbd "C-s") 'swiper)
      (global-unset-key-if-bind (kbd "C-c C-r") 'ivy-resume)))

  (spacemacs/swiper-enable))

(when (configuration-layer/package-usedp 'counsel)
  (defun spacemacs/counsel-enable ()
    (progn ;; "Keybinding: Elscreen"
      ;;{{ elscreen
      (global-set-key-if-unbind (kbd "M-x") 'counsel-M-x)
      (global-set-key-if-unbind (kbd "C-x C-f") 'counsel-find-file)))

  (defun spacemacs/counsel-disable ()
    (progn ;; "Keybinding: Elscreen"
      (global-unset-key-if-bind (kbd "M-x") 'counsel-M-x)
      (global-unset-key-if-bind (kbd "C-x C-f") 'counsel-find-file)))

  (spacemacs/counsel-enable))
