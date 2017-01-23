
;; ref: http://boxes.thomasjensen.com/

;; Jason L. Shiffer kindly submitted the following information on
;; integrating boxes with Emacs: The simple interface (only a single
;; box style, but easy):

(defun configuration|common|appt-config|boxes|config ()
  (autoload 'boxes-command-on-region "boxes" nil t)
  (autoload 'boxes-remove "boxes" nil t)
  (autoload 'boxes-create "boxes" nil t)




  ;; (Defun box(min max)
  ;;   (let ((min min)
  ;;         (max max))
  ;;     ))
  )

;;;###autoload
(defun configuration|common|appt-config|packages ()
  '(boxes))

;;;###autoload
(defun configuration|common|appt-config|boxes|init ()
  (use-package boxes
      :defer t
      :config
      (configuration|common|appt-config|boxes|config)))

(provide 'art-config)
