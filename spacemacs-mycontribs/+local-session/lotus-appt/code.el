
(defun configuration|common|appt-config|appt|init|config ()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www.emacswiki.org/emacs/AppointmentMode
;; start
  (if (featurep 'appt)
      (if (not running-xemacs)
          (appt-activate 1); use (appt-activate 1) for GNU Emacs
          (appt-initialize))) ; XEmacs

  (setq appt-msg-countdown-list '(10 5 1) ; XEmacs
        appt-audible (cons 3 .5)          ;
        appt-check-time-syntax nil        ; XEmacs
        appt-announce-method 'appt-persistent-message-announce  ; XEmacs
        appt-display-duration 59)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; end
  ;; from http://www.emacswiki.org/emacs/AppointmentMode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; end
  ;; from http://alfredobuttari.wordpress.com/2008/02/08/emacs-appt-mode-reminders-with-gtk-popups/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; start
  ;; from
  )

;;;###autoload
(defun configuration|common|appt-config|packages ()
  '(appt))

;;;###autoload
(defun configuration|common|appt-config|appt|init ()
  (use-package appt
      :defer t
      :config
      (configuration|common|appt-config|appt|init|internal)))


(provide 'appt-config)
