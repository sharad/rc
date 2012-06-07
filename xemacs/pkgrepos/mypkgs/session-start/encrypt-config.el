;; from: http://www.emacswiki.org/emacs/GnusEncryptedAuthInfo
(deh-require-maybe epa-file
  (epa-file-enable)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; now I do not want to disable agent ....
;;{{ http://www.enigmacurry.com/2009/01/14/extending-emacs-with-advice/
  ;; (defadvice epg--start (around advice-epg-disable-agent disable)
  ;;   "Make epg--start not able to find a gpg-agent"
  ;;   (let ((agent (getenv "GPG_AGENT_INFO")))
  ;;     (setenv "GPG_AGENT_INFO" nil)
  ;;     ad-do-it
  ;;     (setenv "GPG_AGENT_INFO" agent)))
  (defun epg-disable-agent ()
    "Make EasyPG bypass any gpg-agent"
    (interactive)
    (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
    (ad-activate 'epg--start)
    (message "EasyPG gpg-agent bypassed"))

  (defun epg-enable-agent ()
    "Make EasyPG use a gpg-agent after having been disabled with epg-disable-agent"
    (interactive)
    (ad-disable-advice 'epg--start 'around 'advice-epg-disable-agent)
    (ad-activate 'epg--start)
    (message "EasyPG gpg-agent re-enabled"))
;;}}
  )



(defun forgetpass (&optional buffer)
   "thisandthat."
   (interactive "bbuffer: ")
   (let* ((buffer (or buffer (current-buffer)))
          (file-name (file-truename (buffer-file-name (get-buffer buffer)))))
     (when file-name
       (setq epa-file-passphrase-alist (assq-delete-all-test file-name epa-file-passphrase-alist #'string-equal))
       (kill-buffer buffer))))



;; test
;; (setq epa-file-passphrase-alist nil)

;; (assq-delete-all-test
;;  (concat (getenv "HOME") "/.setup-trunk/spaa.cresh")
;;  epa-file-passphrase-alist
;;  'string-equal)

;; (assq-delete-all-test
;;  (concat (getenv "HOME") "/.Private/secure.d/mail/authinfo")
;;  epa-file-passphrase-alist
;;  'string-equal)

;; epa-file-passphrase-alist


(provide 'encrypt-config)
