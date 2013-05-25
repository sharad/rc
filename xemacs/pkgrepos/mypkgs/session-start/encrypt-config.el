;; from: http://www.emacswiki.org/emacs/GnusEncryptedAuthInfo

(eval-after-load "epa"
    '(epa-file-disable))

(deh-require-maybe epa-file


  ;; (epa-file-enable)

  (setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; now I do not want to disable agent ....
;;{{ http://www.enigmacurry.com/2009/01/14/extending-emacs-with-advice/
  (defadvice epg--start (around advice-epg-disable-agent disable)
    "Make epg--start not able to find a gpg-agent"
    (let ((agent (getenv "GPG_AGENT_INFO")))
      (setenv "GPG_AGENT_INFO" nil)
      ad-do-it
      (setenv "GPG_AGENT_INFO" agent)))
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



(defun forget-passphrase (&optional buffer)
   "thisandthat."
   (interactive "bbuffer: ")
   (let* ((buffer (or buffer (current-buffer)))
          (file-name (file-truename (buffer-file-name (get-buffer buffer)))))
     (when file-name
       (setq epa-file-passphrase-alist (assq-delete-all-test file-name epa-file-passphrase-alist #'string-equal))
       (kill-buffer buffer))))

(defvar epa-file-passphrase-suspend-cleanup nil "Stop cleanup for now.")

(defun epa-file-passphrase-cleanup (&optional exceptitions)
  (interactive)

  (dolist (a epa-file-passphrase-alist)
    (let* ((file-name (car a))
           (buffer-of-file (find file-name (buffer-list) :key #'buffer-file-name :test #'string-equal)))
      (unless (member file-name
                      (mapcar 'file-truename exceptitions))
        (setq epa-file-passphrase-alist (assq-delete-all-test file-name epa-file-passphrase-alist #'string-equal))
        (if buffer-of-file
            (kill-buffer buffer-of-file)))))

  (dolist (buff (buffer-list))
    (let ((buff-name (buffer-file-name buff)))
      (when (and
             (and buff-name
                  ;; (file-exists-p buff-name)
                  (string-match ".gpg\$" buff-name))
             (not (member buff-name exceptitions)))
        ;; (message "found %s" buff-name)
        (kill-buffer buff)
        ;; (message "killed %s" buff)
        (setq epa-file-passphrase-alist (assq-delete-all-test buff-name epa-file-passphrase-alist #'string-equal))))))

(defvar epa-file-passphrase-cleanup-exceptitions nil "Epa file passphrase cleanup exceptitions")

(require 'common-info)
(defvar epa-file-passphrase-cleanup-timer nil "epa file passphrase cleanup timer")

(setq
 epa-file-passphrase-cleanup-timer
 (run-with-idle-timer 10 t 'epa-file-passphrase-cleanup epa-file-passphrase-cleanup-exceptitions))

(defun epa-passphrase-cleanup-suspend ()
  (interactive)
  (timer-activate epa-file-passphrase-cleanup-timer t))

(defun epa-passphrase-cleanup-resume ()
  (interactive)
  (timer-activate epa-file-passphrase-cleanup-timer))


(pushnew "~/.authinfo.gpg" epa-file-passphrase-cleanup-exceptitions)

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
