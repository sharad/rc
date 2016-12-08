;;
;; bbdb-config.el
;;
;; Made by Sharad Pratap
;; Login   <s@taj>
;;
;; Started on  Sun Dec  6 11:29:45 2009 Sharad Pratap
;; Last update Sun Dec  6 13:15:07 2009 Sharad Pratap




;;;###autoload
(defun configuration|common|bbdb-config|bbdb|config ()
  (setq
   bbdb-use-pop-up nil
   bbdb/mail-auto-create-p t
   bbdb/news-auto-create-p t)
  ;; save bbdb
  (add-hook 'kill-emacs-hook 'bbdb-save-db))

;;;###autoload
(defun configuration|common|bbdb-config|bbdb|init ()
    (use-package bbdb
      :defer t
      :config
      (configuration|common|bbdb-config|bbdb|config)))

;;;###autoload
(defun configuration|common|bbdb-config|packages ()
  '(bbdb bbdb-merge bbdb-vcard-import))





(provide 'bbdb-config)
