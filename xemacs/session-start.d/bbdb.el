;;
;; bbdb.el
;;
;; Made by Sharad Pratap
;; Login   <s@taj>
;;
;; Started on  Sun Dec  6 11:29:45 2009 Sharad Pratap
;; Last update Sun Dec  6 13:15:07 2009 Sharad Pratap



(deh-require-maybe 'bbdb
  (xrequire 'bbdb-merge)
  (xrequire 'bbdb-vcard-import)

  (setq
   bbdb-use-pop-up nil
   bbdb/mail-auto-create-p t
   bbdb/news-auto-create-p t)
  ;; save bbdb
  (add-hook 'kill-emacs-hook 'bbdb-save-db))

(user-provide 'bbdb)

