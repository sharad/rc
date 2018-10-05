


;; https://stackoverflow.com/questions/19283368/how-can-i-open-quickly-a-file-in-emacs
(defun my-helm-omni (&rest arg)
  ;; just in case someone decides to pass an argument, helm-omni won't fail.
  (interactive)
  (helm-other-buffer
   (append '(helm-c-source-buffers-list ;; list of all open buffers
             helm-c-source-recentf)    ;; all recent files

           ;; projectile errors out if you're not in a project
           (if (projectile-project-p) ;; so look before you leap
               '(helm-source-projectile-files-list
                 helm-source-projectile-recentf-list
                 helm-source-projectile-buffers-list)
               '())

           '(
             helm-c-source-files-in-current-dir ;; files in current directory
             helm-c-source-locate               ;; file anywhere
             helm-c-source-bookmarks            ;; bookmarks too
             helm-c-source-buffer-not-found))     ;; ask to create a buffer otherwise

   "*helm-omni*"))



(defun my-helm-omni (&rest arg)
  (interactive)
  (helm-other-buffer
   (append
    '(helm-source-buffers-list)
    (if (projectile-project-p) ;; so look before you leap
        '(helm-source-projectile-files-list
          helm-source-projectile-recentf-list
          helm-source-projectile-buffers-list)
        '())

    '(helm-source-locate))
   "*helm-omni*"))



(defun lotus-helm-omni (&rest arg)
  (interactive)
  (helm
   :sources
   (append
    '(helm-source-buffers-list)
    (when (projectile-project-p) ;; so look before you leap
      '(helm-source-projectile-files-list
        helm-source-projectile-recentf-list
        helm-source-projectile-buffers-list))
    '(helm-source-locate
      helm-source-bookmarks
      helm-source-bookmark-set))
   :buffer "*helm-omni*"))
