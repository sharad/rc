


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





;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/

(setq h-data '((:num 1 :key "apple")
               (:num 9 :key "berry")
               (:num 2 :key "cactus")
               (:num 5 :key "dog")
               (:num 4 :key "frog")))

(defun h-candidates ()
  "Returns candidates for the helm source."
  (loop for cand in h-data
        collect (cons (format "%s %s"
                              (plist-get cand :num)
                              (plist-get cand :key))
                      cand)))

(print (h-candidates))

(defvar h-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>")   'h-sort)
    map)
  "keymap for a helm source.")

(defvar h-sort-fn nil)

(defun h-sort ()
  (interactive)
  (let ((action (read-char "#decreasing (d) | #increasing (i) | a-z (a) | z-a (z: ")))
    (cond
     ((eq action ?d)
      (setq h-sort-fn (lambda (c1 c2) (> (plist-get (cdr c1) :num) (plist-get (cdr c2) :num)))))
     ((eq action ?i)
      (setq h-sort-fn (lambda (c1 c2) (< (plist-get (cdr c1) :num) (plist-get (cdr c2) :num)))))
     ((eq action ?a)
      (setq h-sort-fn (lambda (c1 c2) (string< (plist-get (cdr c1) :key) (plist-get (cdr c2) :key)))))
     ((eq action ?z)
      (setq h-sort-fn (lambda (c1 c2) (string> (plist-get (cdr c1) :key) (plist-get (cdr c2) :key)))))
     (t (setq h-sort-fn nil)))
    (helm-refresh)
    (setq h-sort-fn nil)))

(defun h-candidate-transformer (candidates source)
  (if h-sort-fn
      (progn (message "Sorting with %s" h-sort-fn)
             (-sort h-sort-fn candidates))
    candidates))

(defun h-action-transformer (actions candidate)
  "Candidate is the result selected."
  (if (evenp (plist-get candidate :num))
      '(("Even" . identity))
    '(("Odd" . identity))))

(setq h-source
      (helm-build-sync-source "number-selector"
        :keymap h-map
        :candidates #'h-candidates
        :filtered-candidate-transformer #'h-candidate-transformer
        :action-transformer #'h-action-transformer))

(helm :sources 'h-source)

