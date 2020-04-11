
;; https://kkatsuyuki.github.io/notmuch-conf/

(defun notmuch-exec-offlineimapx ()
  "execute offlineimap"
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "offlineimap"
                                "*offlineimap*"
                                (concat "~/bin/syncimap -u ttyui -a " (getenv "OFFLINEIMAPACCOUNT")))
   #'(lambda (process event)
       (notmuch-refresh-all-buffers)
       (let ((w (get-buffer-window "*offlineimap*")))
         (when w
           (with-selected-window w (recenter (window-end)))))))
  (popwin:display-buffer "*offlineimap*"))

(add-to-list 'popwin:special-display-config
             '("*offlineimap*"
               :dedicated t
               :position bottom
               :stick t
               :height 0.4
               :noselect t))


