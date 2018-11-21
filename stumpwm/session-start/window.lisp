;; This defines two new stumpwm commands: gforward and gbackward that
;; "move" current group. It's quite handy when you have many groups
;; and want to reorder them.


(defcommand  jump-to-new-window () ()
  (let* ((group (current-group))
         (win (car (last (group-windows group)))))
    (unless (eq (group-current-window group) win)
      (when win (group-focus-window group win)))))
