;; This defines two new stumpwm commands: gforward and gbackward that
;; "move" current group. It's quite handy when you have many groups
;; and want to reorder them.

(defun swap-groups (group1 group2)
  (rotatef (slot-value group1 'number) (slot-value group2 'number)))

(defun move-group-forward (&optional (group (current-group)))
  (swap-groups group (next-group group (sort-groups (current-screen)))))

(defun move-group-backward (&optional (group (current-group)))
  (swap-groups group (next-group group (reverse (sort-groups (current-screen))))))

(define-stumpwm-command "gforward" () ()
  (move-group-forward)
  (echo-groups (current-screen) *group-format*))

(define-stumpwm-command "gbackward" () ()
  (move-group-backward)
  (echo-groups (current-screen) *group-format*))
