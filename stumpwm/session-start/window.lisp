;; This defines two new stumpwm commands: gforward and gbackward that
;; "move" current group. It's quite handy when you have many groups
;; and want to reorder them.


(defcommand  jump-to-new-window () ()
  (let* ((group (current-group))
         (win (car (last (group-windows group)))))
    (unless (eq (group-current-window group) win)
      (when win (group-focus-window group win)))))



;; Open PR for stumpwm


(defun frame-raise-window-ordered (g f w &optional (focus t))
  "Raise the window w in frame f in group g. if FOCUS is
T (default) then also focus the frame."
  (let ((oldw (frame-window f)))
    ;; nothing to do when W is nil
    (setf (frame-window f) w)
    (unless (and w (eq oldw w))
      (if w
          (raise-window w)
          (mapc 'hide-window (frame-windows g f))))
    ;; If raising a window in the current frame we must focus it or
    ;; the group and screen will get out of sync.
    (when (or focus
              (eq (tile-group-current-frame g) f))
      (focus-frame g f))
    (when (and w (not (window-modal-p w)))
      (raise-modals-of w))))

(defun clear-frame-ordered (frame group)
  "Clear the given frame."
  (frame-raise-window-ordered group frame nil (eq (tile-group-current-frame group) frame)))

(defcommand (fclear-ordered tile-group) () ()
  "Clear the current frame."
  (clear-frame-ordered (tile-group-current-frame (current-group)) (current-group)))


;; OR


(defun frame-raise-window (g f w &optional (focus t))
  "Raise the window w in frame f in group g. if FOCUS is
T (default) then also focus the frame."
  (let ((oldw (frame-window f)))
    ;; nothing to do when W is nil
    (setf (frame-window f) w)
    (unless (and w (eq oldw w))
      (if w
          (raise-window w)
          (mapc 'hide-window (reverse (frame-windows g f)))))
    ;; If raising a window in the current frame we must focus it or
    ;; the group and screen will get out of sync.
    (when (or focus
              (eq (tile-group-current-frame g) f))
      (focus-frame g f))
    (when (and w (not (window-modal-p w)))
      (raise-modals-of w))))

