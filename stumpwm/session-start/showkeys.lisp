

;; Code by Male
;; Display the keysequence in progress
(defun key-press-hook (key key-seq cmd)
  (declare (ignore key))
  (unless (eq *top-map* *resize-map*)
    (let ((*message-window-gravity* :bottom-right))
      (message "Key sequence: ~A" (print-key-seq (reverse key-seq))))
    (when (stringp cmd)
      ;; Give 'em time to read it.
      (sleep 0.5))))

(defmacro replace-hook (hook fn)
 `(remove-hook ,hook ,fn)
 `(add-hook ,hook ,fn))

;; (replace-hook *key-press-hook* 'key-press-hook)

