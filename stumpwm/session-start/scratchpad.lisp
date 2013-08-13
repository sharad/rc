;; scratchpad.lisp ------------------------------------------------------------------
(in-package :stumpwm)

;; last grooup (reverse (sort-groups (current-screen)))


(defstruct scratchpad
  (last-group '())
  (group '()))

(defvar *scratchpads* '()
  "All scratchpads indexed by screen.")

(defun current-scratchpad ()
  (gethash (current-screen) *scratchpads*))

(defun create-scratchpad-group (screen)
  (let ((scratchpad-group (add-group screen ".scratchpad" :background t)))
    (setf (group-number scratchpad-group) 0)
    scratchpad-group))

(unless *scratchpads*
  ;; Create a scratchpad for each screen
  (setf *scratchpads* (make-hash-table :test #'eq))
  (let ((start-screen (car *screen-list*)))
    (loop for i in *screen-list*
          do (progn (switch-to-screen i)
                    (let ((scratchpad-group (create-scratchpad-group i)))
                      ;;Store the scratchpad
                      (setf (gethash (current-screen)
                                     *scratchpads*)
                            (make-scratchpad
                             :group scratchpad-group)))))
    (switch-to-screen start-screen)))


(defcommand scratchpad () ()
  (let ((scratchpad (current-scratchpad)))
    (if scratchpad
        (cond
         ((scratchpad-last-group scratchpad)
          (switch-to-group (scratchpad-last-group scratchpad))
          (setf (scratchpad-last-group scratchpad) nil))
         ((eq (current-group) (scratchpad-group scratchpad))
          (message "scratchpad: I don't know where home is"))
         (t
          (setf (scratchpad-last-group scratchpad) (current-group))
          (switch-to-group (scratchpad-group scratchpad))
          (message "scratchpad")
          t))
        (prog1
            nil
          (message "No scratchpad for this screen.")))))
