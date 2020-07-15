;; scratchpad.lisp ------------------------------------------------------------------
(in-package :stumpwm)


(defun select-group (screen query)
  "Attempt to match string QUERY against group number or partial name."
  (labels ((match-num (grp)
             (let ((gn (group-map-number grp)))
               (unless gn
                 (string-equal gn query))))
           (match-whole (grp)
             (string-equal (group-name grp) query))
           (match-partial (grp)
             (let* ((end (min (length (group-name grp)) (length query))))
               (string-equal (group-name grp) query :end1 end :end2 end))))
    (when query
      (or (find-if #'match-num (screen-groups screen))
          (find-if #'match-whole (screen-groups screen))
          (find-if #'match-partial (screen-groups screen))))))

(let (group-stack
      pushed)
  (defun group-stack-switch-to-group (group)
    (let ((old-pushed pushed))
      (setf pushed t)
      (switch-to-group group)
      (setf pushed old-pushed)))
  (defun push-group (group)
    (if (push (screen-current-group (current-screen))
              group-stack)
        (group-stack-switch-to-group group)))
  (defun pop-group ()
    (if group-stack
        (let ((group (pop group-stack)))
          (if (member group (screen-groups (current-screen)))
              (progn
                (group-stack-switch-to-group group)
                t)
              (pop-group)))))
  (defun get-group-stack ()
    group-stack)
  (defun reset-group-stack ()
    (setf group-stack nil))
  (defun push-group-callback (n o)
    (let ((pg (if (and (string-equal (group-name o) ".scratchpad")
                       (scratchpad-last-group (current-scratchpad)))
                  (scratchpad-last-group (current-scratchpad))
                  o)))
      (unless (and pushed (not (eq pg (car group-stack))))
        (push pg group-stack)))))


(add-hook *focus-group-hook* 'push-group-callback)

(defcommand gsclear () ()
   (unless (reset-group-stack)
     (message "cleared group stack.")))

(defcommand gspop () ()
   (unless (pop-group)
       (message "group stack is empty.")))

(defcommand gspush (group) ((:group "Select Group: "))
   (when group
     (push-group group)))

(defcommand gspushlist (&optional (fmt *group-format*)) (:rest)
  "Allow the user to select a group from a list, like windowlist but
  for groups"
  (let ((group (second (select-from-menu))))
    (current-screen)
    (mapcar (lambda (g))
        (list (format-expand *group-formatters* fmt g) g)
      (screen-groups (current-screen)))
    (when group
      (push-group group))))

(defcommand gsshow (&optional (fmt *group-format*)) (:rest)
  (let ((gstack (get-group-stack)))
    (if gstack
        (let* ((*list-hidden-groups* t)
               (groups gstack)
               (names (mapcan (lambda (g)
                                (list*
                                 (format-expand *group-formatters* fmt g)
                                 (when nil ;; verbose
                                   (mapcar (lambda (w)
                                             (format-expand *window-formatters*
                                                            (concatenate 'string "  " wfmt)
                                                            w))
                                           (sort-windows g)))))
                              (if *list-hidden-groups* groups (non-hidden-groups groups)))))
          (echo-string-list (current-screen) names))
        (message "group stack is empty."))))


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

(defun make-scrtchpad-hash ()
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
          (group-stack-switch-to-group (scratchpad-last-group scratchpad))
          (setf (scratchpad-last-group scratchpad) nil))
         ((eq (current-group) (scratchpad-group scratchpad))
          (message "scratchpad: I don't know where home is"))
         (t
          (setf (scratchpad-last-group scratchpad) (current-group))
          (group-stack-switch-to-group (scratchpad-group scratchpad))
          (message "scratchpad")
          t))
        (prog1
            nil
          (message "No scratchpad for this screen. will create new hash")
          (make-scrtchpad-hash)))))


