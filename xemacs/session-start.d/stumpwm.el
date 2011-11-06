;; -*- emacs-lisp -*-


(defun planner-today-ensure-exists ()
  (unless (file-exists-p (concat planner-directory (planner-today) ".muse"))
                        (save-excursion
                          (save-window-excursion
                            (plan))))
    (planner-today))

(defun planner-task-lists (plan)
  (planner-extract-tasks
   (list (cons plan (concat planner-directory "/" plan ".muse")))))

;;test
;;
(testing
 (planner-task-lists (planner-today-ensure-exists)))


(defun planner-plans-on-task-lists-main (task-lists)
  (remove-duplicates
   (apply
    'append
    (mapcar
     '(lambda (task)
        (split-string (nth 6 task) ","))
     task-lists))
   :test 'equal))

(defun planner-plans-on-task-lists (task-lists)
  (mapcar
   '(lambda (str)
     (if (string-match "\\[\\[\\(\\S\\+\\)\\]\\]" str)
         (replace-match "\\1" t nil str)))
   (planner-plans-on-task-lists-main task-lists)))

;; (defun planner-plans-on-task-lists (task-lists)
;;   (mapcar
;;    '(lambda (str)
;;      (let ((task
;;             (if (string-match "\\[\\[\\(\\S\\+\\)\\]\\]" str)
;;                 (replace-match "\\1" t nil str))))
;;      (cons task task)))
;;    (planner-plans-on-task-lists-main task-lists)))

;; test
(testing
 (let ((str "[[TaskBy]]"))
   (if (string-match "\\[\\[\\(\\S\\+\\)\\]\\]" str)
       (replace-match "\\1" t nil str))))

;;test
(testing
 (planner-plans-on-task-lists
  (planner-task-lists (planner-today-ensure-exists))))

(defun planner-plans-on-page (page)
  (planner-plans-on-task-lists
   (planner-task-lists page)))

(defun planner-plans-on-today ()
  (planner-plans-on-page (planner-today-ensure-exists)))

(testing
 (planner-plans-on-today))

;; (defun planner-task-lists-if (test task-lists &key fun)
;;   (loop for task in task-lists          ;lisp is beautiful !!
;;         when (funcall test task)
;;         collect (if fun (funcall fun task) task)))

(defun planner-task-lists-if (test task-lists &key fun)
  (mapcar fun
          (remove-if-not test task-lists)))

;;start: one way to get tasks of plan from one page
(defun task-lists-of-plan-with-status-p (task plan status)
  (and
   (member (concat "[[" plan "]]") (nth 5 task))
   (member (nth 3 task) status)))

(defun planner-tasks-of-plan-from-page (page plan status) ;should be fault tolrent.
  (planner-task-lists-if                         ;else face lot of time waste.
   '(lambda (task)
      (task-lists-of-plan-with-status-p task plan status))
   (planner-task-lists page)
   :fun '(lambda (task-list) (nth 4 task-list))))
;;end

;;test
(testing
 (planner-task-lists-if
  '(lambda (task) (task-list-of-plan-with-status-p task (planner-today-ensure-exists) '("_" "o")))
  (planner-task-lists (planner-today-ensure-exists))
  :fun '(lambda (task-list) (nth 4 task-list))))

;;start: another way to get tasks of plan from one page
(defun task-lists-with-status-p (task status)
  (member (nth 3 task) status))

(defun planner-tasks-from-page (page status)
  (planner-task-lists-if
   '(lambda (task)
      (task-lists-with-status-p task plan status))
   (planner-task-lists page)
   :fun '(lambda (task-list) (nth 4 task-list))))

(defun planner-tasks-of-pages-intersection (plan1 plan2 status)
  (intersection
   (planner-tasks-from-page page1 status)
   (planner-tasks-from-page page2 status)))
;;end

;;test
(testing
 (planner-tasks-of-plan-from-page (planner-today-ensure-exists) "MyMIS" '("_" "o")))


;should be fault tolrent. else face lot of time waste.
(defun planner-tasks-of-plan-today (plan status)
  (planner-tasks-of-plan-from-page
   (planner-today-ensure-exists) plan '("_" "o")))

;;test
(testing
 (planner-tasks-of-plan-today (planner-today-ensure-exists) '("_" "o")))



(defun normalize-task (task)
  (replace-regexp-in-string
   "\\([]\\[]\\)" "\\\\\\1" task))

(defun ci-task (task)
  (let ((task (normalize-task task)))
    (save-excursion
      (save-window-excursion
        (let (buf)
          (setq buf (find-file (concat planner-directory "/" (planner-today-ensure-exists) ".muse")))
          (goto-char 0)
          (re-search-forward task)
          (planner-task-in-progress)
          (save-buffer)
          (bury-buffer buf)
          (kill-buffer buf))))))

(defun co-task (task)
  (let ((task (normalize-task task)))
    (save-excursion
      (save-window-excursion
        (let (buf)
          (setq buf (find-file (concat planner-directory "/" (planner-today-ensure-exists) ".muse")))
          (goto-char 0)
          (re-search-forward task)
          (timeclock-out)
          (save-buffer)
          (bury-buffer buf)
          (kill-buffer buf))))))

(defun stumpwm/planner-task-done (task)
  (let ((task (normalize-task task)))
    (save-excursion
      (save-window-excursion
        (find-file (concat planner-directory "/" (planner-today-ensure-exists) ".muse"))
        (goto-char 0)
        (re-search-forward task)
        (planner-task-done)
        (save-buffer)))))


(defun stumpwm/planner-create-note-from-task (task)
    (let ((task (normalize-task task)))
    ;;   (save-window-excursion
    ;; (save-excursion
      (save-restriction
        (progn
          (find-file
           (concat planner-directory "/" (planner-today-ensure-exists) ".muse"))
          (goto-char 0)
          (re-search-forward task)
          (planner-create-note-from-task)))))
;;))


;; start: http://www.emacswiki.org/emacs/.emacs-thierry.el
;; (add-hook 'after-init-hook #'(lambda ()
;;                                (server-start)
;;                                (setq server-raise-frame t)))

;; emacsclient-and-stumpish (to ".emacsclient-and-stumpish")
;; When using emacsclient from external programs, raise emacs and come back
;; to external program when finish
(if window-system
    (add-hook 'server-done-hook
              (lambda ()
                (shell-command "stumpish 'eval (stumpwm::return-es-called-win stumpwm::*es-win*)'"))))
;; end

(user-provide 'stumpwm)

