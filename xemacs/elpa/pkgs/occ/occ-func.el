
(require 'occ-base-objects)

(defun occ-make-task-at-point (builder)
  ;; (org-element-at-point)
  (let (task
        (heading-with-string-prop
         (unless (org-before-first-heading-p)
           (org-get-heading 'notags))))
    (let ((heading (if heading-with-string-prop
                       (substring-no-properties heading-with-string-prop)))
          (heading-prop (if heading-with-string-prop
                            heading-with-string-prop))
          (marker  (move-marker
                    (make-marker)
                    (point)
                    (org-base-buffer (current-buffer))))
          (file    (buffer-file-name))
          (point   (point))
          (clock-sum (if (org-before-first-heading-p)
                         0
                         (org-clock-sum-current-item)))
          (task (cadr (org-element-at-point))))
      (when heading
        (setf task
              (funcall builder
                       :heading heading
                       :file file
                       :point point
                       :heading-prop heading-prop
                       :clock-sum clock-sum))

        (let ((inherited-props (org-context-clock-keys-with-operation :getter nil)))
          (dolist (prop inherited-props)
            (let* ((propstr (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))
                   (val (org-entry-get nil propstr t)))
              (unless (org-context-clock-task-get-property task prop)
                (org-context-clock-task-set-property task prop val))))))
      task)))

;; (defun org-task-collect-task-clock-info ()
(defun occ-make-task-from-clock ()
  ;; NOT used anywhere
  ;; (org-element-at-point)
  (let ((heading-with-string-prop
         (unless (org-before-first-heading-p)
           (org-get-heading 'notags))))
    (let ((heading (if heading-with-string-prop
                       (substring-no-properties heading-with-string-prop)))
          (heading-prop (if heading-with-string-prop
                            heading-with-string-prop))
          (marker  (move-marker
                    (make-marker)
                    (point)
                    (org-base-buffer (current-buffer))))
          (file    (buffer-file-name))
          (point   (point))
          (clock-sum (if (org-before-first-heading-p)
                         0
                         (org-clock-sum-current-item)))
          (task (cadr (org-element-at-point)))
          (task-content-start ))
      (when heading
        ;; (if root   (push (cons "Root" root) task))
        (if marker    (org-context-clock-task-set-property task :task-clock-marker marker))
        (if file      (org-context-clock-task-set-property task :task-clock-file file))
        (if point     (org-context-clock-task-set-property task :task-clock-point point))
        (if heading   (org-context-clock-task-set-property task :task-clock-heading heading))
        (if heading-prop   (org-context-clock-task-set-property task :task-clock-heading-prop heading-prop))
        (if clock-sum (org-context-clock-task-set-property task :task-clock-clock-sum clock-sum))
        (let ((inherited-props (org-context-clock-keys-with-operation :getter nil)))
          (dolist (prop inherited-props)
            (let* ((propstr (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))
                   (val (org-entry-get nil propstr t)))
              (unless (org-context-clock-task-get-property task prop)
                (org-context-clock-task-set-property task prop val)))))
        (if heading-with-string-prop
            (org-context-clock-task-set-property task :task-clock-content (org-context-clock-heading-content-only))))
      task)))



(provide 'occ-func)
