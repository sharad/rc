
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
          (task-plist (cadr (org-element-at-point))))
      (when heading
        (setf task
              (funcall builder
                       :heading heading
                       :file file
                       :point point
                       :heading-prop heading-prop
                       :clock-sum clock-sum
                       :plist task-plist))

        (let ((inherited-props (org-context-clock-keys-with-operation :getter nil)))
          (dolist (prop inherited-props)
            (let* ((propstr (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))
                   (val (org-entry-get nil propstr t)))
              (unless (plist-get (aref task :plist) prop)
                (plist-put (aref task :plist) prop val))))))
      task)))

;; (defun org-task-collect-task-clock-info ()
(defun occ-make-task-from-clock (builder)
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
          (task-plist (cadr (org-element-at-point)))
          (task-content-start ))
      (when heading
        (setf task
              (funcall builder
                       :heading heading
                       :file file
                       :point point
                       :heading-prop heading-prop
                       :clock-sum clock-sum
                       :plist task-plist))
        (let ((inherited-props (org-context-clock-keys-with-operation :getter nil)))
          (dolist (prop inherited-props)
            (let* ((propstr (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))
                   (val (org-entry-get nil propstr t)))
              (unless (plist-get (aref task :plist) prop)
                (plist-put (aref task :plist) prop val)))))
        (if heading-with-string-prop
            (plist-put (aref task :plist) :task-clock-content (org-context-clock-heading-content-only))))
      task)))



(provide 'occ-func)
