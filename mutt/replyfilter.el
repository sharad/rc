
(defun reply-backward-kill-empty-line ()
  (let ((kill-whole-line t))            ;irresepctive whatever set.
    ( and (re-search-backward "^\\([ \t]*>[ \t]*\\)+$" nil t)
          (kill-line) ;delete whole line.
      )))

(defun reply-kill-empty-line-region (beg end)
  "remove empty lines in reply mail"
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (and
            (< beg (point))
            (reply-backward-kill-empty-line)))))


(defun mark-salute ()
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point."
  (interactive)
  (push-mark (point))
  ;;(end-of-defun)
  (re-search-forward "^\\(>[ \t]*\\)+\\(Regard\\).+$")
  (beginning-of-line)
  (push-mark (point) nil t)
  (re-search-forward "^\\(>[ \t]*\\)+\\(Regard\\).+$"))
  ;; (re-search-backward "^\n" (- (point) 1) t))

;; (defvar )
;; (defun reply-kill-learn-seg (beg end)
;;   "remove empty lines in reply mail"
;;   (interactive "r")
;;   (buffer-substring beg end)
  
;; )

;; (defun reply-brief-region (beg end)
;;   ""
;;   (interactive "r")
;;   (while (< beg (point))
;;     (setq end (progn
;;                 (while (reply-kill-empty-line) ()) ; wrong
;;                 (point)))  ;till empty line delete.
;;     ))

;; (defun reply-read-file ()
;;   (reply-kill-empty-line-region (point-min) (point-max)))

;; (add-hook 'read-file-hooks 'reply-read-file)

