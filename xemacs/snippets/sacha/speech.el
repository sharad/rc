


;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgd610701

(defvar my/espeak-command "espeak")
(defun my/say (string &optional speed)
  (interactive "MString: ")
  (let ((speed (or speed 175)))
    (call-process my/espeak-command nil nil nil string "-s" (number-to-string speed))))
