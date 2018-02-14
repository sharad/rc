
;; https://emacs.stackexchange.com/questions/1030/how-can-i-set-different-font-sizes-for-buffers-and-for-the-mode-line
(defun lotus-set-condense-mode-line-face ()
  (interactive)
  (let ((faces '(mode-line
                 mode-line-buffer-id
                 mode-line-emphasis
                 mode-line-highlight
                 mode-line-inactive)))
    (mapc
     #'(lambda (face)
         (set-face-attribute face nil
                             :font (face-attribute 'default :font)
                             :height 110))
     faces)))
