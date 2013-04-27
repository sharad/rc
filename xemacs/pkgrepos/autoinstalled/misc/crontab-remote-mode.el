;Written by Ch. Tronche (http://tronche.com/)
;Copyright by the author. This is unmaintained, no-warranty free software.
;Please use freely. It is appreciated (but by no means mandatory) to
;acknowledge the author's contribution. Thank you.

;; Major mode for editing a crontab file.

;; put this in your .emacs:
;;
;; (autoload 'crontabr-mode "crontabr-mode.el" "Major mode for editing your crontabr file." t)

(require 'telnet)

(defvar crontabr-output-buffer "*Shell Command Output*")
(defvar crontabr-buffer-name "crontabr")

(defun crontabr-mode (remote-crontabr)
  "Major mode for editing your crontabr file.

When invoked with an argument, prompt for the name of a machine and
edit the crontabr file on that machine.

When you've finished, you can type
\\[crontabr-send-buffer] (`crontabr-send-buffer') to redefine your
crontabr file.
"
  (interactive "P")
  (let (buffer-name remote-host crontabr-buffer-existed remote-extension crontabr-rsh)
    (if remote-crontabr
	(setq remote-host (read-string "remote host: ")
	      	    remote-extension (concat "-" remote-host)
		    crontabr-rsh (concat remote-shell-program " " remote-host " ")))
    (setq buffer-name (concat "*" crontabr-buffer-name remote-extension "*")
	  crontabr-buffer-existed (get-buffer buffer-name))
    (switch-to-buffer buffer-name)
    (setq default-directory (expand-file-name "~/"))
    (auto-save-mode auto-save-default)
    (if crontabr-buffer-existed
	()
      (shell-command (concat crontabr-rsh "crontabr -l") t)
      (make-variable-buffer-local 'crontabr-put-command)
      (setq crontabr-put-command (concat crontabr-rsh "crontabr"))
      (set-buffer-modified-p ())
      (setq major-mode 'crontabr-mode)
      (setq mode-name "Crontabr")
      (local-set-key "\C-c\C-c" 'crontabr-send-buffer)
      (setq buffer-auto-save-file-name "#" buffer-name "#")
      (run-hooks 'crontabr-mode-hook))))

(defun crontabr-send-buffer ()
  (interactive)
  (if (or (buffer-modified-p)
	  (yes-or-no-p "crontabr not modified, still send it ? "))
      (progn
	(shell-command-on-region (point-min) (point-max) crontabr-put-command)
	(let ((errorp))
	  (if (get-buffer crontabr-output-buffer)
	      (save-excursion
		(set-buffer crontabr-output-buffer)
		(goto-char (point-min))
		(setq errorp (search-forward "error" () t))))
	  (if errorp
	      ()
	    (set-buffer-modified-p ())
	      (bury-buffer))))))

(provide 'crontab-remote-mode)
