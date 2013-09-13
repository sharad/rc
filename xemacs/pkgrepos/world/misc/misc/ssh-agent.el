;; run ssh-agent from within emacs 
;;
;; Copyright (C) 2003 Will Glozer
;; Copyright (C) 2009 Christophe Rhodes
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59
;; Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;; ----------------------------------------------------------------------
;;
;; Run ssh-agent and ssh-add as emacs processes, setting the proper env
;; variables, and accepting password input

(defcustom ssh-agent-buffer "*ssh-agent*"
  "buffer to display ssh-agent output in"
  :type 'string
  :group 'ssh-agent)

(defcustom ssh-agent-program "ssh-agent"
  "ssh-agent program"
  :type 'string
  :group 'ssh-agent)

(defcustom ssh-add-program "ssh-add"
  "ssh-add program"
  :type 'string
  :group 'ssh-agent)

(defcustom ssh-add-prompt "Enter passphrase for \\([^:]+\\):"
  "ssh-add prompt for passphrases"
  :type 'string
  :group 'ssh-agent)

(defcustom ssh-add-invalid-prompt "Bad passphrase, try again:"
  "ssh-add prompt indicating an invalid passphrase"
  :type 'string
  :group 'ssh-agent)

(defun ssh-agent (&optional cmd)
  "execute the ssh agent"
  (interactive (list (if current-prefix-arg
			 (read-string "Run ssh-agent: " ssh-agent-program)
		       ssh-agent-program)))
  (unless cmd
    (setq cmd ssh-agent-program))
  (let ((args (split-string cmd)))
    (if args
	(set-process-filter
	 (apply #'start-process "ssh-agent" nil (first args) (rest args))
	 #'ssh-agent-process-filter)
      (error "No command given"))))

(defun ssh-add (&optional cmd)
  "run ssh-add"
  (interactive (list (if current-prefix-arg
			 (read-string "Run ssh-add: " ssh-add-program)
		       ssh-add-program)))
  (unless cmd
    (setq cmd ssh-add-program))
  (let ((args (split-string cmd)))
    (if args
	(set-process-filter
	 (apply #'start-process "ssh-add" nil (first args) (rest args))
	 #'ssh-add-process-filter)
      (error "No command given"))))

(defun ssh-agent-process-filter (process input)
  "filter for ssh-agent input"
  (cond ((ssh-agent-read-var "SSH_AUTH_SOCK" input)
         (ssh-add))
        ((ssh-agent-read-var "SSH_AGENT_PID" input))))

(defun ssh-agent-read-var (var line)
  "read a shell script variable from ssh-agent's output"
  (if (string-match (format "%s[= ]\\([^;]+\\)" var) line)
      (with-current-buffer (get-buffer-create ssh-agent-buffer)
        (let ((value (match-string 1 line)))
          (setenv var value)
          (insert line))
        t)))

(defun ssh-add-process-filter (process input)
  "filter for ssh-add input"
  (cond ((string-match ssh-add-prompt input)
         (ssh-send-passwd process input))
        ((string-match ssh-add-invalid-prompt input)
         (ssh-send-passwd process input))
        (t (with-current-buffer (get-buffer-create ssh-agent-buffer)
             (insert input)))))

(defun ssh-send-passwd (process prompt)
  "read a passphrase with `read-passwd` and pass it to the ssh-add process"
  (let ((passwd (read-passwd prompt)))
    (process-send-string process passwd)
    (process-send-string process "\n")
    (clear-string passwd)))

(provide 'ssh-agent)