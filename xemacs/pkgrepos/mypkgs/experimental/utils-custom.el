;;; utils-custom.el --- Utils custom                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:



(require 'cl)

(eval-when-compile
  '(require 'cl))




(defvar resume-workdir "/home/s/paradise/Projects/doc/resume" "resume work dir.")

(defvar tags-from-resume nil "Tags from resume")

(defun tags-from-resume (prompt)
  (if tags-from-resume
      tags-from-resume
      (let ((resume-make-keys (format "make -sC %s resume=%s keys" resume-workdir "sharad")))
        (read-string prompt (shell-command-to-string resume-make-keys)))))



(defun insert-reply-object (resume object &optional keys attachment type discription)
  "Prepare reply object"
  (interactive
   (let*
       ((resume (read-from-minibuffer "who: " "sharad"))
        (object (read-from-minibuffer "object: " "resume"))
        (resume-make-keys (format "make -sC %s name=%s object=%s keys" resume-workdir resume object))
        (keys   (read-string "keys: " (shell-command-to-string resume-make-keys)))
        (type (or (read-from-minibuffer "type: " "pdf") "txt"))
        ;(attachment (y-or-n-p (format "make inline: ")))
        (discription "attachment")
        attachment)
     ;; (list resume object keys attachment type disposition)))
     (list resume object keys attachment type description)))
  (let* ((resume-make-keys (format "make -sC %s name=%s object=%s keys" resume-workdir resume object))
         (keys (or keys  (read-string "keys: " (shell-command-to-string resume-make-keys))))
         (keys-of-resume-name (mapconcat 'identity (sort (split-string keys) #'string-lessp) "-"))
         (resume-make-cmd (format "make -C %s name=%s object=%s filter_targets='%s' %s link%s" resume-workdir resume object keys type type))
         (resume-attachable-file (format "%s/output/%s-%s.%s" resume-workdir resume object type))
         (resume-actual-file (format "%s/output/%s-%s-%s.%s" resume-workdir resume object keys-of-resume-name type))
         (resume-view-cmd (format "%s %s" "evince" resume-actual-file)))
    (message "preparing %s by: %s" object resume-make-cmd)
    (if (and
           (shell-command resume-make-cmd)
           (if attachment
               (shell-command resume-view-cmd) t)
           (file-exists-p resume-actual-file))
      (if attachment
          (mml-attach-file
           resume-attachable-file
           (mm-default-file-encoding resume-attachable-file)
           ;; (mml-minibuffer-read-type resume-attachable-file) ; "application/pdf"
           discription "inline")
          (insert-file-contents resume-actual-file))
      (message "Not able to %s %s."
               (if attachment "attach" "insert")
               object))))

;; (interactive
;;    (let* ((file (mml-minibuffer-read-file "Attach file: "))
;; 	  (type (mml-minibuffer-read-type file))
;; 	  (description (mml-minibuffer-read-description))
;; 	  (disposition (mml-minibuffer-read-disposition type nil file)))
;;      (list file type description disposition)))

;; (let* ((file (mml-minibuffer-read-file "Attach file: "))
;;        (type (mml-minibuffer-read-type file))))


;; test
;; (trace-function 'attach-resume)
;; (untrace-function 'delete-file)
;; (trace-function 'make-symbolic-link)
;; (delete-file "asfasdf")


;;;###autoload
(defun lotus-read-file (filename)
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (let ((contents
             (condition-case e
                 ;; (read (current-buffer))
                 (buffer-string)
               ('end-of-file nil))))
        contents))))

(defun lotus-read-sexp (filename)
  (when (file-exists-p filename)
    (car (read-from-string (lotus-read-file filename)))))


(defun lotus-write-file (filename content)
  (with-current-buffer (or (find-buffer-visiting filename)
                           (find-file-noselect filename))
    (set-buffer-file-coding-system
     (if (coding-system-p 'utf-8-emacs)
         'utf-8-emacs
         'emacs-mule))
    (erase-buffer)
    (insert content)
    (write-file filename)))

(defun lotus-write-append-file (filename content)
  (when (file-exists-p filename)
    (write-region content nil filename t)
    (put-file-in-rcs filename)))

;;{{ already present in tramp-remote-path
;; have to add into .profile
;; (add-to-list 'tramp-remote-path "/usr/local/bin")
;; (add-to-list 'tramp-remote-path "~/bin")
;;}}

;; tramp-file-name-handler-alist
;;     ;; `executable-find' is not official yet.
;;     (executable-find . tramp-handle-executable-find)
;;     (start-file-process . tramp-handle-start-file-process)



(defun shell-command-no-output (command)
  ;; (interactive "scommand: ")
  (let ((handler
         (find-file-name-handler (directory-file-name default-directory)
                                 'shell-command)))
    (if (string-match "[ \t]*&[ \t]*\\'" command)
        (let ((directory default-directory)
              proc)
          ;; Remove the ampersand.
          (setq command (substring command 0 (match-beginning 0)))
          (setq default-directory directory)
          (setq proc (start-file-process "Shell" nil shell-file-name
                                         shell-command-switch command))
          (set-process-sentinel proc 'shell-command-sentinel)
          ;; Use the comint filter for proper handling of carriage motion
          ;; (see `comint-inhibit-carriage-motion'),.
          (set-process-filter proc 'comint-output-filter))
        (ignore-errors
          (equal 0
                 (if handler
                     ;;(process-file-shell-command
                     ;; (funcall handler 'shell-command command nil nil)
                     ;; (start-file-process :shcommand1 nil command)
                     ;; (call-process shell-file-name nil nil nil "-c" command)
                     ;; (start-file-process "shcommand1" nil shell-file-name "-c" command)
                     (process-file shell-file-name nil nil nil shell-command-switch command)
                     (call-process shell-file-name nil nil nil shell-command-switch command)))))))

(defun shell-command-local-no-output (cmd)
  ;; (interactive "scmd: ")
  (let ((default-directory "~/"))
    (shell-command-no-output cmd)))

;; (defun shell-command-local-no-output (cmd)
;;   ;; (interactive "scmd: ")
;;   (equal 0 (call-process shell-file-name nil nil nil "-c" cmd)))


(defun messageto (buf &rest text)
  (with-current-buffer (get-buffer-create buf)
    (funcall 'message (concat
                      (if (stringp buf) buf (buffer-name buf))
                      ": "
                      (apply 'concat text)))
    (apply 'insert text)
    (insert "\n")))

(messageto "*Complains*" "Bookmarking fecility, may consider Org mode which is unexpanded.")


(defcustom recentf-save-file-modes 384 ;; 0600
  "Mode bits of recentf save file, as an integer, or nil.
If non-nil, after writing `recentf-save-file', set its mode bits to
this value.  By default give R/W access only to the user who owns that
file.  See also the function `set-file-modes'."
  :group 'recentf
  :type '(choice (const :tag "Don't change" nil)
          integer))


(defcustom recentf-save-file (convert-standard-filename "~/.recentf")
  "File to save the recent list into."
  :group 'recentf
  :type 'file
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (let ((oldvalue (eval symbol)))
           (custom-set-default symbol value)
           (and (not (equal value oldvalue))
                recentf-mode
                (recentf-load-list)))))


(defconst recentf-save-file-coding-system
  (if (coding-system-p 'utf-8-emacs)
      'utf-8-emacs
    'emacs-mule)
  "Coding system of the file `recentf-save-file'.")

(defun recentf-save-list ()
  "Save the recent list.
Write data into the file specified by `recentf-save-file'."
  (interactive)
  (condition-case error
      (with-temp-buffer
        (erase-buffer)
        (set-buffer-file-coding-system recentf-save-file-coding-system)
        (insert (format recentf-save-file-header (current-time-string)))
        (recentf-dump-variable 'recentf-list recentf-max-saved-items)
        (recentf-dump-variable 'recentf-filter-changer-current)
        (insert "\n\n;; Local Variables:\n"
                (format ";; coding: %s\n" recentf-save-file-coding-system)
                ";; End:\n")
        (write-file (expand-file-name recentf-save-file))
        (when recentf-save-file-modes
          (set-file-modes recentf-save-file recentf-save-file-modes))
        nil)
    (error
     (warn "recentf mode: %s" (error-message-string error)))))


(progn ;; "trim-string"
  ;; http://xahlee.blogspot.in/2011/09/emacs-lisp-function-to-trim-string.html
  (defun trim-string (string)
    "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
    (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))))

(progn ;; "library utils"
  (defun find-library-directory (lib)
    (file-name-directory (let (nosuffix)
                           (locate-file lib
                                        load-path
                                        (append (unless nosuffix (get-load-suffixes))
                                                load-file-rep-suffixes))))))

(progn ;; notify
  (if (require 'notify nil t)
      (defun message-notify (title fmt &rest args)
        (let ((msg (apply 'format fmt args)))
          (message "%s: %s" title msg)
          (ignore-errors
            (notify title msg))
          msg))
      (defun message-notify (title fmt &rest args)
        (let ((msg (apply 'format fmt args)))
          (message "%s: %s" title msg)
          ;; (notify title msg)
          msg))))


(progn ;; "have-x-focus"
  (defun have-x-focus ()
    "Runs on-blur-hook if emacs has lost focus."
    (if (and
         (featurep 'x)
         window-system)
        (let* ((active-window (x-window-property
                               "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
               (active-window-id (if (numberp active-window)
                                     active-window
                                     (string-to-number
                                      (format
                                       "%x%x"
                                       ; "%x00%x"
                                       (car active-window)
                                       (cdr active-window)) 16)))
               (emacs-window-id (string-to-number
                                 (frame-parameter nil 'outer-window-id))))
          ;; (message "emacs-window-id %d active-window-id %d" emacs-window-id active-window-id)
          (= emacs-window-id active-window-id))
        (message "Not in Graphical Window system.")))

  (when nil
    (have-x-focus)
    (progn
      (sleep-for 4)
      (list (selected-frame)
            (have-x-focus)))))



(progn ;; "setenv-from-file"
  (defun setenv-from-file (file &optional buses)
    (let ((buses (if (consp buses) buses (list buses))))
      (if (file-exists-p file)
          (mapc
           (lambda (ev)
             (let ((p (position ?\= ev)))
               (setenv (substring ev 0 p)
                       (substring ev (1+ p)))
               (when (consp buses)
                 (dolist (bus buses)
                   (ignore-errors
                     (dbus-setenv
                      bus
                      (substring ev 0 p)
                      (substring ev (1+ p))))))))
           (remove-if-not (lambda (l)
                            (and (not (string-match "^#" l))
                                 (string-match "\\w+=\\w+" l)))
                          (split-string (lotus-read-file file) "\n")))))))



(progn ;; "debugging"
  (defun backtrace-to-buffer (buf)
    ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Internals-of-Debugger.html
    (with-output-to-temp-buffer buf ; "backtrace-output"
      (let ((var 1))
        (save-excursion
          (setq var (eval '(progn
                            (if (boundp 'var) (1+ var))
                            (list 'testing (backtrace))))))))))


(defun assq-delete-all-test (key alist &optional testf)
  "Delete from ALIST all elements whose car is `eq' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (let ((testf (or testf 'eq)))
    (while (and (consp (car alist))
                (funcall testf (car (car alist)) key))
      (setq alist (cdr alist)))
    (let ((tail alist)
          tail-cdr)
      (while (setq tail-cdr (cdr tail))
        (if (and (consp (car tail-cdr))
                 (funcall testf (car (car tail-cdr)) key))
            (setcdr tail (cdr tail-cdr))
            (setq tail tail-cdr))))
    alist))

(defun rassq-delete-all-test (value alist &optional testf)
  "Delete from ALIST all elements whose cdr is `eq' to VALUE.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (let ((testf (or testf 'eq)))
    (while (and (consp (car alist))
                (funcall testf (cdr (car alist)) value))
      (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
        (if (and (consp (car tail-cdr))
                 (eq (cdr (car tail-cdr)) value))
            (setcdr tail (cdr tail-cdr))
            (setq tail tail-cdr))))
    alist))


(provide 'utils-custom)
;;; utils-custom.el ends here
