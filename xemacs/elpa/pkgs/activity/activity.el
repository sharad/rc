;;; activity.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d@gmail.com>
;; Keywords: data

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

;; This package meant to log, analyze and report all emacs activity of
;; user which could further utilized to visualize activity of user
;; during period of time or editing session.

;; Enable Activity for the current buffer by invoking
;; `activity-mode'. If you wish to activate it globally, use
;; `global-activity-mode'.

;; Set variable `activity-api-key' to your API key. Point
;; `activity-cli-path' to the absolute path of the CLI script
;; (activity-cli.py).

;;; Code:

(defconst activity-version    "1.0.2")
(defconst activity-user-agent "emacs-activity")
(defvar activity-noprompt      nil)
(defvar activity-init-started  nil)
(defvar activity-init-finished nil)
(defvar activity-python-path   nil)

(defgroup activity nil
  "Customizations for Activity"
  :group 'convenience
  :prefix "activity-")

(defclass activity () ; No superclasses
  ((name :initarg :name
         :initform ""
         :type string
         :custom string
         :documentation "The name of a activity.")
   (occurredon
    :initarg :occurredon
    :initform (current-time)
    :custom list
    :type list
    :documentation "Activity occurrence time."))
  "An activity for tracking."
  :abstract t)


;; (defclass name superclass slots &rest options-and-doc)

(defmethod call-activity ((act activity) &optional scriptname)
  "Dial the phone for the actord ACT.
   Execute the program SCRIPTNAME as to dial the phone."
  (message "Dialing the phone for %s"  (oref act name)))

(defclass buffer-activity (activity)
  ((buffer :initarg :buffer
           :initform (current-buffer)
           :type buffer
           :documentation "Current buffer."))
  "A buffer activity.")

(defclass mail-activity (buffer-activity)
  ((message-id :initarg :message-id
               :initform (current-buffer)
               :type string
               :documentation "message-id.")
   (to :initarg :to
       :initform (current-buffer)
       :type string
       :documentation "to")
   (from :initarg :from
         :initform (current-buffer)
         :type string
         :documentation "from")
   (subject :initarg :subject
            :initform (current-buffer)
            :type string
            :documentation "subject")
   (brief-body :initarg :brief-body
               :initform (current-buffer)
               :type string
               :documentation "brief-body")))


(defmethod call-activity :before ((act buffer-activity) &optional scriptname)
  "Prepend country code to phone number, then dial the phone for REC.
   Execute the program SCRIPTNAME as to dial the phone"
  (message "Prepending country code to phone number.")
  (unless (string-match "^00" (oref act :phone))
    (let ((country (oref act :country)))
      (cond
        ;; just an example...
        ((string= country "IT")
         (oset act :phone (concat "0043" (oref act :phone))))))))

(defmethod record-org :before ((act buffer-activity) &optional scriptname)
  "Prepend country code to phone number, then dial the phone for REC.
   Execute the program SCRIPTNAME as to dial the phone"
  (message "Prepending country code to phone number.")
  (unless (string-match "^00" (oref act :phone))
    (let ((country (oref act :country)))
      (cond
        ;; just an example...
        ((string= country "IT")
         (oset act :phone (concat "0043" (oref act :phone))))))))

(defmethod record-elisp :before ((act buffer-activity) &optional scriptname)
  "Prepend country code to phone number, then dial the phone for REC.
   Execute the program SCRIPTNAME as to dial the phone"
  (message "Prepending country code to phone number.")
  (unless (string-match "^00" (oref act :phone))
    (let ((country (oref act :country)))
      (cond
        ;; just an example...
        ((string= country "IT")
         (oset act :phone (concat "0043" (oref act :phone))))))))

(defmethod record-log-note :before ((act buffer-activity) &optional scriptname)
  "Prepend country code to phone number, then dial the phone for REC.
   Execute the program SCRIPTNAME as to dial the phone"
  (message "Prepending country code to phone number.")
  (unless (string-match "^00" (oref act :phone))
    (let ((country (oref act :country)))
      (cond
        ;; just an example...
        ((string= country "IT")
         (oset act :phone (concat "0043" (oref act :phone))))))))

(defmethod record-log-note-interact :before ((act buffer-activity) &optional scriptname)
  "Prepend country code to phone number, then dial the phone for REC.
   Execute the program SCRIPTNAME as to dial the phone"
  (message "Prepending country code to phone number.")
  (unless (string-match "^00" (oref act :phone))
    (let ((country (oref act :country)))
      (cond
        ;; just an example...
        ((string= country "IT")
         (oset act :phone (concat "0043" (oref act :phone))))))))


(when nil
  ;; This function just prepends the country code for Italy if necessary. If you
  ;; think all this 'oset' and 'oref' stuff is tedious - you are right. But you
  ;; can define so called "accessor" functions: in the slot definition you can
  ;; write

  ;; :accessor get-phone

  (progn
    (setq act
          (buffer-activity "rand" :name "Random Sample"))

    (buffer-activity-p act)

    (oref act :buffer)

    (oset act :phone "555-5566")
    (oref act :phone)

    (call-activity act)

    (setq buffact (buffer-activity "friend" :name "Good Friend" :birthday "01/01/2000" :phone "555-5555" :country "IT"))

    (call-activity buffact)

    (eieio-customize-object act)))


;; (call-next-method)

(defun activity-save ()
  "Send save notice to Activity."
  (when (buffer-file-name (current-buffer))
    ))

(defun activity-bind-hooks ()
  "Watch for activity in buffers."
  (add-hook 'after-save-hook 'activity-save nil t)
  (add-hook 'auto-save-hook 'activity-save nil t)
  (add-hook 'first-change-hook 'activity-ping nil t))

(defun activity-unbind-hooks ()
  "Stop watching for activity in buffers."
  (remove-hook 'after-save-hook 'activity-save t)
  (remove-hook 'auto-save-hook 'activity-save t)
  (remove-hook 'first-change-hook 'activity-ping t))

(defun activity-turn-on (defer)
  "Turn on Activity."
  (if defer
    (run-at-time "1 sec" nil 'activity-turn-on nil)
    (let ()
      (activity-init)
      (if activity-init-finished
        (activity-bind-hooks)
        (run-at-time "1 sec" nil 'activity-turn-on nil)))))

(defun activity-turn-off ()
  "Turn off Activity."
  (activity-unbind-hooks))

;;;###autoload
(define-minor-mode activity-mode
  "Toggle Activity (Activity mode)."
  :lighter    " waka"
  :init-value nil
  :global     nil
  :group      'activity
  (cond
    (noninteractive (setq activity-mode nil))
    (activity-mode (activity-turn-on t))
    (t (activity-turn-off))))

;;;###autoload
(define-globalized-minor-mode global-activity-mode activity-mode
  (lambda () (activity-mode 1)))

(define-minor-mode activity-mode
      "Prepare for working with collarative office project. This
is the mode to be enabled when I am working in some files on
which other peoples are also working."
    :initial-value nil
    :lighter " Act"
    :global t
    (condition-case e
        (when office-mode
          (message "calling office mode")
          (if (or (eq major-mode 'c-mode)
                  (eq major-mode 'c++-mode))
              (c-set-style "stroustrup" 1))
          (set (make-local-variable 'before-save-hook) before-save-hook)
          (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
          (message "called office mode"))
      (error (message "Error: %s" e))))


(provide 'activity)
;;; activity.el ends here
