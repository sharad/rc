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

;; Enable Activity for the current buffer by invokingi
;; `activity-mode'. If you wish to activate it globally, use
;; `global-activity-mode'.

;; Set variable `activity-api-key' to your API key. Point
;; `activity-cli-path' to the absolute path of the CLI script
;; (activity-cli.py).

;;; Code:

(require '@)

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


(defvar @activity
  (@extend :name "Class Activity" :occuredon (current-time)))

(def@ @activity :log ()
      (message "Time %s" @:occuredon))

(def@ @activity :message ()
      (error "No :message function found."))

(defvar @dispatchable (@extend :name "Class Dispatchable"))

(def@ @dispatchable :dispatch ()
      ())

(defvar @defferred-dispatchable (@extend :name "Class Deferred Dispatchable"))


(defvar @transition (@extend :name "Class Transition")
  )



(defvar @buffer-activity
  (@extend @activity
           :name "Class Buffer Activity"
           :buffer (current-buffer)))

(defvar @clock-activity
  (@extend @buffer-activity
           :clock-marker nil
           :heading nil))

(def@ @clock-activity :setclock-marker (marker)
      (@! @:clock-marker marker))

(defvar @clock-in-activity
  (@extend @clock-activity
           :prev-clock nil))

(defvar @clock-out-activity
  (@extend @clock-activity
           :next-clock nil))

(def@ @clock-out-activity :message ()
      (if @:next-clock
          (format
           "clocking out from [%s] to clocking in to [%s]"
           @:heading
           (@! @:next-clock :headign))
          (format
           "clocking out from [%s]"
           @:heading)))

(def@ @clock-out-activity :init ()
      (message "test1"))

;; (setf test (@! @clock-out-activity :new))

;; (@! @clock-out-activity :message)


;; (call-next-method)

(defun activity-save ()
  "Send save notice to Activity."
  (when (buffer-file-name (current-buffer))
    ))

(defun activity-bind-hooks ()
  "Watch for activity in buffers."
  ;; (add-hook 'after-save-hook 'activity-save nil t)
  ;; (add-hook 'auto-save-hook 'activity-save nil t)
  ;; (add-hook 'first-change-hook 'activity-ping nil t)
  )

(defun activity-unbind-hooks ()
  "Stop watching for activity in buffers."
  ;; (remove-hook 'after-save-hook 'activity-save t)
  ;; (remove-hook 'auto-save-hook 'activity-save t)
  ;; (remove-hook 'first-change-hook 'activity-ping t)
  )

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
  :lighter    " act"
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





(progn
  (defvar @squares (@extend))

  (def@ @squares :get (property)
        (if (numberp property)
            (expt property 2)
            (@^:get property)))  ; explained in a moment

  (mapcar (lambda (n) (@ @squares n)) '(0 1 2 3 4))
                                        ; => (0 1 4 9 16)



  )

(provide 'activity)
;;; activity.el ends here
