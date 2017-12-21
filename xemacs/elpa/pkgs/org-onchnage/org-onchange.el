;;; org-onchnage.el --- copy config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@merunetworks.com>
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

;;; note on change

(require 'desktop)
(require 'session)

(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'org-misc-utils-lotus)


;;;###autoload
(defun org-clock-out-with-note (note &optional switch-to-state fail-quietly at-time) ;BUG TODO will it work or save-excursion save-restriction also required
  (interactive
   (let ((note (read-from-minibuffer "Closing notes: "))
         (switch-to-state current-prefix-arg))
     (list note switch-to-state)))

  (let ((org-log-note-clock-out t))
    (move-marker org-log-note-return-to nil)
    (move-marker org-log-note-marker nil)
    (org-clock-out switch-to-state fail-quietly at-time)
    (remove-hook 'post-command-hook 'org-add-log-note)
    (org-insert-log-note note)))


;; (progn                                  ;old
;;   (defun org-add-log-note-background (&optional _purpose)
;;     "Pop up a window for taking a note, and add this note later."
;;     (remove-hook 'post-command-hook 'org-add-log-note-background)
;;     (setq org-log-note-window-configuration (current-window-configuration))
;;     (delete-other-windows)


;;     (move-marker org-log-note-return-to (point))
;;     ;; (pop-to-buffer-same-window (marker-buffer org-log-note-marker))
;;     ;; (goto-char org-log-note-marker)



;;     (org-switch-to-buffer-other-window "*Org Note*")
;;     (erase-buffer)
;;     (if (memq org-log-note-how '(time state))
;;         (let (current-prefix-arg) (org-store-log-note))
;;         (let ((org-inhibit-startup t)) (org-mode))
;;         (insert (format "# Insert note for %s.
;; # Finish with C-c C-c, or cancel with C-c C-k.\n\n"
;;                         (cond
;;                           ((eq org-log-note-purpose 'clock-out) "stopped clock")
;;                           ((eq org-log-note-purpose 'done)  "closed todo item")
;;                           ((eq org-log-note-purpose 'state)
;;                            (format "state change from \"%s\" to \"%s\""
;;                                    (or org-log-note-previous-state "")
;;                                    (or org-log-note-state "")))
;;                           ((eq org-log-note-purpose 'reschedule)
;;                            "rescheduling")
;;                           ((eq org-log-note-purpose 'delschedule)
;;                            "no longer scheduled")
;;                           ((eq org-log-note-purpose 'redeadline)
;;                            "changing deadline")
;;                           ((eq org-log-note-purpose 'deldeadline)
;;                            "removing deadline")
;;                           ((eq org-log-note-purpose 'refile)
;;                            "refiling")
;;                           ((eq org-log-note-purpose 'note)
;;                            "this entry")
;;                           (t (error "This should not happen")))))
;;         (when org-log-note-extra (insert org-log-note-extra))
;;         (setq-local org-finish-function 'org-store-log-note)
;;         (run-hooks 'org-log-buffer-setup-hook)))

;;   (defun org-add-log-setup-background (&optional purpose state prev-state how extra)
;;     "Set up the post command hook to take a note.
;; If this is about to TODO state change, the new state is expected in STATE.
;; HOW is an indicator what kind of note should be created.
;; EXTRA is additional text that will be inserted into the notes buffer."
;;     (move-marker org-log-note-marker (point))
;;     (setq org-log-note-purpose purpose
;;           org-log-note-state state
;;           org-log-note-previous-state prev-state
;;           org-log-note-how how
;;           org-log-note-extra extra
;;           org-log-note-effective-time (org-current-effective-time))
;;     (add-hook 'post-command-hook 'org-add-log-note-background 'append)))


(progn                                  ;new

  (defun org-add-log-note-background (&optional _purpose)
    "Pop up a window for taking a note, and add this note later."
    ;; (remove-hook 'post-command-hook 'org-add-log-note-background)
    ;; (setq org-log-note-window-configuration (current-window-configuration))
    ;; (delete-other-windows)

    ;; (move-marker org-log-note-return-to (point))
    (org-with-no-active-minibuffer
      (progn
        (message "add-log-note-background: minibuffer already active quitting")
        (message nil))
      (let ((cleanupfn-local #'(lambda () nil)))
        (org-with-timed-new-win
            3 timer cleanupfn-newwin cleanupfn-local win
            (condition-case err
                (let ((target-buffer (get-buffer-create "*Org Note*")))

                  ;; (pop-to-buffer-same-window (marker-buffer org-log-note-marker))
                  ;; (goto-char org-log-note-marker)
                  ;; (org-switch-to-buffer-other-window "*Org Note*")

                  (switch-to-buffer target-buffer 'norecord)
                  (set-buffer target-buffer)
                  (erase-buffer)

                  (if (memq org-log-note-how '(time state))
                      (let (current-prefix-arg) (org-store-log-note))
                      (let ((org-inhibit-startup t)) (org-mode))
                      (insert (format "# Insert note for %s.
# Finish with C-c C-c, or cancel with C-c C-k.\n\n"
                                      (cond
                                        ((eq org-log-note-purpose 'clock-out) "stopped clock")
                                        ((eq org-log-note-purpose 'done)  "closed todo item")
                                        ((eq org-log-note-purpose 'state)
                                         (format "state change from \"%s\" to \"%s\""
                                                 (or org-log-note-previous-state "")
                                                 (or org-log-note-state "")))
                                        ((eq org-log-note-purpose 'reschedule)
                                         "rescheduling")
                                        ((eq org-log-note-purpose 'delschedule)
                                         "no longer scheduled")
                                        ((eq org-log-note-purpose 'redeadline)
                                         "changing deadline")
                                        ((eq org-log-note-purpose 'deldeadline)
                                         "removing deadline")
                                        ((eq org-log-note-purpose 'refile)
                                         "refiling")
                                        ((eq org-log-note-purpose 'note)
                                         "this entry")
                                        (t (error "This should not happen")))))
                      (when org-log-note-extra (insert org-log-note-extra))
                      (setq-local org-finish-function 'org-store-log-note)
                      (run-hooks 'org-log-buffer-setup-hook)))
              ((quit)
               (progn
                 (funcall cleanup win local-cleanup)
                 (if timer (cancel-timer timer))
                 (signal (car err) (cdr err)))))))))

  (defun org-add-log-setup-background (&optional purpose state prev-state how extra)
    "Set up the post command hook to take a note.
If this is about to TODO state change, the new state is expected in STATE.
HOW is an indicator what kind of note should be created.
EXTRA is additional text that will be inserted into the notes buffer."
    (move-marker org-log-note-marker (point))
    (setq org-log-note-purpose purpose
          org-log-note-state state
          org-log-note-previous-state prev-state
          org-log-note-how how
          org-log-note-extra extra
          org-log-note-effective-time (org-current-effective-time))
    (org-add-log-note-background)
    ;; (add-hook 'post-command-hook 'org-add-log-note-background 'append)
    ))

;;;##autoload
;; (defun org-clock-lotus-log-note-current-clock-background (&optional fail-quietly)
;;   (interactive)
;;   (if (org-clocking-p)
;;       (org-clock-lotus-with-current-clock
;;        (org-add-log-setup-background
;;         'note nil nil nil
;;         (concat "# Task: " (org-get-heading t) "\n\n")))
;;       (if fail-quietly (throw 'exit t) (user-error "No active clock"))))

(defun org-clock-lotus-log-note-current-clock-background (&optional fail-quietly)
  (interactive)
  (when (org-clocking-p)
    (move-marker org-log-note-return-to (point))
    (org-clock-lotus-with-current-clock
       (org-add-log-setup-background
        'note nil nil nil
        (concat "# Task: " (org-get-heading t) "\n\n")))))


(defun lotus-buffer-changes-count ()
  (let ((changes 0))
    (when buffer-undo-tree
      (undo-tree-mapc
       (lambda (node)
         (setq changes (+ changes 1;; (length (undo-tree-node-next node))
                          )))
       (undo-tree-root buffer-undo-tree)))
    changes))

(defvar lotus-minimum-char-changes 70)
(defvar lotus-minimum-changes 70)

(defvar lotus-last-buffer-undo-tree-count 0) ;internal add in session and desktop
(when (featurep 'desktop)
  (add-to-list 'desktop-locals-to-save 'lotus-last-buffer-undo-tree-count))
(when (featurep 'session)
  (add-to-list 'session-locals-include 'lotus-last-buffer-undo-tree-count))
(make-variable-buffer-local 'lotus-last-buffer-undo-tree-count)

(defun lotus-action-on-buffer-undo-tree-change (action &optional minimal-changes)
  (let ((chgcount (- (lotus-buffer-changes-count) lotus-last-buffer-undo-tree-count)))
    (if (>= chgcount minimal-changes)
        (if (funcall action)
            (setq lotus-last-buffer-undo-tree-count chgcount))
        (when nil
         (message "buffer-undo-tree-change: only %d changes not more than %d" chgcount minimal-changes)))))

(defvar lotus-last-buffer-undo-list-pos nil) ;internal add in session and desktop
(make-variable-buffer-local 'lotus-last-buffer-undo-list-pos)
(when (featurep 'desktop)
  (add-to-list 'desktop-locals-to-save 'lotus-last-buffer-undo-list-pos))
(when (featurep 'session)
  (add-to-list 'session-locals-include 'lotus-last-buffer-undo-list-pos))
;;;###autoload
(defun lotus-action-on-buffer-undo-list-change (action &optional minimal-char-changes)
  "Set point to the position of the last change.
Consecutive calls set point to the position of the previous change.
With a prefix arg (optional arg MARK-POINT non-nil), set mark so \
\\[exchange-point-and-mark]
will return point to the current position."
  ;; (interactive "P")
  ;; (unless (buffer-modified-p)
  ;;   (error "Buffer not modified"))
  (when (eq buffer-undo-list t)
    (error "No undo information in this buffer"))
  ;; (when mark-point (push-mark))
  (unless minimal-char-changes
    (setq minimal-char-changes 10))
  (let ((char-changes 0)
        (undo-list (if lotus-last-buffer-undo-list-pos
                       (cdr (memq lotus-last-buffer-undo-list-pos buffer-undo-list))
                       buffer-undo-list))
        undo)
    (while (and undo-list
                (car undo-list)
                (< char-changes minimal-char-changes))
      (setq undo (car undo-list))
      (cond
        ((and (consp undo) (integerp (car undo)) (integerp (cdr undo)))
         ;; (BEG . END)
         (setq char-changes (+ char-changes (abs (- (car undo) (cdr undo))))))
        ((and (consp undo) (stringp (car undo))) ; (TEXT . POSITION)
         (setq char-changes (+ char-changes (length (car undo)))))
        ((and (consp undo) (eq (car undo) t))) ; (t HIGH . LOW)
        ((and (consp undo) (null (car undo)))
         ;; (nil PROPERTY VALUE BEG . END)
         ;; (setq position (cdr (last undo)))
         )
        ((and (consp undo) (markerp (car undo)))) ; (MARKER . DISTANCE)
        ((integerp undo))		; POSITION
        ((null undo))		; nil
        (t (error "Invalid undo entry: %s" undo)))
      (setq undo-list (cdr undo-list)))

    (cond
      ((>= char-changes minimal-char-changes)
       (if (funcall action)
           (setq lotus-last-buffer-undo-list-pos undo)))
      (t ))))
(defun org-clock-lotus-log-note-on-change ()
  ;; (when (or t (eq buffer (current-buffer)))
  (if (and
       (consp buffer-undo-list)
       (car buffer-undo-list))
      (lotus-action-on-buffer-undo-list-change #'org-clock-lotus-log-note-current-clock-background  lotus-minimum-char-changes)
      (lotus-action-on-buffer-undo-tree-change  #'org-clock-lotus-log-note-current-clock-background lotus-minimum-changes)))

(defvar org-clock-lotus-log-note-on-change-timer nil)


;; (unintern 'org-clock-lotus-log-note-on-change-timer)

;;;###autoload
(defun org-clock-lotus-log-note-on-change-start-timer ()
  (interactive)
  (if org-clock-lotus-log-note-on-change-timer
      (progn
        (cancel-timer org-clock-lotus-log-note-on-change-timer)
        (setq org-clock-lotus-log-note-on-change-timer nil)))
  (setq
   org-clock-lotus-log-note-on-change-timer (run-with-idle-timer 10 10 'org-clock-lotus-log-note-on-change)))

;;;###autoload
(defun org-clock-lotus-log-note-on-change-stop-timer ()
  (interactive)
  (if org-clock-lotus-log-note-on-change-timer
      (progn
        (cancel-timer org-clock-lotus-log-note-on-change-timer)
        (setq org-clock-lotus-log-note-on-change-timer nil))))

;;;###autoload
(defun org-clock-lotus-log-note-on-change-insinuate ()
  (interactive)
  ;; message-send-mail-hook
  (org-clock-lotus-log-note-on-change-start-timer))

;;;###autoload
(defun org-clock-lotus-log-note-on-change-uninsinuate ()
  (interactive)
  ;; message-send-mail-hook
  (org-clock-lotus-log-note-on-change-stop-timer))

(provide 'org-onchnage)
;;; org-onchnage.el ends here
