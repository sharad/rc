;;; packages.el --- lotus-cal-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-cal-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-cal-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-cal-org/pre-init-PACKAGE' and/or
;;   `lotus-cal-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-cal-orgS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-cal-org-packages
  '(
    calendar
    (g :location local) ;; http://bc.tech.coop/blog/070306.html
    )
  "The list of Lisp packages required by the lotus-cal-org layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-cal-org/init-calendar ()
  (use-package calendar
      :defer t
      :config
      (with-eval-after-load "org"
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; from http://tsdh.wordpress.com/2008/07/03/some-calendar-and-org-mode-integration-stuff/
        ;; Some calendar and org-mode integration stuff
        ;; 03Jul08

        ;; The following code binds RET in calendar mode to a function that opens
        ;; an org agenda buffer for that day (or better that week).

        (defun th-calendar-open-agenda ()
          (interactive)
          (let* ((calendar-date (or
                                 ;; the date at point in the calendar buffer
                                 (calendar-cursor-to-date)
                                 ;; if there's none, use the curren date
                                 (calendar-current-date)))
                 (day (time-to-days (encode-time 1 1 1
                                                 (second calendar-date)
                                                 (first calendar-date)
                                                 (third calendar-date))))
                 (calendar-buffer (current-buffer)))
            (org-agenda-list nil day)
            (select-window (get-buffer-window calendar-buffer))))

        (define-key calendar-mode-map (kbd "RET") 'th-calendar-open-agenda)

        ;; And heres a small minor mode which uses the function above to refresh
        ;; the agenda buffer when you move point in the calendar buffer, so
        ;; calendar and agenda stay in sync.

        (define-minor-mode th-org-agenda-follow-calendar-mode
            "If enabled, each calendar movement will refresh the org agenda
buffer."
          :lighter " OrgAgendaFollow"
          (if (not (eq major-mode 'calendar-mode))
              (message "Cannot activate th-org-agenda-follow-calendar-mode in %s." major-mode)
              (if th-org-agenda-follow-calendar-mode
                  (add-hook 'calendar-move-hook 'th-calendar-open-agenda)
                  (remove-hook 'calendar-move-hook 'th-calendar-open-agenda))))

        (add-hook 'calendar-mode-hook 'th-org-agenda-follow-calendar-mode)

        ;; Another thing I added to calendar is the display of the week-of-year
        ;; in the mode-line.

        (add-to-list 'calendar-mode-line-format
                     '(let ((day (nth 1 date))
                            (month (nth 0 date))
                            (year (nth 2 date)))
                       (format-time-string "Week of year: %V"
                        (encode-time 1 1 1 day month year))))

        ;;Possibly related posts: (automatically generated)
        )))


(defun lotus-cal-org/init-g ()
  (use-package g
      :defer t
      :config
      (progn
        (setq g-user-email "id@gmail.com")
        (setq g-html-handler 'w3m-buffer)

        ;; Write to Diary any entry added to Google Calendar
        (with-eval-after-load "gcal"
          '(progn
            (defun gcal-read-event (title content
                                    where
                                    start end
                                    who
                                    transparency status)
              "Prompt user for event params and return an event structure."
              (interactive
               (list
                (read-from-minibuffer "Title: ")
                (read-from-minibuffer "Content: ")
                (read-from-minibuffer "Where: ")
                (gcal-read-calendar-time "Start Time: ")
                (gcal-read-calendar-time "End Time: ")
                (gcal-read-who "Participant: ")
                (gcal-read-transparency)
                (gcal-read-status)))
              (save-excursion
                (let ((pop-up-frames (window-dedicated-p (selected-window))))
                  (find-file-other-window (substitute-in-file-name diary-file)))
                (when (eq major-mode default-major-mode) (diary-mode))
                (widen)
                (diary-unhide-everything)
                (goto-char (point-max))
                (when (let ((case-fold-search t))
                        (search-backward "Local  Variables:"
                                         (max (- (point-max) 3000) (point-min))
                                         t))
                  (beginning-of-line)
                  (insert "n")
                  (forward-line -1))
                (let* ((dayname)
                       (day (substring start 8 10))
                       (month (substring start 5 7))
                       (year (substring start 0 4))
                       (starttime (substring start 11 16))
                       (endtime (substring end 11 16))
                       (monthname (calendar-month-name (parse-integer month) t))
                       (date-time-string (concat (mapconcat 'eval calendar-date-display-form "")
                                                 " " starttime " - " endtime)))
                  (insert
                   (if (bolp) "" "n")
                   ""
                   date-time-string " " title))
                (bury-buffer)
                (declare (special gcal-auth-handle))
                (let ((event (make-gcal-event
                              :author-email (g-auth-email gcal-auth-handle))))
                  (setf (gcal-event-title  event) title
                        (gcal-event-content event) content
                        (gcal-event-where event) where
                        (gcal-event-when-start event) start
                        (gcal-event-when-end event) end
                        (gcal-event-who event) who
                        (gcal-event-transparency  event) transparency
                        (gcal-event-status event) status)
                  event))))))))



;;; packages.el ends here
