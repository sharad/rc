;;; schedule-config.el --- Diary, Calendar etc

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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



(defun fancy-diary-display-week-graph-if-appt ()

  (if (or (not diary-entries-list)
          (and (not (cdr diary-entries-list))
               (string-equal (car (cdr (car diary-entries-list))) "")))

      (let* ((holiday-list (if holidays-in-diary-buffer
                               (check-calendar-holidays original-date)))
             (msg (format "No diary entries for %s %s"
                          (concat date-string (if holiday-list ":" ""))
                          (mapconcat 'identity holiday-list "; "))))
        (if (<= (length msg) (frame-width))
            (message msg)
            (set-buffer (get-buffer-create holiday-buffer))
            (setq buffer-read-only nil)
            (calendar-set-mode-line date-string)
            (erase-buffer)
            (insert (mapconcat 'identity holiday-list "\n"))
            (goto-char (point-min))
            (set-buffer-modified-p nil)
            (setq buffer-read-only t)
            (display-buffer holiday-buffer)
            (message  "No diary entries for %s" date-string) ))

      (fancy-diary-display-week-graph)))

(deh-require-maybe weekly-view
  ;; http://www.emacswiki.org/emacs/CalendarWeeklyView
  ;; (remove-hook 'diary-display-hook 'fancy-diary-display-week-graph))
  (add-hook 'diary-display-function 'fancy-diary-display-week-graph-if-appt)


  (defun toggle-fancy-diary-display-week-graph ()
    (interactive)
    (if (memq 'fancy-diary-display-week-graph-if-appt diary-display-function)
        (remove-hook 'diary-display-hook 'fancy-diary-display-week-graph-if-appt)
        (add-hook 'diary-display-function 'fancy-diary-display-week-graph-if-appt))))

(deh-require-maybe diary-lib
  (setq diary-display-function 'diary-fancy-display)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-list-entries-hook 'diary-mark-included-diary-files)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t))


;; (defadvice diary-fancy-display (before decide activate)
;;       (let ((appt-is-here (car (diary-display-no-entries))))
;; )


(deh-require-maybe (and planner-interface midnight)
  ;; (midnight-delay-set 'midnight-delay 16200) ;; (eq (* 4.5 60 60) "4:30am")
  (midnight-delay-set 'midnight-delay "4:30am")
  (add-hook 'midnight-hook '(lambda ()
                             (with-safe-plan-env ;so it will not call update-ssh-agent in night.
                                 (save-excursion
                                   (save-window-excursion
                                     (message "Midnight: running calendar and planner")
                                     (calendar)
                                     ;; check planner-carry-tasks-forward
                                     (plan 7)))))))


(deh-require-maybe (progn
                     calfw
                     calfw-howm
                     calfw-ical
                     calfw-org
                     calfw-cal)
  ;; https://github.com/kiwanami/emacs-calfw

  (deh-require-maybe calfw-howm
    (eval-after-load "howm-menu" '(progn
                                   (require 'calfw-howm)
                                   (cfw:install-howm-schedules)
                                   (define-key howm-mode-map (kbd "M-C") 'cfw:open-howm-calendar)))

    (define-key howm-mode-map (kbd "M-C") 'cfw:elscreen-open-howm-calendar))

  (deh-require-maybe calfw-ical
    ;; (cfw:open-ical-calendar "http://www.google.com/calendar/ical/.../basic.ics")
    )

  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")  ; orgmode source
      (cfw:howm-create-source "Blue")  ; howm source
      (cfw:cal-create-source "Orange") ; diary source
      (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
                                        ; google calendar ICS
      (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed"))))


  ;; Holidays

  ;; The calfw collects holidays from the customize variable
  ;; calendar-holidays which belongs to holidays.el in the Emacs. See
  ;; the document and source of holidays.el for details.

  ;; Format of month and week days

  ;; Month
  (setq calendar-month-name-array
        ["January" "February" "March"     "April"   "May"      "June"
                   "July"    "August"   "September" "October" "November" "December"])

  ;; Week days
  (setq calendar-day-name-array
        ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

  ;; First day of the week
  (setq calendar-week-start-day 0) ; 0:Sunday, 1:Monday


  ;; Faces
  (custom-set-faces
   '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
   '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
   '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
   '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
   '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
   '(cfw:face-grid ((t :foreground "DarkGrey")))
   '(cfw:face-default-content ((t :foreground "#bfebbf")))
   '(cfw:face-periods ((t :foreground "cyan")))
   '(cfw:face-day-title ((t :background "grey10")))
   '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
   '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
   '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
   '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
   '(cfw:face-today ((t :background: "grey10" :weight bold)))
   '(cfw:face-select ((t :background "#2f2f2f")))
   '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
   '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
   '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold))))


  ;; Grid setting example:
  ;; Default setting
  (setq cfw:fchar-junction ?+
        cfw:fchar-vertical-line ?|
        cfw:fchar-horizontal-line ?-
        cfw:fchar-left-junction ?+
        cfw:fchar-right-junction ?+
        cfw:fchar-top-junction ?+
        cfw:fchar-top-left-corner ?+
        cfw:fchar-top-right-corner ?+ )

  ;; Unicode characters
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)

  ;; Another unicode chars
  (setq cfw:fchar-junction ?╬
        cfw:fchar-vertical-line ?║
        cfw:fchar-horizontal-line ?═
        cfw:fchar-left-junction ?╠
        cfw:fchar-right-junction ?╣
        cfw:fchar-top-junction ?╦
        cfw:fchar-top-left-corner ?╔
        cfw:fchar-top-right-corner ?╗)


  ;;  Line breaking

  ;; If a content string is longer than the cell width, the calfw breaks
  ;; into the multiple lines. In the current implementation, the Calfw
  ;; has 3 strategies: none, simple and wordwrap. The variable
  ;; cfw:render-line-breaker selects the strategy to break lines.

  ;;     cfw:render-line-breaker-none
  ;;         Never breaks lines. Longer contents are truncated.
  ;;     cfw:render-line-breaker-simple (default)
  ;;         This strategy breaks lines with rigid width. This may be not so beautiful, but In the most cases it looks good.
  ;;     cfw:render-line-breaker-wordwrap

  ;;         This strategy breaks lines with the emacs function
  ;;         fill-region. Although, the line breaking algorithm of the
  ;;         Emacs is not so smart as more complicated ones, such as
  ;;         Knuth/Plass algorithm, this strategy is better than the
  ;;         simple one.


  (add-hook 'sharad/enable-startup-inperrupting-feature-hook 'cfw:open-calendar-buffer t))

(provide 'schedule-config)
;;; schedule.el ends here

