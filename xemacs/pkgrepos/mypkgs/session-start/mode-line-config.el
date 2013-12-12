;;; mode-line-config.el --- session setting

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

;;

;;; Code:


;; mode-line-format

(setq global-mode-string
      '("" win:mode-string " " display-time-string " " appt-mode-string))


(deh-section "ModeLineDirtrack"
  ;; emacswiki: http://www.emacswiki.org/emacs/ModeLineDirtrack
  (defun add-mode-line-dirtrack ()
    (add-to-list 'mode-line-buffer-identification
                 '(:propertize (" " default-directory " ") face dired-directory)))
  (add-hook 'shell-mode-hook 'add-mode-line-dirtrack))

;; http://stackoverflow.com/questions/778508/emacs-add-hostname-to-mode-line
(let ((pos (cddr (memq 'mode-line-modes mode-line-format))))
  (setcdr pos
          (cons
           '(:eval
             (if (frame-parameter (selected-frame) 'frame-spec-id)
                 (concat
                  " "
                  (file-name-nondirectory (frame-parameter (selected-frame) 'frame-spec-id))

                  (unless (sharad/check-session-saving)
                    " noAutoSave"))))
            (cons
             '(:eval
               (if (car sidebrain-current-stack)
                   (concat
                    " "
                    (car sidebrain-current-stack))))
             (cdr pos)))))


(deh-require 'scroll-mode-line-mode
  )

;; (testing
;;  (let* ((x '(a b c d e f))
;;         (pos (memq 'e x)))
;;    (setcdr pos
;;            (cons
;;             'n
;;             (cons
;;              'l
;;              (cdr pos))))
;;    x))


(when nil
(deh-section "Amit's thought"
  ;; http://amitp.blogspot.in/2011/08/emacs-custom-mode-line.html
  ;; Mode line setup
  (setq-default
   mode-line-format
   '(; Position, including warning for 80 columns
     (:propertize "%4l:" face mode-line-position-face)
     (:eval (propertize "%3c" 'face
             (if (>= (current-column) 80)
                 'mode-line-80col-face
                 'mode-line-position-face)))
                                        ; emacsclient [default -- keep?]
     mode-line-client
     "  "
                                        ; read-only or modified status
     (:eval
      (cond (buffer-read-only
             (propertize " RO " 'face 'mode-line-read-only-face))
            ((buffer-modified-p)
             (propertize " ** " 'face 'mode-line-modified-face))
            (t "      ")))
     "    "
                                        ; directory and buffer/file name
     (:propertize (:eval (shorten-directory default-directory 30))
      face mode-line-folder-face)
     (:propertize "%b"
      face mode-line-filename-face)
                                        ; narrow [default -- keep?]
     " %n "
                                        ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
     (vc-mode vc-mode)
     "  %["
     (:propertize mode-name
      face mode-line-mode-face)
     "%] "
     (:eval (propertize (format-mode-line minor-mode-alist)
             'face 'mode-line-minor-mode-face))
     (:propertize mode-line-process
      face mode-line-process-face)
     (global-mode-string global-mode-string)
     "    "
                                        ; nyan-mode uses nyan cat as an alternative to %p
     (:eval (when nyan-mode (list (nyan-create))))
     ))

  ;; Helper function
  (defun shorten-directory (dir max-length)
    "Show up to `max-length' characters of a directory name `dir'."
    (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
          (output ""))
      (when (and path (equal "" (car path)))
        (setq path (cdr path)))
      (while (and path (< (length output) (- max-length 4)))
        (setq output (concat (car path) "/" output))
        (setq path (cdr path)))
      (when path
        (setq output (concat ".../" output)))
      output))

  ;; Extra mode line faces
  (make-face 'mode-line-read-only-face)
  (make-face 'mode-line-modified-face)
  (make-face 'mode-line-folder-face)
  (make-face 'mode-line-filename-face)
  (make-face 'mode-line-position-face)
  (make-face 'mode-line-mode-face)
  (make-face 'mode-line-minor-mode-face)
  (make-face 'mode-line-process-face)
  (make-face 'mode-line-80col-face)

  (set-face-attribute 'mode-line nil
                      :foreground "gray60" :background "gray20"
                      :inverse-video nil
                      :box '(:line-width 6 :color "gray20" :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "gray80" :background "gray40"
                      :inverse-video nil
                      :box '(:line-width 6 :color "gray40" :style nil))

  (set-face-attribute 'mode-line-read-only-face nil
                      :inherit 'mode-line-face
                      :foreground "#4271ae"
                      :box '(:line-width 2 :color "#4271ae"))
  (set-face-attribute 'mode-line-modified-face nil
                      :inherit 'mode-line-face
                      :foreground "#c82829"
                      :background "#ffffff"
                      :box '(:line-width 2 :color "#c82829"))
  (set-face-attribute 'mode-line-folder-face nil
                      :inherit 'mode-line-face
                      :foreground "gray60")
  (set-face-attribute 'mode-line-filename-face nil
                      :inherit 'mode-line-face
                      :foreground "#eab700"
                      :weight 'bold)
  (set-face-attribute 'mode-line-position-face nil
                      :inherit 'mode-line-face
                      :family "Menlo" :height 100)
  (set-face-attribute 'mode-line-mode-face nil
                      :inherit 'mode-line-face
                      :foreground "gray80")
  (set-face-attribute 'mode-line-minor-mode-face nil
                      :inherit 'mode-line-mode-face
                      :foreground "gray40"
                      :height 110)
  (set-face-attribute 'mode-line-process-face nil
                      :inherit 'mode-line-face
                      :foreground "#718c00")
  (set-face-attribute 'mode-line-80col-face nil
                      :inherit 'mode-line-position-face
                      :foreground "black" :background "#eab700")))



(deh-section "Hiding and replacing modeline strings with clean-mode-line"
  ;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
  (defvar mode-line-cleaner-alist
    `((auto-complete-mode . " α")
      (yas/minor-mode . " υ")
      (paredit-mode . " π")
      (eldoc-mode . "")
      (abbrev-mode . "")
      ;; Major modes
      (lisp-interaction-mode . "λ")
      (hi-lock-mode . "")
      (python-mode . "Py")
      (emacs-lisp-mode . "EL")
      (nxhtml-mode . "nx"))
    "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")


  (defun clean-mode-line ()
    (interactive)
    (loop for cleaner in mode-line-cleaner-alist
         do (let* ((mode (car cleaner))
                   (mode-str (cdr cleaner))
                   (old-mode-str (cdr (assq mode minor-mode-alist))))
              (when old-mode-str
                (setcar old-mode-str mode-str))
              ;; major mode
              (when (eq mode major-mode)
                (setq mode-name mode-str)))))


  (add-hook 'after-change-major-mode-hook 'clean-mode-line)

  ;;; alias the new `flymake-report-status-slim' to
  ;;; `flymake-report-status'
  (defalias 'flymake-report-status 'flymake-report-status-slim)
  (defun flymake-report-status-slim (e-w &optional status)
    "Show \"slim\" flymake status in mode line."
    (when e-w
      (setq flymake-mode-line-e-w e-w))
    (when status
      (setq flymake-mode-line-status status))
    (let* ((mode-line " Φ"))
      (when (> (length flymake-mode-line-e-w) 0)
        (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
      (setq mode-line (concat mode-line flymake-mode-line-status))
      (setq flymake-mode-line mode-line)
      (force-mode-line-update))))



(deh-section "test"

  (deh-require-maybe (progn modeline-posn ;; http://www.emacswiki.org/emacs/ModeLinePosition
                            nyan-mode ;; http://www.emacswiki.org/emacs/NyanMode http://nyan-mode.buildsomethingamazing.com/
                            sml-modeline ;; http://www.emacswiki.org/emacs/SmlModeLine
                            diminish ;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/#comment-14637
                            )


                     ;; (:eval (list (nyan-create)))

                     ))





(provide 'mode-line-config)
;;; mode-line-config.el ends here
