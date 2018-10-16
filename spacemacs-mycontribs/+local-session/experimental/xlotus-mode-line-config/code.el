;;; mode-line-config.el --- session setting

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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


(require 'macros-config)

(setq global-mode-string
      ;; '("" win:mode-string " " display-time-string " " appt-mode-string)
      '(org-mode-line-string
        org-mode-work-day-mode-line-string
        win:mode-string
        display-time-string
        appt-mode-string))


(deh-section "ModeLineDirtrack"
  ;; emacswiki: http://www.emacswiki.org/emacs/ModeLineDirtrack
  (defun add-mode-line-dirtrack ()
    (add-to-list 'mode-line-buffer-identification
                 '(:propertize (" " default-directory " ") face dired-directory)))
  (add-hook 'shell-mode-hook 'add-mode-line-dirtrack))

;; http://stackoverflow.com/questions/778508/emacs-add-hostname-to-mode-line
(let ((pos (cddr (memq 'mode-line-modes mode-line-format))))
  (if pos
      (setcdr pos
              (cons
               '(:eval
                 (if (frame-parameter (selected-frame) 'frame-spec-id)
                     (concat
                      " "
                      (frame-parameter (selected-frame) 'frame-spec-id)
                      (unless (lotus-check-session-saving)
                        " noAutoSave"))))
               (cons
                '(:eval
                  (if (car sidebrain-current-stack)
                      (concat
                       " "
                       (car sidebrain-current-stack))))
                (cdr pos))))))


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

;;(when t
(deh-section "Mode Line Dynamic Add"
  (require 'tree)
  (defvar *mode-line-tree* nil "Mode line tree")


  (defvar global-mode-line-list nil "global-mode-line-list")

  (setq winring-show-names t)

  (defun setup-mode-line ()
    (interactive)
    (let* ((dynamic nil)
           (mode-line-help
            (reduce '(lambda (a b) (concat a "\n" b))
                    '("mouse-1: Select (drag to resize)"
                      "mouse-2: Make current window occupy the whole frame"
                      "mouse-3: Remove current window from display")))
          ;; (separator ,(propertize "--" 'help-echo mode-line-help))
           (gap-white-space (propertize "" 'help-echo mode-line-help))
           (white-space     (propertize " " 'help-echo mode-line-help))
           (separator (if dynamic
                          `(:eval
                            (if (display-graphic-p)
                                (propertize " " 'help-echo ,mode-line-help)
                                (propertize "-" 'help-echo ,mode-line-help)))
                          (if (display-graphic-p)
                              (propertize " " 'help-echo mode-line-help)
                              (propertize "-" 'help-echo mode-line-help)))))

      (setq *mode-line-tree* nil)
      (setf (tree-node* *mode-line-tree* 1 1) "%e")
      (setf (tree-node* *mode-line-tree* 1 2)
            '((winring-show-names ("<" winring-name "> ")) (propertize "-" help-echo mode-line-help)))
      (setf (tree-node* *mode-line-tree* 1 3) 'mode-line-mule-info)
      (setf (tree-node* *mode-line-tree* 1 4) 'mode-line-client)
      (setf (tree-node* *mode-line-tree* 1 5) 'mode-line-modified)
      (setf (tree-node* *mode-line-tree* 1 6) 'mode-line-remote)
      (setf (tree-node* *mode-line-tree* 1 7) 'mode-line-frame-identification)
      (setf (tree-node* *mode-line-tree* 1 8) 'mode-line-buffer-identification)
      (setf (tree-node* *mode-line-tree* 1 9) white-space)
      (setf (tree-node* *mode-line-tree* 1 10) 'mode-line-position)

      (setf (tree-node* *mode-line-tree* 2 1)  `(elscreen-display-screen-number (,gap-white-space elscreen-e21-mode-line-string)))
      (setf (tree-node* *mode-line-tree* 2 2) '(vc-mode vc-mode))
      (setf (tree-node* *mode-line-tree* 2 3) white-space)
      (setf (tree-node* *mode-line-tree* 2 4) 'mode-line-modes)
      ;; (setf (tree-node* *mode-line-tree* 2 5) 'mode-line-misc-info)
      ;; (setf (tree-node* *mode-line-tree* 2 5) `(which-function-mode (,separator which-func-format)))
      (setf (tree-node* *mode-line-tree* 2 5) `(which-function-mode (,gap-white-space which-func-format ,separator)))
      (setf (tree-node* *mode-line-tree* 2 6) `(global-mode-string (,gap-white-space global-mode-string ,separator)))
      (setf (tree-node* *mode-line-tree* 2 7) `(:eval
                                                (apply
                                                 'concat
                                                 (append
                                                  (mapcar
                                                   #'(lambda (e)
                                                       (cond
                                                         ((stringp e)   e)
                                                         ((and (symbolp e) (boundp e) (stringp (symbol-value e)))   (symbol-value e))
                                                         ;; ((and (symbolp e) (fboundp e)) (funcall (symbol-function e)))
                                                         (t   "")))
                                                   global-mode-line-list)))))
      (setf (tree-node* *mode-line-tree* 2 8) separator)

      (setf (tree-node* *mode-line-tree* 2 9) `(:eval
                                                (if (frame-parameter (selected-frame) 'frame-spec-id)
                                                    (concat ,gap-white-space (propertize (frame-parameter (selected-frame) 'frame-spec-id) 'help-echo "Frame Session name")))))

      (setf (tree-node* *mode-line-tree* 2 10) `(:eval
                                                 (unless (lotus-check-session-saving)
                                                   (concat ,separator (propertize "noAutoSave" 'help-echo "Session auto save is not working.")))))

      (setf (tree-node* *mode-line-tree* 2 11) `(:eval (if (car sidebrain-current-stack) (concat ,separator (car sidebrain-current-stack)))))
      (setf (tree-node* *mode-line-tree* 2 12) 'mode-line-end-spaces))

    (setq-default
     mode-line-format
     (reverse
      (mapcar 'cdr
              (apply 'append (mapcar 'cdr
                                     *mode-line-tree*)))))
    (setq
     mode-line-format
     (reverse
      (mapcar 'cdr
              (apply 'append (mapcar 'cdr
                                     *mode-line-tree*))))))


  ;; (setq mode-line-format-original mode-line-format)

  (defvar mode-line-format-original nil)
  (setq mode-line-format-original mode-line-format)


  (message "setting up mode-line")
  (setup-mode-line)

  (add-hook 'lotus-enable-startup-interrupting-feature-hook
            '(lambda ()
              (add-hook 'after-make-frame-functions
               '(lambda (nframe)
                 (setup-mode-line))))))

;; )


                                        ; (setq mode-line-format default-mode-line-format)
;; global-mode-string

;; (setq
;;  global-mode-line-list
;;  '("" win:mode-string " " display-time-string " " org-mode-work-day-mode-line-string appt-mode-string))

;; (setq
;;  global-mode-line-list
;;  '(org-mode-work-day-mode-line-string))


;; (apply
;;  'concat
;;  (mapcar
;;   #'(lambda (e)
;;       (cond
;;         ((stringp e) e)
;;         ((and (symbolp e) (boundp e) (stringp (symbol-value e))) (symbol-value e))
;;         ((and (symbolp e) (fboundp e)) (funcall (symbol-function e)))
;;         (t "")))
;;   global-mode-line-list))







  ;; (reverse (tree-leaves *mode-line-tree* 2))


(provide 'mode-line-config)
;;; mode-line-config.el ends here
