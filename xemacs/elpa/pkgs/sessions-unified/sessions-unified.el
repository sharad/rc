;;; sessions-unified.el --- session setting

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

;; function frame-session-restore-hook-func
;; (add-hook 'lotus-enable-startup-interrupting-feature-hook
;;           'frame-session-restore-hook-func
;;           t)

;; (add-hook ;; 'after-init-hook
;;  'lotus-enable-startup-interrupting-feature-hook
;;  '(lambda ()
;;    (run-at-time-or-now 7 'lotus-desktop-session-restore)))

;;; Code:



;; (require 'dot-emacs-helper)

(require 'general-testing)
;; testing
(require 'rcs-backup)

(require 'cl)

(eval-when-compile
  '(require 'cl))

(require 'wrappers)
(require 'basic-utils)
;; run-at-time-or-now
(require 'utils-custom)
;; lotus-read-sexp
(require 'misc-utils)

(require 'desktop)
(require 'session)
(require 'elscreen)
(require 'emacs-panel)

;; ;; BUG TODO
;; (require 'vc-config)

(defvar *session-unified-desktop-enabled* t "Enable desktop restoration.")
(defvar *session-unified-session-enabled* t "Enable session restoration.")


(defvar lotus-disable-desktop-restore-interrupting-feature-hook nil
  "feature that need to be disabled for proper restoring of desktop.")

(defvar lotus-enable-desktop-restore-interrupting-feature-hook nil
  "feature that were disabled for proper restoring of desktop will get re-enabled here.")

(defvar session-unified-save-all-sessions-before-hook nil "Hook run before saving all session")
(defvar session-unified-save-all-sessions-after-hook nil "Hook run after saving all session")

(eval-when-compile
 (defvar sessions-unified-utils-notify nil)
 (unless (null 'sessions-unified-utils-notify)
   (setq sessions-unified-utils-notify
         (lambda (title fmt &rest args)
           (concat title ": "
                   (apply 'message fmt args))))))

(defvar sessions-unified-utils-notify nil)

(unless (null 'sessions-unified-utils-notify)
  (setq sessions-unified-utils-notify
        (lambda (title fmt &rest args)
          (concat title ": "
                  (apply 'message fmt args)))))


(defun sessions-unified-put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
        (progn
          (setcdr elm value)
          alist)
        (cons (cons key value) alist))))

(defun sessions-unified-set-alist (symbol key value)
  "Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE."
  (or (boundp symbol)
      (set symbol nil))
  (set symbol (sessions-unified-put-alist key value (symbol-value symbol))))

;;;###autoload
(defun add-to-enable-desktop-restore-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-to-hook
   'lotus-enable-desktop-restore-interrupting-feature-hook
   fn
   append
   local))
;;;###autoload
(defun remove-from-enable-desktop-restore-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook
   'lotus-enable-desktop-restore-interrupting-feature-hook
   fn
   local))

;;;###autoload
(defun add-to-disable-desktop-restore-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-to-hook
   'lotus-disable-desktop-restore-interrupting-feature-hook
   fn
   append
   local))
;;;###autoload
(defun remove-from-disable-desktop-restore-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook
   'lotus-disable-desktop-restore-interrupting-feature-hook
   fn
   local))


;; (lotus-elscreen-get-screen-to-name-alist)
(with-eval-after-load "elscreen"
  (defun lotus-elscreen-get-screen-to-name-alist ()
    ;; (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
    (elscreen-notify-screen-modification-suppress
     (elscreen-set-window-configuration (elscreen-get-current-screen)
                                        (elscreen-current-window-configuration))
     (let* ((screen-list (sort (elscreen-get-screen-list) '<))
            screen-name screen-to-name-alist nickname-type-map)
       (elscreen-save-screen-excursion
        (mapcar
         #'(lambda (screen)
             ;; If nickname exists, use it.
             (setq screen-name (elscreen-get-screen-nickname screen))
             ;; Nickname does not exist, so examine major-mode and buffer-name.
             (when (null screen-name)
               (elscreen-goto-internal screen)

               (setq nickname-type-map
                     (mapcar
                      (lambda (window)
                        (with-current-buffer (window-buffer window)
                          (or (elscreen-get-alist-to-nickname
                               elscreen-mode-to-nickname-alist-internal
                               'string-match (symbol-name major-mode))
                              (elscreen-get-alist-to-nickname
                               elscreen-buffer-to-nickname-alist-internal
                               'string-match (buffer-name))
                              (cons 'buffer-name (cons (buffer-name) (buffer-file-name))))))
                      (window-list)))

               (let (nickname-list)
                 (while (> (length nickname-type-map) 0)
                   (let ((type (caar nickname-type-map))
                         (buff-file (cdar nickname-type-map)))
                     (when buff-file
                       (setq nickname-list (cons buff-file nickname-list)))
                     (setq nickname-type-map
                           (if (eq type 'nickname)
                               (delete (car nickname-type-map) nickname-type-map)
                               (cdr nickname-type-map)))))
                 ;; (setq screen-name
                 ;;       (mapconcat 'identity (reverse nickname-list) ":"))
                 (setq screen-name (reverse nickname-list))))

             (sessions-unified-set-alist 'screen-to-name-alist screen screen-name))
         screen-list))

       ;; (elscreen-set-screen-to-name-alist-cache screen-to-name-alist)
       (reverse screen-to-name-alist))))

  (defun lotus-elscreen-get-desktop-buffer-args-list ()
    ;; (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
    (elscreen-notify-screen-modification-suppress
     (elscreen-set-window-configuration (elscreen-get-current-screen)
                                        (elscreen-current-window-configuration))
     (let* ((screen-list (sort (elscreen-get-screen-list) '<))
            screen-name)
       (let ((desktop-buffers
              (elscreen-save-screen-excursion
               (remove-duplicates
                (mapcan
                 (lambda (screen)
                   ;; If nickname exists, use it.
                   (setq screen-name (elscreen-get-screen-nickname screen))
                   ;; Nickname does not exist, so examine major-mode and buffer-name.
                   (when (null screen-name)
                     (elscreen-goto-internal screen)
                     (mapcar
                      (lambda (window)
                        (window-buffer window))
                      (window-list))))
                 screen-list)))))

         (remove nil
                 (mapcar 'desktop-make-create-buffer-list desktop-buffers)))))))

(with-eval-after-load "desktop"
  (defun desktop-make-create-buffer (buffer)
    (let ((l (desktop-buffer-info buffer))
          ;; (eager desktop-restore-eager)
          (eager t))
      (let ((base (pop l)))
        (when (apply 'desktop-save-buffer-p l)
          (when (and base (not (string= base "")))
            (setcar (nthcdr 1 l) base))
          `(let ((desktop-buffer-ok-count 0)
                 (desktop-buffer-fail-count 0)
                 desktop-first-buffer)
             (
              ,(if (or (not (integerp eager))
                       (if (zerop eager)
                           nil
                           (setq eager (1- eager))))
                   'desktop-create-buffer
                   'desktop-append-buffer-args)

               (string-to-number desktop-file-version)
               ;; If there's a non-empty base name, we save it instead of the buffer name
               ;; (when (and base (not (string= base "")))
               ;;   (setcar (nthcdr 1 l) base))
               ;; (dolist (e l)
               ;;   (insert "\n  " (desktop-value-to-string e)))
               ,@(mapcar '(lambda (s)
                           (read (desktop-value-to-string s))) l)))))))

  (testing
   ;; (desktop-make-create-buffer-list (current-buffer))
   (let ((desktop-buffer-ok-count 0)
         (desktop-buffer-fail-count 0)
         desktop-first-buffer)
     ;; (apply 'desktop-create-buffer (car (lotus-elscreen-get-desktop-buffer-args-list)))
     (apply 'desktop-create-buffer
            (car (lotus-elscreen-get-desktop-buffer-args-list)))))

  (defun desktop-make-create-buffer-list (buffer)
    (let ((l (desktop-buffer-info buffer))
          ;; (eager desktop-restore-eager)
          (eager t))
      (let ((base (pop l)))
        (when (apply 'desktop-save-buffer-p l)
          (when (and base (not (string= base "")))
            (setcar (nthcdr 1 l) base))
          ;; `(
          ;;   (string-to-number desktop-file-version)
          ;;   ;; If there's a non-empty base name, we save it instead of the buffer name
          ;;   ;; (when (and base (not (string= base "")))
          ;;   ;;   (setcar (nthcdr 1 l) base))
          ;;   ;; (dolist (e l)
          ;;   ;;   (insert "\n  " (desktop-value-to-string e)))
          ;;   ,@(mapcar '(lambda (s)
          ;;               (read (desktop-value-to-string s))) l))
          (cons (string-to-number desktop-file-version)
                l))))))

(with-eval-after-load "elscreen"

  ;; (defvar elscreen-session-restore-create-scratch-buffer nil "elscreen-session-restore-create-scratch-buffer")

  ;; (setq desktop-base-file-name "session.desktop")

  ;;{{ http://stackoverflow.com/a/13711234
  ;; from: http://stackoverflow.com/questions/847962/what-alternate-session-managers-are-available-for-emacs
  ;; (desktop-save (fmsession-read-location))
  ;; (desktop-read (fmsession-read-location))

  ;; (desktop-make-create-buffer-list (current-buffer))

  (require 'utils-config)

  (defvar *elscreen-session-restore-data* nil "elscreen session restore data like current screen buffer among multiple screens.")

  (defun elscreen-session-session-list-get (&optional nframe)
    (with-selected-frame (or nframe (selected-frame))
      (let (session-list)
        (push (cons 'screens (lotus-elscreen-get-screen-to-name-alist)) session-list)
        (push (cons 'current-buffer-file (cons (buffer-name (current-buffer)) (buffer-file-name))) session-list)
        (push (cons 'current-screen (elscreen-get-current-screen)) session-list)
        (push (cons 'desktop-buffers (lotus-elscreen-get-desktop-buffer-args-list)) session-list))))

  (defun elscreen-session-session-list-set (session-list &optional nframe)
    ;; TODO BUG minibuffer should not get windows, which is happening now
    (if session-list                    ;may causing error
        (with-selected-frame (or nframe (selected-frame))
          (let* ((desktop-buffers
                  (cdr (assoc 'desktop-buffers session-list)))
                 (screens
                  (or
                   (cdr (assoc 'screens session-list))
                   `((,(length session-list) "*scratch*"))))
                 (session-current-screen-buffers
                  (cadr (assoc
                         (cdr (assoc 'current-screen session-list))
                         screens)))
                 (session-current-buffer-file
                  (cdr (assoc 'current-buffer-file session-list))))
            ;; (when t
            (testing
              (message "Bstart: session-current-screen-buffers %s" session-current-screen-buffers)
              (message "Astart: screen-to-name-alist %s" session-list)
              (message "Cstart: desktop-buffers %s" desktop-buffers))

            ;; ready file for buffer in session-list, using desktop-restore methods
            (if desktop-buffers
                ;; recreate desktop buffer if not present.
                (let ((bufs (mapcar
                             '(lambda (bl) (nth 2 bl))
                             desktop-buffers)))
                  (funcall sessions-unified-utils-notify "elscreen-session-session-list-set"
                                  "Please wait I am busy to restore %d\nbuffers %s"
                                  (length desktop-buffers) bufs)
                  (let ((desktop-buffer-ok-count 0)
                        (desktop-buffer-fail-count 0)
                        desktop-first-buffer)
                    (dolist (desktop-buffer-args desktop-buffers)
                      (let ((bufname (nth 2 desktop-buffer-args))
                            (file-path (nth 1 desktop-buffer-args)))
                        (message "restoring %s" bufname)
                        (if (find-buffer-visiting file-path)
                            (message "buffer %s already here" bufname)
                            (if (stringp bufname)
                                (if (get-buffer bufname)
                                    (message "buffer %s already here" bufname)
                                    (let ()
                                      (message "Hello 1")
                                      (message "elscreen-session-session-list-set: Desktop lazily opening %s" bufname)
                                      (unless (ignore-errors
                                                (save-window-excursion
                                                  (apply 'desktop-create-buffer desktop-buffer-args)))
                                        (message "elscreen-session-session-list-set: Desktop lazily opening Failed."))
                                      (message "Hello 2")
                                      (message "restored %s" bufname)))
                                (message "bufname: %s is not string" bufname))))))
                  (funcall sessions-unified-utils-notify "elscreen-session-session-list-set"
                                  "Restored %d\nbuffers %s"
                                  (length desktop-buffers) bufs))
                (message "No desktop-buffers"))

            ;; setup elscreens with buffers
            (while screens
              (message "while screen: %s" screens)
              ;; (setq screen (caar screens))
              ;; (setq buff-files (cdar screens))
              (let* ((screen         (caar screens))
                     (buff-files     (cdar screens))
                     (not-first-buff nil))

                (while buff-files

                  (unless (eq screen 0)
                    (elscreen-create))

                  (let* ((buff-file  (car buff-files))
                         (file-path  (if (consp buff-file)
                                         (cdr buff-file)))
                         (buff (ignore-errors
                                 (get-buffer
                                  (or (if file-path
                                          (find-buffer-visiting file-path))
                                      (if (consp buff-file)
                                          (car buff-file)
                                        buff-file)))))
                         (minibuff-name " *Minibuf"))
                    (message "  while buff: %s file-path: %s" buff file-path)
                    (when (and
                           buff
                           (bufferp buff)
                           (not
                            (string-equal
                             (substring
                              (buffer-name buff)
                              0
                              (length minibuff-name))
                             minibuff-name))) ;check once for if buff is here or not.
                      ;; newly added here to avoid " *Minibuffer*"
                      (if not-first-buff
                          (switch-to-buffer-other-window buff)
                        (switch-to-buffer buff)
                        (setq not-first-buff t)))
                    (message "test4"))

                  (setq buff-files (cdr buff-files)))

                  (message "progn buff-files: %s" buff-files)
                  (testing (message "else")))

              (setq screens (cdr screens))
              (message "while screen: %s" screens)
              (message "test5")) ;; (while screens

            ;; (when elscreen-session-restore-create-scratch-buffer
            ;;   (elscreen-find-and-goto-by-buffer (get-buffer-create "*scratch*") t t))

            (when nil (elscreen-create))                 ;trap

            ;; set current screen, window, and buffer.
            (let* ((file-path  (if (consp session-current-buffer-file)
                                   (cdr session-current-buffer-file)))
                   (buff
                    (ignore-errors
                      (get-buffer
                       (or (if file-path
                               (find-buffer-visiting file-path))
                           (if (consp session-current-buffer-file)
                               (car session-current-buffer-file)
                             session-current-buffer-file))))))
              (when (and
                     buff
                     (bufferp buff))
                (elscreen-find-and-goto-by-buffer buff nil nil)
                (setq *elscreen-session-restore-data* session-current-buffer-file))
              ;; (if (get-buffer buff)
              ;;     (progn
              ;;       (setq *elscreen-session-restore-data* (list (cons 'cb session-current-screen-buffers)))
              ;;       (testing
              ;;        (message "*elscreen-session-restore-data* %s" *elscreen-session-restore-data*)))
              ;;     (testing
              ;;      (message "in when session-current-screen-buffers %s" session-current-screen-buffers)))
              )
            ) ;; (let* ((desktop-buffers
          (testing
           (message "elscreen-notify-screen-modification"))
          (elscreen-notify-screen-modification 'force-immediately)
          (message "elscreen-session-session-list-set: DONE.")
          )
          (message "elscreen-session-session-list-set: Session do not exists.")))

  (defvar *frames-elscreen-session* nil "Stores all elscreen sessions here.")
  (defvar *frames-elscreen-session-old* nil "Stores all discarded elscreen sessions here.")

  (eval-after-load "desktop"
    '(progn
      (add-to-list
       'desktop-globals-to-save
       '*frames-elscreen-session*)
      (add-to-list
       'desktop-globals-to-save
       '*frames-elscreen-session-old*)))

  (eval-after-load "session"
    '(progn
      (add-to-list
       'session-globals-include
       '(*frames-elscreen-session* 100))
      (add-to-list
       'session-globals-include
       '(*frames-elscreen-session-old* 100))))

  (defun fmsession-migration ()
    (interactive)
    (dolist (session (directory-files "~/.emacs.d/session/frames/" nil "[a-zA-Z]+"))
      (pushnew
       (cons session
             (lotus-read-sexp
              (concat "~/.emacs.d/session/frames/" session "/elscreen")))
       *frames-elscreen-session*)))


  (defun fmsession-delete-session (session)
    (interactive
     (list
      (fmsession-read-location)))
    ;; save somewhere as backup
    (if (and
         session
         (y-or-n-p (format "Can I delete screen \"%s\" session: " session)))
        (progn
          (push
           (find session *frames-elscreen-session* :key 'car :test 'string-equal)
           *frames-elscreen-session-old*)
          (setq *frames-elscreen-session*
                (remove* session *frames-elscreen-session*
                         :key 'car
                         :test 'string-equal)))
        (message "Not deleting screen \"%s\" session: " session)))


  (defun fmsession-modify-element (fun)
    (mapcar fun
            (copy-tree *frames-elscreen-session*)))

  (defun fmsession-modify-name (fun)
    (mapcar (lambda (x)
              (setcar x (funcall fun (car x)))
              x)
            (copy-tree *frames-elscreen-session*)))

  (defun fmsession-store-to-file (file)
    (interactive "Ffile: ")
    (with-temp-file file
      (insert
       (prin1-to-string *frames-elscreen-session*))))

  (defun fmsession-restore-from-file (file)
    (interactive "ffile: ")
    (setq *frames-elscreen-session*
          (append
           *frames-elscreen-session*
           (lotus-read-sexp file))))

  (defun elscreen-session-store (elscreen-session &optional nframe)
    (interactive
     (list
      (fmsession-read-location)))
    (let ((session-list (elscreen-session-session-list-get (or nframe (selected-frame)))))
      (if (assoc elscreen-session *frames-elscreen-session*)
          (setcdr (assoc elscreen-session *frames-elscreen-session*) session-list)
          (push (cons elscreen-session session-list) *frames-elscreen-session*))))

  (defun elscreen-session-restore (elscreen-session &optional nframe)
    (interactive
     (list
      (fmsession-read-location)))
    (message "elscreen-session-restore: start")
    (let ((elscreen-session-list
           (cdr (assoc elscreen-session *frames-elscreen-session*))))
      (testing
       (message "Nstart: session-session %s" elscreen-session))
      (if elscreen-session-list
          (elscreen-session-session-list-set elscreen-session-list (or nframe (selected-frame))))))

  (defun fmsession-read-location (&optional initial-input)
    (let ((used t)
          sel)
      (while used
        (setq used
              (member
               (setq sel (fmsession-read-location-internal initial-input))
               (remove-if #'null
                          (mapcar (lambda (f) (frame-parameter f 'frame-spec-id)) (frame-list))))))
      sel))

  (defun fmsession-read-location-internal (&optional initial-input)
    (condition-case terr
        (ido-completing-read "Session: "
                             (remove-if-not
                              #'(lambda (dir)
                                  (not
                                   (member
                                    dir
                                    (remove-if #'null
                                               (mapcar (lambda (f) (frame-parameter f 'frame-spec-id)) (frame-list))))))
                              (mapcar 'car *frames-elscreen-session*))
                             nil
                             nil
                             initial-input)
      ('quit nil)))

  (defun fmsession-store (session-name &optional nframe)
    "Store the elscreen tab configuration."
    (interactive
     (list (fmsession-read-location)))
    (elscreen-session-store session-name nframe))

  (defun fmsession-restore (session-name &optional nframe)
    "Restore the elscreen tab configuration."
    (interactive
     (list (fmsession-read-location)))
    (message "fmsession-restore: start")
    (if (and
         (fboundp 'elscreen-get-conf-list)
         (elscreen-get-conf-list 'screen-history))
        (elscreen-session-restore session-name nframe)
        (funcall sessions-unified-utils-notify
                 "fmsession-restore"
                 "not restoring screen session as screen-history config not found.")))

  ;; (elscreen-restore)
  ;;}}

  ;;{{

  (defvar *frame-session-restore* nil "*frame-session-restore* if it is true than only frame session will get restored.")
  (defun server-create-frame-before-adrun ()
    "remove-scratch-buffer"
    (setq *frame-session-restore* t))

  (defun server-create-frame-after-adrun ()
      "remove-scratch-buffer"
      (if *elscreen-session-restore-data*
          (let* ((buffer-file (get-buffer (cdr (assoc 'cb *elscreen-session-restore-data*))))
                 (file-path  (if (consp buffer-file)
                                 (cdr buffer-file)))
                 (buff (or (if file-path
                               (find-buffer-visiting file-path))
                           (if (consp buffer-file)
                               (car buffer-file)
                               buffer-file))))
            (testing
             (message "running server-create-window-system-frame afer advise if")
             (message "*elscreen-session-restore-data* %s" *elscreen-session-restore-data*))
            (when buff
              (elscreen-kill)
              (elscreen-find-and-goto-by-buffer buff nil nil)
              (setq *elscreen-session-restore-data* nil)
              (elscreen-notify-screen-modification 'force-immediately)))
          (testing (message "running server-create-window-system-frame afer advise else"))))

  (defadvice server-create-window-system-frame
      (around remove-scratch-buffer activate)
    "remove-scratch-buffer"
    (let ((*frame-session-restore* t))
      ad-do-it
      (server-create-frame-after-adrun)))

  (defadvice server-create-tty-frame
      (around remove-scratch-buffer activate)
    "remove-scratch-buffer"
    (let ((*frame-session-restore* t))
      ad-do-it
      (server-create-frame-after-adrun)))

  (when nil
    ;; (ad-disable-advice 'server-create-window-system-frame 'before 'set-restore-frame-session)
    ;; (ad-disable-advice 'server-create-window-system-frame 'after 'remove-scratch-buffer)
    ;; (ad-enable-advice 'server-create-window-system-frame 'before 'set-restore-frame-session)
    ;; (ad-enable-advice 'server-create-window-system-frame 'after 'remove-scratch-buffer)
    ;; (ad-remove-advice 'server-create-window-system-frame 'before 'set-restore-frame-session)
    ;; (ad-remove-advice 'server-create-window-system-frame 'after 'remove-scratch-buffer)
    ;; (ad-update 'server-create-window-system-frame)
    ;; (ad-activate 'server-create-window-system-frame)

    ;; (ad-disable-advice 'server-create-tty-frame 'before 'set-restore-frame-session)
    ;; (ad-disable-advice 'server-create-tty-frame 'after 'remove-scratch-buffer)
    ;; (ad-enable-advice 'server-create-tty-frame 'before 'set-restore-frame-session)
    ;; (ad-enable-advice 'server-create-tty-frame 'after 'remove-scratch-buffer)
    ;; (ad-remove-advice 'server-create-tty-frame 'before 'set-restore-frame-session)
    ;; (ad-remove-advice 'server-create-tty-frame 'after 'remove-scratch-buffer)
    ;; (ad-update 'server-create-tty-frame)
    ;; (ad-activate 'server-create-tty-frame)
    )
  ;;}}
  )

  ;;{{
(progn ;; "per frame session"

  ;; (require 'emacs-panel)

  (defvar *desktop-vc-read-inprogress* nil "desktop-vc-read-inpgrogress")

  (defun frame-session-set-this-location (nframe &optional not-ask)
    (interactive
     (list (selected-frame)))
    (if nframe (select-frame nframe) (error "nframe is nil"))
    (message "in frame-session-set-this-location")
    (let* ((xwin-enabled (custom-display-graphic-p))
           (wm-hints
            (if xwin-enabled
                (ignore-errors (emacs-panel-wm-hints))))
           (desktop-name (if wm-hints
                             (nth
                              (cadr (assoc 'current-desktop wm-hints))
                              (cdr (assoc 'desktop-names wm-hints)))))
           (location (if (and not-ask
                              desktop-name
                              (member desktop-name (mapcar 'car *frames-elscreen-session*)))
                         desktop-name
                         (fmsession-read-location desktop-name))))
      (if xwin-enabled
          (unless wm-hints
            (message "Some error in wm-hints")))
      (message "frame-session-set-this-location: %s" location)
      (set-frame-parameter nframe 'frame-spec-id location)
      location))

  (defvar *frame-session-restore-screen-display-function* #'display-about-screen
    "function to display screen with frame-session-restore, e.g. display-about-screen, spacemacs-buffer/goto-buffer")

  (defun frame-session-restore (nframe &optional not-ask)
    (message "in frame-session-restore")
    (if (and
         *frame-session-restore*
         (null *desktop-vc-read-inprogress*))
        (progn
          (message "pass in frame-session-restore")
          (if nframe (select-frame nframe) (error "nframe is nil"))
          (fmsession-restore (frame-session-set-this-location nframe not-ask))
          ;; nframe)

          (when (and
                 *frame-session-restore-screen-display-function*
                 (functionp '*frame-session-restore-screen-display-function*))
            (funcall *frame-session-restore-screen-display-function*))
          nframe)
        (progn
          (funcall sessions-unified-utils-notify
           "frame-session-restore"
           "not restoring screen session.")
          (if *desktop-vc-read-inprogress*
              (funcall sessions-unified-utils-notify
               "frame-session-restore"
               "as desktop restore is in progress *desktop-vc-read-inprogress* %s"
               *desktop-vc-read-inprogress*))
          (if (null *frame-session-restore*)
              (funcall sessions-unified-utils-notify
               "frame-session-restore"
               "as another frame session restore in progress *frame-session-restore* %s"
               *frame-session-restore*)))))

  (defun frame-session-apply (nframe)
    "Apply existing frame session to NFRAME."
    (interactive
     (list (selected-frame)))
    (progn
      (select-frame nframe)
      (fmsession-restore (fmsession-read-location) nframe)))

  (defun frame-session-save (nframe)
    (message "in frame-session-save:")
    (let ((location (frame-parameter nframe 'frame-spec-id)))
      (when location
        (message "saved the session for %s" location)
        (fmsession-store location nframe))))

  (defun save-all-frames-session ()
    (dolist (f (frame-list))
      (frame-session-save f)))

  ;; ;; (add-hook '*lotus-after-init-hook*
  ;; (add-hook 'lotus-enable-startup-interrupting-feature-hook ;new
  ;;           '(lambda ()
  ;;             ;; (add-hook 'after-make-frame-functions 'frame-session-set-this-location t)
  ;;             (add-hook
  ;;              'after-make-frame-functions
  ;;              '(lambda (nframe)
  ;;                (run-at-time-or-now-arg 3
  ;;                 (lambda (frm)
  ;;                   (let ((*frame-session-restore* t))
  ;;                       (frame-session-restore frm t)))
  ;;                 nframe))
  ;;               t)
  ;;             (add-hook 'delete-frame-functions 'frame-session-save)
  ;;             ;; (add-hook 'kill-emacs-hook 'save-all-frames-session)) ; done in save-all-sessions-auto-save
  ;;             ;; t
  ;;             )
  ;;           t)

  (defun frame-session-restore-hook-func ()
    "Add to hook"
    ;; (add-hook 'after-make-frame-functions 'frame-session-set-this-location t)
    (add-hook
     'after-make-frame-functions
     '(lambda (nframe)
       (frame-session-restore nframe t))
     t)
    (add-hook 'delete-frame-functions 'frame-session-save))

  (testing
   (frame-parameter (selected-frame) 'frame-spec-id)
   after-make-frame-functions
   delete-frame-functions
   *lotus-after-init-hook*
   ))
  ;;}}

(with-eval-after-load "desktop"
  ;; (testing
  ;; http://stackoverflow.com/questions/2703743/restore-emacs-session-desktop
  ;; (desktop-save-mode 1)
  ;; (desktop-read)

  ;; from: http://www.emacswiki.org/emacs/DeskTop
  ;; You can add any extra variables you want saved across sessions to the list ‘desktop-globals-to-save’. For example:
  ;; (setq history-length 250)
  ;; (add-to-list 'desktop-globals-to-save 'file-name-history)

  ;; Specifying Files Not to be Opened

  ;; You can specify buffers which should not be saved, by name or by mode, e.g.:


  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$")

        ;; check it http://stackoverflow.com/a/4055504/341107
        desktop-files-not-to-save       ;very important
        ;; default value
        ;; "\\(^/[^/:]*:\\|(ftp)$\\)"
        ;; that do not allow to save trampe file
        "^$")

  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


  ;;{{
  ;; Automatically Overriding Stale Locks

  ;; If you are using desktop-mode and your emacs crashes (or more
  ;; likely, your system crashes), then your desktop file will not be
  ;; released, and emacs will bother you about using it next time you
  ;; start it up. To avoid this, I wrote the following code to
  ;; override the lock on a desktop file if the indicated process is
  ;; not still running.

  ;; desktop-override-stale-locks.el begins here
  ;;   (defun emacs-process-p (pid)
  ;;     "If pid is the process ID of an emacs process, return t, else nil.
  ;; Also returns nil if pid is nil."
  ;;     (when pid
  ;;       (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
  ;;         (when (file-exists-p cmdline-file)
  ;;           (with-temp-buffer
  ;;             (insert-file-contents-literally cmdline-file)
  ;;             (goto-char (point-min))
  ;;             (search-forward "emacs" nil t)
  ;;             pid)))))
  (defadvice desktop-owner (after pry-from-cold-dead-hands activate)
    "Don't allow dead emacsen to own the desktop file."
    (when (not (emacs-process-p ad-return-value))
      (setq ad-return-value nil)))
  ;; desktop-override-stale-locks.el ends here

  ;; If anyone has a more robust implementation of `emacs-process-p,’ feel free to provide it.

  ;; I implemented ‘emacs-process-p’ by following way, it could work on both Windows and Linux.

  (defun emacs-process-p (pid)
    "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
    (when pid
      (let ((attributes (process-attributes pid)) (cmd))
        (dolist (attr attributes)
          (if (string= "comm" (car attr))
              (setq cmd (cdr attr))))
        (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

  ;; I think the original function contains an error. Should it not end something like:

  ;; (when (search-forward "emacs" nil t)
  ;;   pid))))))
  ;;}}

  ;;{{
  ;; Minimal Setup

  ;; This is for people who only want minimal session management
  ;; functionality, and don’t want their previous sessions automatically
  ;; restored at start-up. Note that you need desktop-save-mode NOT
  ;; ENABLED for this to work as intended.

  ;; It works for me with one desktop, with more than one may need some tweaking.

  ;; use only one desktop
  ;; (make-directory (expand-file-name "autoconfig/desktop/" user-emacs-directory) t)
  ;; (setq todoo-file-name (expand-file-name "autoconfig/desktop/" user-emacs-directory))

  (setq desktop-path (auto-config-file "desktop/"))

  (setq desktop-dirname (auto-config-file "desktop/"))

  (setq desktop-base-lock-name
        (concat
         ".emacs.desktop"
         (if (boundp 'server-name)
             (concat "-" server-name))
         ".lock"))

  ;; (debug)

  (setq desktop-base-file-name
        (concat
         "emacs-desktop"
         (if (boundp 'server-name)
             (concat "-" server-name))))

  ;; Since all lists will be truncated when saved, it is important to
  ;; have a high default history length, for example. If that is not
  ;; enough, follow the suggestions in the doc-string of
  ;; ‘desktop-globals-to-save’:

  ;;   An element may be variable name (a symbol) or a cons cell of
  ;;   the form (VAR . MAX-SIZE), which means to truncate VAR’s value
  ;;   to at most MAX-SIZE elements (if the value is a list) before
  ;;   saving the value.


  ;; Auto-Saving the Desktop

  ;; I’m starting to work on a new package called desktop-recover.el
  ;; with some improvements like this. Alternatively, you can just add
  ;; something like this to your init file to auto-save your desktop
  ;; when Emacs is idle: – Doom


  ;; (defun my-desktop-save ()
  ;;   (interactive)
  ;;   ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  ;;   (let ((owner (or (desktop-owner) -1)))
  ;;     (if (eq owner (emacs-pid))
  ;;       (desktop-save desktop-dirname)
  ;;       ;; (desktop-save-in-desktop-dir)
  ;;       (error "You %d are not the desktop owner %d."
  ;;              (emacs-pid) owner))))

  (defvar *desktop-save-filename* (expand-file-name desktop-base-file-name desktop-dirname))
  ;; (setq *desktop-save-filename* (expand-file-name desktop-base-file-name desktop-dirname))

  ;; (when (or (not *emacs-in-init*) (not reloading-libraries))
  (when (or *emacs-in-init* reloading-libraries)
    ;setting to nil so it will be asked from user.
    (setq *desktop-save-filename* nil))

  ;; might be the reason for Terminal 0 is locked.
  ;; after start check M-: (debug)
  ;; in case of timeout and no timeout
  (defun find-desktop-file (prompt desktop-dir default-file)
    (let ((default-local-file (concat default-file "-local")))
      (if (file-directory-p desktop-dir)
          (let ((default-file-path (expand-file-name default-local-file desktop-dir)))

            (unless (file-exists-p default-local-file)
              (ignore-errors
                (message
                 "desktop file %s do not exists, trying to check it out."
                 file default-local-file)
                (vc-checkout-file default-local-file)))

            (expand-file-name
             (with-timeout (20
                            (progn
                              (when (active-minibuffer-window)
                                (abort-recursive-edit))
                              default-local-file))
               (read-file-name prompt     ;promt
                               desktop-dir ;dir
                               default-local-file ;default file name
                               'confirm           ;mustmatch
                               default-local-file ;initial
                               (lambda (f)        ;predicate  BUG failing this cause bugs
                                 (message "f: %s" f)
                                 (string-match
                                  (concat "^"
                                          (file-truename (expand-file-name default-file desktop-dir)) "-")
                                  ;; (concat "^" desktop-dir "/" default-file "-")
                                  (file-truename f)))))
             desktop-dir))
        (error "desktop directory %s don't exists." desktop-dir))))

  ;; (find-desktop-file "select desktop: " "~/tmp/" desktop-base-file-name)

  (defun desktop-get-desktop-save-filename ()
    (interactive)
    (if *desktop-save-filename*
        *desktop-save-filename*
      (setq *desktop-save-filename*
            (find-desktop-file "select desktop: " desktop-dirname desktop-base-file-name))))

  (defun switch-desktop-file ()
    ;; save desktop
    ;; kill all file buffer
    ;; change name of desktop file
    ;; restore desktop file
    )


  (defun desktop-vc-remove (&optional desktop-save-filename)
    "Delete desktop file"
    (interactive "Fdesktop file: ")
    (let* ((desktop-save-filename (or desktop-save-filename *desktop-save-filename*))
           (desktop-base-file-name (file-name-nondirectory desktop-save-filename)))
      (when (file-exists-p desktop-save-filename)
        (put-file-in-rcs desktop-save-filename)
        (delete-file desktop-save-filename))))

  (defun desktop-vc-owner (&optional desktop-save-filename)
    (interactive "fdesktop file: ")
    (let* ((desktop-save-filename (or desktop-save-filename *desktop-save-filename*))
           (desktop-base-file-name (file-name-nondirectory desktop-save-filename))
           (retval (desktop-owner (dirname-of-file desktop-save-filename))))
      (when (emacs-process-p retval)
        retval)))

  (defun desktop-vc-save (&optional desktop-save-filename)
    (interactive "Fdesktop file: ")
    (let* ((desktop-save-filename (or desktop-save-filename *desktop-save-filename*))
           (desktop-base-file-name (file-name-nondirectory desktop-save-filename)))
      (desktop-save (dirname-of-file desktop-save-filename))
      (if (file-exists-p desktop-save-filename)
          (put-file-in-rcs desktop-save-filename))
      (setq desktop-file-modtime (nth 5 (file-attributes desktop-save-filename
                                                         ;; (desktop-full-file-name)
                                                         )))))
  ;; NOTE:
  ;; (setq desktop-restore-eager 2)
  (setq desktop-restore-eager 0) ;; for avoiding error from read only buffer when applying pabber-expand-mode

  (defun desktop-vc-read (&optional desktop-save-filename)
    (interactive "fdesktop file: ")
    (funcall sessions-unified-utils-notify "desktop-vc-read" "desktop-restore-eager value is %s" desktop-restore-eager)
    (let* ((desktop-save-filename (or desktop-save-filename *desktop-save-filename*))
           (desktop-base-file-name (file-name-nondirectory desktop-save-filename)))
      (prog1
          (setq *desktop-vc-read-inprogress* t)
        (run-each-hooks 'lotus-disable-desktop-restore-interrupting-feature-hook)
        (if


            ;; // sharad
            ;; (unless (ignore-errors
            ;;           (save-window-excursion
            ;;             (apply 'desktop-create-buffer desktop-buffer-args)))
            ;;   (message "Desktop lazily opening Failed."))

            ;; ?? how to ignore error generated here

            (desktop-read (dirname-of-file desktop-save-filename))

            (setq *desktop-vc-read-inprogress* nil)
          (message "desktop read failed."))
        (funcall sessions-unified-utils-notify "desktop-vc-read" "finished."))))

  ;; remove desktop after it's been read
  (add-hook 'desktop-after-read-hook
            '(lambda ()
               ;; desktop-remove clears desktop-dirname
               (setq desktop-dirname-tmp desktop-dirname)
               (desktop-vc-remove)
               (setq desktop-dirname desktop-dirname-tmp)))

  (defvar *my-desktop-save-max-error-count* 6 "")
  (defvar *my-desktop-save-error-count* 0 "")

  (defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (let ((owner (or (desktop-vc-owner) -1)))
      (when t ;;condition-case e
        (if (or
             (eq owner (emacs-pid))
             ;; TODO: it was mean to be used as non-obtrusive and non-interctive
             (y-or-n-p (format
                        "Your pid %d are not same as the desktop owner pid %d\nOverwrite existing desktop (might be it was not restore properly at startup)? "
                        (emacs-pid) owner)))
            (if *desktop-save-filename*
                (desktop-vc-save *desktop-save-filename*)
              (error "my-desktop-save: *desktop-save-filename* is nil, run M-x desktop-get-desktop-save-filename"))
          ;; (desktop-save-in-desktop-dir)
          (progn
            (lotus-disable-session-saving)
            ;; (remove-hook 'auto-save-hook #'save-all-sessions-auto-save)
            (error "You %d are not the desktop owner %d. removed save-all-sessions-auto-save from auto-save-hook and kill-emacs-hook by calling M-x lotus-disable-session-saving"
                   (emacs-pid) owner))))))

  (defcustom save-all-sessions-auto-save-idle-time-interval 7
    "save all sessions auto save idle time interval"
    :group 'session)
  (defvar save-all-sessions-auto-save-idle-time-interval-dynamic 7 "save all sessions auto save idle time interval dynamic.")
  (defcustom save-all-sessions-auto-save-time-interval (* 20 60)
    "save all sessions auto save time interval"
    :group 'session)
  (defvar save-all-sessions-auto-save-time (current-time) "save all sessions auto save time")
  (defvar session-debug-on-error nil "session-debug-on-error")

  (defun save-all-sessions-auto-save (&optional force)
    "Save elscreen frame, desktop, and session time to time
to restore in case of sudden emacs crash."
    (interactive "P")
    (let ((idle-time (or (current-idle-time) '(0 0 0)))
          (time-format "%a %H:%M:%S")
          ;; (time-since-save-all-sessions-auto-save-time (float-time (time-since save-all-sessions-auto-save-time)))
          )
      (let ((time-since-last-save (float-time (time-since save-all-sessions-auto-save-time))))
        (if (or force
                (> time-since-last-save (float-time idle-time)))
            (if (or force
                    (> time-since-last-save save-all-sessions-auto-save-time-interval))
                (if (or
                     force
                     (and idle-time
                          ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Control.html#Auto-Save-Control
                          (> (float-time idle-time) save-all-sessions-auto-save-idle-time-interval-dynamic)))
                    (progn
                      (run-hooks 'session-unified-save-all-sessions-before-hook)
                      (funcall sessions-unified-utils-notify "save-all-sessions-auto-save" "Started to save frame desktop and session.\ncurrent time %s, idle time %d idle-time-interval left %d"
                               (format-time-string time-format save-all-sessions-auto-save-time)
                               (float-time idle-time)
                               save-all-sessions-auto-save-idle-time-interval-dynamic)
                      ;; (message "current time %s, idle time %d idle-time-interval left %d"
                      ;;          (format-time-string time-format save-all-sessions-auto-save-time)
                      ;;          (float-time idle-time)
                      ;;          save-all-sessions-auto-save-idle-time-interval-dynamic)
                      (setq save-all-sessions-auto-save-time (current-time)
                            save-all-sessions-auto-save-idle-time-interval-dynamic save-all-sessions-auto-save-idle-time-interval)
                      (prog1
                          (if session-debug-on-error
                              (progn
                                (save-all-frames-session)
                                (session-vc-save-session)
                                (when *session-unified-desktop-enabled* (my-desktop-save))
                                (funcall sessions-unified-utils-notify "save-all-sessions-auto-save" "Saved frame desktop and session.")
                                (message nil))
                            (condition-case e
                                (progn
                                  (save-all-frames-session)
                                  (session-vc-save-session)
                                  (when *session-unified-desktop-enabled* (my-desktop-save))
                                  (funcall sessions-unified-utils-notify "save-all-sessions-auto-save" "Saved frame desktop and session.")
                                  (message nil))
                              ('error
                               (progn
                                 ;; make after 2 errors.
                                 (funcall sessions-unified-utils-notify "save-all-sessions-auto-save" "Error: %s" e)
                                 (1+ *my-desktop-save-error-count* )
                                 (unless(< *my-desktop-save-error-count* *my-desktop-save-max-error-count*)
                                   (setq *my-desktop-save-error-count* 0)
                                   (funcall sessions-unified-utils-notify "save-all-sessions-auto-save" "Error %s" e)
                                   (lotus-disable-session-saving))))))
                        (run-hooks 'session-unified-save-all-sessions-after-hook)))
                  (setq save-all-sessions-auto-save-idle-time-interval-dynamic
                        (1- save-all-sessions-auto-save-idle-time-interval-dynamic))))

          (setq save-all-sessions-auto-save-time (current-time)
                save-all-sessions-auto-save-idle-time-interval-dynamic save-all-sessions-auto-save-idle-time-interval)))))

  (defun save-all-sessions-auto-save-immediately () (save-all-sessions-auto-save t))

  (defun lotus-desktop-saved-session ()
    "check file exists."
    (file-exists-p *desktop-save-filename*))

  ;; use session-save to save the desktop manually
  ;;;###autoload
  (defun lotus-desktop-session-save ()
    "Save an emacs session."
    (interactive)
    (if *session-unified-desktop-enabled*
        (progn
          (if (lotus-desktop-saved-session)
              (if (y-or-n-p "Overwrite existing desktop (might be it was not restore properly at startup)? ")
                  (desktop-vc-save *desktop-save-filename*)
                (message "Session not saved."))
            (desktop-vc-save *desktop-save-filename*)))
      (message
       "*session-unified-desktop-enabled*: %s"
       *session-unified-desktop-enabled*)))

  (defun lotus-disable-session-saving-immediately ()
    (interactive)
    (remove-hook 'auto-save-hook #'save-all-sessions-auto-save)
    (remove-hook 'kill-emacs-hook #'save-all-sessions-auto-save-immediately)
    (funcall sessions-unified-utils-notify "lotus-disable-session-saving"  "Removed save-all-sessions-auto-save from auto-save-hook and kill-emacs-hook"))


  (defun lotus-enable-session-saving-immediately ()
    (interactive)
    (add-hook 'auto-save-hook #'save-all-sessions-auto-save)
    (add-hook 'kill-emacs-hook #'save-all-sessions-auto-save-immediately)
    (funcall sessions-unified-utils-notify "lotus-enable-session-saving" "Added save-all-sessions-auto-save to auto-save-hook and kill-emacs-hook"))

  (defun lotus-enable-session-saving ()
    ;; (if (or
    ;;      (eq desktop-restore-eager t)
    ;;      (null (lotus-desktop-saved-session)))
    (if (eq desktop-restore-eager t)
        (lotus-enable-session-saving-immediately)
      (progn
        (ad-enable-advice 'desktop-idle-create-buffers 'after 'desktop-idle-complete-actions)
        (ad-update 'desktop-idle-create-buffers)
        (ad-activate 'desktop-idle-create-buffers)))
    (if (lotus-desktop-saved-session)
        (message "desktop file exists.")
      (message "desktop file do not exists.")))

  (defun lotus-disable-session-saving ()
    (lotus-disable-session-saving-immediately)
    (progn
      (ad-disable-advice 'desktop-idle-create-buffers 'after 'desktop-idle-complete-actions)
      (ad-update 'desktop-idle-create-buffers)
      (ad-activate 'desktop-idle-create-buffers)))

  (defun lotus-check-session-saving ()
    (interactive)
    (if (called-interactively-p 'interactive)
        (message
         "%s, %s"
         (if (member #'save-all-sessions-auto-save auto-save-hook)
             "Yes save-all-sessions-auto-save is present in auto-save-hook"
           "No save-all-sessions-auto-save is present in auto-save-hook")
         (if (member #'save-all-sessions-auto-save-immediately kill-emacs-hook)
             "Yes save-all-sessions-auto-save is present in kill-emacs-hook"
           "No save-all-sessions-auto-save is present in kill-emacs-hook"))
      (and
       (member #'save-all-sessions-auto-save auto-save-hook)
       (member #'save-all-sessions-auto-save-immediately kill-emacs-hook))))

  (when nil
    (defvar lotus-enable-desktop-restore-interrupting-feature-hook nil
      "feature that were disabled for proper restoring of desktop will get re-enabled here.")
    )

  (defun desktop-idle-create-buffers ()
    "Create buffers until the user does something, then stop.
If there are no buffers left to create, kill the timer."
    (let ((tags-add-tables nil))
      (let ((repeat 1))
        (while (and repeat desktop-buffer-args-list)
          (unless (ignore-errors
                    (save-window-excursion
                      (desktop-lazy-create-buffer)))
            (message "Desktop lazily opening Failed."))
          (setq repeat (sit-for 0.2))
          (unless desktop-buffer-args-list
            (cancel-timer desktop-lazy-timer)
            (setq desktop-lazy-timer nil)
            (message "Lazy desktop load complete")
            (sit-for 3)
            (message nil))))))

  (defadvice desktop-idle-create-buffers (after desktop-idle-complete-actions)
    "This advice will finally run lotus-enable-desktop-restore-interrupting-feature-hook
when all buffer were creaed idly."
    (unless desktop-buffer-args-list
      (funcall sessions-unified-utils-notify "desktop-idle-complete-actions"
               "Enabled session saving")
      (lotus-enable-session-saving-immediately)
      (progn
        (ad-disable-advice 'desktop-idle-create-buffers 'after 'desktop-idle-complete-actions)
        (ad-update 'desktop-idle-create-buffers)
        (ad-activate 'desktop-idle-create-buffers))
      (run-each-hooks 'lotus-enable-desktop-restore-interrupting-feature-hook)))

  ;; use session-restore to restore the desktop manually


  ;; (debug)

  ;;;###autoload
  (defun lotus-desktop-session-restore ()
    "Restore a saved emacs session."
    (interactive)
    (if *session-unified-desktop-enabled*
        (progn
          ;; ask user about desktop to restore, and use it for session.
          ;; will set *desktop-save-filename*
          (if (desktop-get-desktop-save-filename)
              (let ((desktop-restore-frames nil)
                    (enable-local-eval t                ;query
                                       )
                    (enable-recursive-minibuffers t)
                    (flymake-run-in-place nil)
                    (show-error (called-interactively-p 'interactive))
                    (*constructed-name-desktop-save-filename*
                     (concat "^" (getenv "HOME") "/.emacs.d/.cache/autoconfig/desktop/emacs-desktop-" server-name)))
                (setq debug-on-error t)
                (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "entering lotus-desktop-session-restore")

                (if (not (string-match *constructed-name-desktop-save-filename* *desktop-save-filename*))
                    (progn
                      (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "*desktop-save-filename* is not equal to %s but %s"
                               *constructed-name-desktop-save-filename*
                               *desktop-save-filename*)
                      (if (y-or-n-p (format "lotus-desktop-session-restore" "*desktop-save-filename* is not equal to %s but %s\nshould continue with it ? "
                                            *constructed-name-desktop-save-filename*
                                            *desktop-save-filename*))
                          (message "continuing..")
                        (error "desktop file %s is not correct" *desktop-save-filename*)))

                  (progn
                    (unless (lotus-desktop-saved-session)
                      (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "%s not found so trying to checkout it." *desktop-save-filename*)
                      (vc-checkout-file *desktop-save-filename*))

                    (if (lotus-desktop-saved-session)
                        (progn
                          (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "lotus-desktop-session-restore")
                          (progn            ;remove P4
                            (setq vc-handled-backends (remove 'P4 vc-handled-backends))
                            (add-hook 'lotus-enable-desktop-restore-interrupting-feature-hook
                                      '(lambda ()
                                         (add-to-list 'vc-handled-backends 'P4))))
                          (if show-error

                              (if (desktop-vc-read *desktop-save-filename*)
                                  (progn
                                    (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "desktop loaded successfully :)")
                                    (lotus-enable-session-saving)
                                    (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "Do you want to set session of frame? ")
                                    (when (y-or-n-p-with-timeout
                                           "Do you want to set session of frame? "
                                           10 t)
                                      (let ((*frame-session-restore* t))
                                        (frame-session-restore (selected-frame)))))
                                (progn
                                  (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "desktop loading failed :(")
                                  (run-at-time "1 sec" nil '(lambda () (insert "lotus-desktop-session-restore")))
                                  (execute-extended-command nil)
                                  nil))

                            (condition-case e
                                (if (let ((desktop-restore-in-progress t))
                                      (desktop-vc-read *desktop-save-filename*))
                                    (progn
                                      (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "desktop loaded successfully :)")
                                      (lotus-enable-session-saving))
                                  (progn
                                    (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "desktop loading failed :(")
                                    nil))
                              ('error
                               (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "Error in desktop-read: %s\n not adding save-all-sessions-auto-save to auto-save-hook" e)
                               (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "Error in desktop-read: %s try it again by running M-x lotus-desktop-session-restore" e)
                               (run-at-time "1 sec" nil '(lambda () (insert "lotus-desktop-session-restore")))
                               (condition-case e
                                   (execute-extended-command nil)
                                 ('error (message "M-x lotus-desktop-session-restore %s" e))))))
                          t)
                      (when (y-or-n-p
                             (funcall sessions-unified-utils-notify "lotus-desktop-session-restore"
                                      "No desktop found. or you can check out old %s from VCS.\nShould I enable session saving in auto save, at kill-emacs ?"
                                      *desktop-save-filename*))
                        (lotus-enable-session-saving)))
                    (let ((enable-recursive-minibuffers t))
                      (when t ;(y-or-n-p-with-timeout "Do you want to set session of frame? " 7 t)
                        (frame-session-restore (selected-frame) t)))
                    (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "leaving lotus-desktop-session-restore"))))
            (funcall sessions-unified-utils-notify "lotus-desktop-session-restore" "desktop-get-desktop-save-filename failed")))
      (progn
        (lotus-enable-session-saving-immediately)
        (run-each-hooks 'lotus-enable-desktop-restore-interrupting-feature-hook)
        (message
         "*session-unified-desktop-enabled* %s" *session-unified-desktop-enabled*))))

  ;; (add-hook 'session-before-save-hook
  ;;           'my-desktop-save)

  ;; (when nil
  ;;   ;; moved to lotus-desktop-session-restore
  ;;   (eval-after-load "session"
  ;;     '(add-hook 'session-before-save-hook 'my-desktop-save)))

  ;; 'lotus-desktop-session-save)

  ;; (testing
  ;;  (remove-hook 'session-before-save-hook
  ;;               'my-desktop-save))

  ;; ;; ask user whether to restore desktop at start-up
  (when nil
    (add-hook ;; 'after-init-hook
     'lotus-enable-startup-interrupting-feature-hook
     '(lambda ()
        (run-at-time-or-now 7 'lotus-desktop-session-restore))))

  ;; Then type ‘M-x session-save’, or ‘M-x session-restore’ whenever you want to save or restore a desktop. Restored desktops are deleted from disk.

  ;;}}


  ;; ----------------------------------------------------------------------------
  ;;;###autoload
  (defun desktop-read-alternate (&optional dirname)
    "Read and process the desktop file in directory DIRNAME.
Look for a desktop file in DIRNAME, or if DIRNAME is omitted, look in
directories listed in `desktop-path'.  If a desktop file is found, it
is processed and `desktop-after-read-hook' is run.  If no desktop file
is found, clear the desktop and run `desktop-no-desktop-file-hook'.
This function is a no-op when Emacs is running in batch mode.
It returns t if a desktop file was loaded, nil otherwise."
    (interactive)
    (unless noninteractive
      (setq desktop-dirname
            (file-name-as-directory
             (expand-file-name
              (or
               ;; If DIRNAME is specified, use it.
               (and (< 0 (length dirname)) dirname)
               ;; Otherwise search desktop file in desktop-path.
               (let ((dirs desktop-path))
                 (while (and dirs
                             (not (file-exists-p
                                   (desktop-full-file-name (car dirs)))))
                   (setq dirs (cdr dirs)))
                 (and dirs (car dirs)))
               ;; If not found and `desktop-path' is non-nil, use its first element.
               (and desktop-path (car desktop-path))
               ;; Default: Home directory.
               "~"))))
      (if (file-exists-p (desktop-full-file-name))
          ;; Desktop file found, but is it already in use?
          (let ((desktop-first-buffer nil)
                (desktop-buffer-ok-count 0)
                (desktop-buffer-fail-count 0)
                (owner (desktop-owner))
                ;; Avoid desktop saving during evaluation of desktop buffer.
                (desktop-save nil))
            (if (and owner
                     (memq desktop-load-locked-desktop '(nil ask))
                     (or (null desktop-load-locked-desktop)
                         (not (y-or-n-p
                               (format
                                "Warning: desktop file appears to be in use by PID %s.\nUsing it may cause conflicts.  Use it anyway? "
                                owner)))))
                (let ((default-directory desktop-dirname))
                  (setq desktop-dirname nil)
                  (run-hooks 'desktop-not-loaded-hook)
                  (unless desktop-dirname
                    (message "Desktop file in use; not loaded.")))
              (desktop-lazy-abort)
              ;; Evaluate desktop buffer and remember when it was modified.
              (load (desktop-full-file-name) t t t)
              (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
              ;; If it wasn't already, mark it as in-use, to bother other
              ;; desktop instances.
              (unless owner
                (condition-case nil
                    (desktop-claim-lock)
                  (file-error (message "Couldn't record use of desktop file")
                              (sit-for 1))))

              ;; `desktop-create-buffer' puts buffers at end of the buffer list.
              ;; We want buffers existing prior to evaluating the desktop (and
              ;; not reused) to be placed at the end of the buffer list, so we
              ;; move them here.
              (mapc 'bury-buffer
                    (nreverse (cdr (memq desktop-first-buffer (nreverse (buffer-list))))))
              (switch-to-buffer (car (buffer-list)))
              (run-hooks 'desktop-delay-hook)
              (setq desktop-delay-hook nil)
              (run-hooks 'desktop-after-read-hook)
              (message "Desktop: %d buffer%s restored%s%s."
                       desktop-buffer-ok-count
                       (if (= 1 desktop-buffer-ok-count) "" "s")
                       (if (< 0 desktop-buffer-fail-count)
                           (format ", %d failed to restore" desktop-buffer-fail-count)
                         "")
                       (if desktop-buffer-args-list
                           (format ", %d to restore lazily"
                                   (length desktop-buffer-args-list))
                         ""))
              t))
        ;; No desktop file found.
        (desktop-clear)
        (let ((default-directory desktop-dirname))
          (run-hooks 'desktop-no-desktop-file-hook))
        (message "No desktop file.")
        nil)))


  (progn ;; "desktop-settings"

    (defmacro desktop-get-readonly-proof-mode (modefn)
      `'(lambda (desktop-buffer-locals)
          (unless (or desktop-buffer-read-only buffer-read-only)
            (condition-case e
                (,modefn 1)
              ('error (message "%s: %s" ,modefn e))))))))


;;For Session
(with-eval-after-load "session" ;;

  ;; (setq desktop-path '("~/.emacs.d/"))
  ;; (setq desktop-dirname "~/.emacs.d/")
  ;; (setq desktop-base-file-name
  ;;       (concat
  ;;        "emacs-desktop"
  ;;        (if (boundp 'server-name)
  ;;            (concat "-" server-name))))

  ;; (defvar *desktop-save-filename* (expand-file-name desktop-base-file-name desktop-dirname))
  (setq session-save-file (auto-config-file "session/session.el"))

  (defun lotus-session-saved-session ()
    (if (file-exists-p session-save-file) session-save-file))

  (defun session-vc-save-session ()
    (if (lotus-session-saved-session)
        (put-file-in-rcs session-save-file))
    (session-save-session))

  (defun session-vc-restore-session ()
    (unless (lotus-session-saved-session)
      (message "lotus-session-vc-session-restore: %s not found so trying to checkout it." session-save-file)
      (vc-checkout-file session-save-file))

    (or session-successful-p
	(setq session-successful-p
	      (and session-save-file
		   (condition-case nil
		       (progn
			 ;; load might fail with coding-system = emacs-mule
			 (load session-save-file t nil t)
			 (run-hooks 'session-after-load-save-file-hook)
			 t)
                     (error nil))))))



  (add-hook 'after-init-hook '(lambda ()
                               (setq session-initialize t)
                               (session-initialize)
                               (remove-hook 'kill-emacs-hook
                                ;; done in save-all-sessions-auto-save
                                'session-save-session)))
  ;; (add-hook 'kill-emacs-hook 'session-vc-save-session)


  (setq session-initialize t)

  ;;{{ http://www.emacswiki.org/emacs/EmacsSession

  ;; There is a function in session that’s not really persistence
  ;; related – ‘session-jump-to-last-change’ <C-x C-/>. This is the
  ;; singular most useful function of any Emacs add-on to me. It moves
  ;; the point to the last modified location. Keep calling it and you
  ;; will visit all the locations you’ve made
  ;; modifications. Absolutely brilliant. Unobstrusive, unlike
  ;; highlight-changes-mode.

  ;; However, it doesn’t automatically reveal folded sections. Here is
  ;; the fix:


  ;; expanded folded secitons as required
  (defun le::maybe-reveal ()
    (when (and (or (memq major-mode  '(org-mode outline-mode))
                   (and (boundp 'outline-minor-mode)
                        outline-minor-mode))
               (outline-invisible-p))
      (if (eq major-mode 'org-mode)
          (org-reveal)
          (show-subtree))))

  (add-hook 'session-after-jump-to-last-change-hook
            'le::maybe-reveal)
  ;;}}
  ;;  (session-initialize))
  ;; Something like this is recommended to get emacs to shut-up
  ;; and never ask you for a coding system. Otherwise this can
  ;; happen on *every* desktop-save triggered by the auto-save-hook:
  (prefer-coding-system 'utf-8)




  (add-hook 'delete-frame-functions
            '(lambda (frame)
              (if (and
                   (< (length (frame-list)) 3)
                   (functionp 'session-save-sessoin))
                  (session-save-sessoin)))))

(require 'savehist-20+)

(when (featurep 'savehist-20+)
  ;; savehist: save some history
  (setq savehist-additional-variables    ;; also save...
        '(search ring regexp-search-ring)    ;; ... my search entries
        savehist-autosave-interval 60        ;; save every minute (default: 5 min)
        savehist-file (auto-config-file "savehist/savehist.el"))   ;; keep my home clean
  ;; do customization before activation
  (savehist-mode t))

;; TODO: find it
;; (require 'workspaces)

;; (deh-require-maybe desktop-recover
;;   ;; ssee:http://www.emacswiki.org/emacs/DesktopRecover
;;   ;; from: https://github.com/doomvox/desktop-recover/blob/master/desktop-recover-setup.el
;;   ;; And this brings up the interactive buffer restore menu
;;   (desktop-recover-interactive))

;; (deh-require-maybe desktopaid
(testing
  ;; see: http://desktopaid.sourceforge.net/
  (dta-hook-up))

;; (deh-require-maybe frame-restore
;;   ;; check this library will know what to do.
;;   ;; http://www.emacswiki.org/emacs/frame-restore.el
;;   )

(require 'revive)
(require 'tapestry)

;; first test it with startup
;; (deh-require-maybe winner
;;   ;; see: http://emacs.wordpress.com/2007/01/28/simple-window-configuration-management/
;;   (winner-mode 1))

(with-eval-after-load "tapestry"
  ;; http://superuser.com/questions/383560/how-to-restore-emacs-windows-and-buffers-from-the-last-session
  (defvar my-tapestry-file "~/.tapestry")

  (defun load-my-tapestry ()
    (interactive)
    (let ((b (find-file-noselect my-tapestry-file)))
      (sit-for 0)
      (set-tapestry (read b))
      (kill-buffer b)))

  (defun save-my-tapestry ()
    (interactive)
    (let ((tap (tapestry)))
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (setcar tap (make-list (length (car tap)) nil))
          (print tap)
          (write-region (point-min) (point-max) my-tapestry-file)))))

  (add-hook 'kill-emacs-hook 'save-my-tapestry))


(progn ;; "emacs session management"
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Session-Management.html
  ;; {{
  ;; 39.17 Session Management

  ;; Emacs supports the X Session Management Protocol, which is used
  ;; to suspend and restart applications. In the X Window System, a
  ;; program called the session manager is responsible for keeping
  ;; track of the applications that are running. When the X server
  ;; shuts down, the session manager asks applications to save their
  ;; state, and delays the actual shutdown until they respond. An
  ;; application can also cancel the shutdown.

  ;; When the session manager restarts a suspended session, it directs
  ;; these applications to individually reload their saved state. It
  ;; does this by specifying a special command-line argument that says
  ;; what saved session to restore. For Emacs, this argument is
  ;; ‘--smid session’.  — Variable: emacs-save-session-functions

  ;; Emacs supports saving state via a hook called
  ;; emacs-save-session-functions. Emacs runs this hook when the
  ;; session manager tells it that the window system is shutting
  ;; down. The functions are called with no arguments, and with the
  ;; current buffer set to a temporary buffer. Each function can use
  ;; insert to add Lisp code to this buffer. At the end, Emacs saves
  ;; the buffer in a file, called the session file.

  ;; Subsequently, when the session manager restarts Emacs, it loads
  ;; the session file automatically (see Loading). This is performed
  ;; by a function named emacs-session-restore, which is called during
  ;; startup. See Startup Summary.

  ;; If a function in emacs-save-session-functions returns non-nil,
  ;; Emacs tells the session manager to cancel the shutdown.

  ;; Here is an example that just inserts some text into *scratch*
  ;; when Emacs is restarted by the session manager.

  ;; (add-hook 'emacs-save-session-functions 'save-yourself-test)

  (defun save-yourself-test ()
    (insert "(save-current-buffer
       (switch-to-buffer \"*scratch*\")
       (insert \"I am restored\"))")
    nil)

;; }}
  )

(progn ;; "undo-history"
  ;; http://stackoverflow.com/questions/2985050/is-there-any-way-to-have-emacs-save-your-undo-history-between-sessions

  (progn ;; "undo funs"
    (defun save-undo-filename (orig-name)
      "given a filename return the file name in which to save the undo list"
      (concat (file-name-directory orig-name)
              "."
              (file-name-nondirectory orig-name)
              ".undo"))

    (defun save-undo-list ()
      "Save the undo list to a file"
      (save-excursion
        (ignore-errors
          (let ((undo-to-save `(setq buffer-undo-list ',buffer-undo-list))
                (undo-file-name (save-undo-filename (buffer-file-name))))
            (find-file undo-file-name)
            (erase-buffer)
            (let (print-level
                  print-length)
              (print undo-to-save (current-buffer)))
            ;; (let ((write-file-hooks (remove 'save-undo-list write-file-hooks)))
            (let ((write-file-functions (remove 'save-undo-list write-file-functions)))
              (save-buffer))
            (kill-buffer))))
      nil)

    (defvar handling-undo-saving nil)

    (defun load-undo-list ()
      "load the undo list if appropriate"
      (ignore-errors
        (when (and
               (not handling-undo-saving)
               (null buffer-undo-list)
               (file-exists-p (save-undo-filename (buffer-file-name))))
          (let* ((handling-undo-saving t)
                 (undo-buffer-to-eval (find-file-noselect (save-undo-filename (buffer-file-name)))))
            (eval (read undo-buffer-to-eval))))))

    ;; (add-hook 'write-file-hooks 'save-undo-list)

    ;; (remove-hook 'write-file-functions 'save-undo-list)
    ;; (remove-hook 'find-file-hook 'load-undo-list)
    )


  (add-to-list 'desktop-locals-to-save 'buffer-undo-list)
  (add-to-list 'session-locals-include 'buffer-undo-list)
  (setq undo-tree-auto-save-history t))






(provide 'sessions-unified)
;;; session-config.el ends here
