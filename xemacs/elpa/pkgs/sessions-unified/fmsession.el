;;; fmsession.el --- Frame session management        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <spratap@merunetworks.com>
;; Keywords: convenience, frames, internal, tools

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

(provide 'fmsession)


(require 'elscreen)
(require 'emacs-panel)


(defvar session-unified-utils-select-frame-fn #'select-frame-set-input-focus "session-unified-utils-select-frame-fn")


;; Not required
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

;; Not required
(defun sessions-unified-set-alist (symbol key value)
  "Set cdr of an element (KEY . ...) in the alist bound to SYMBOL to VALUE."
  (or (boundp symbol)
      (set symbol nil))
  (set symbol (sessions-unified-put-alist key value (symbol-value symbol))))

;; (lotus-elscreen-get-screen-to-name-alist)
(with-eval-after-load "elscreen"
  (defun lotus-elscreen-get-screen-to-name-alist ()
    ;; (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
    (elscreen-notify-screen-modification-suppress
     (elscreen-set-window-configuration (elscreen-get-current-screen)
                                        (elscreen-current-window-configuration))
     (let* ((lexical-binding nil)
            (screen-list (sort (elscreen-get-screen-list) '<))
            screen-name
            screen-to-name-alist
            nickname-type-map)
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

             ;; (sessions-unified-set-alist 'screen-to-name-alist screen screen-name)
             (push (cons screen screen-name) screen-to-name-alist ))
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

                  (setq buff-files (cdr buff-files))

                  (message "progn buff-files: %s" buff-files)
                  (testing (message "else"))))

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
                (setq *elscreen-session-restore-data* session-current-buffer-file))))
              ;; (if (get-buffer buff)
              ;;     (progn
              ;;       (setq *elscreen-session-restore-data* (list (cons 'cb session-current-screen-buffers)))
              ;;       (testing
              ;;        (message "*elscreen-session-restore-data* %s" *elscreen-session-restore-data*)))
              ;;     (testing
              ;;      (message "in when session-current-screen-buffers %s" session-current-screen-buffers)))

             ;; (let* ((desktop-buffers
          (testing
           (message "elscreen-notify-screen-modification"))
          (elscreen-notify-screen-modification 'force-immediately)
          (message "elscreen-session-session-list-set: DONE."))

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
      (when elscreen-session-list
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

  (when nil))
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

  ;;}}


  ;;{{
(progn ;; "per frame session"

  ;; (require 'emacs-panel)

  (defvar *desktop-vc-read-inprogress* nil "desktop-vc-read-inpgrogress")

  (defun frame-session-set-this-location (nframe &optional not-ask)
    (interactive
     (list (selected-frame)))
    (if nframe (funcall session-unified-utils-select-frame-fn nframe) (error "nframe is nil"))
    (message "in frame-session-set-this-location")
    (let* ((xwin-enabled (custom-display-graphic-p))
           (wm-hints
            (if xwin-enabled
                (ignore-errors (emacs-panel-wm-hints))))
           (desktop-name (if wm-hints
                             (nth
                              (cadr (assoc 'current-desktop wm-hints))
                              (cdr (assoc 'desktop-names wm-hints)))))
           (location (if (and
                          not-ask
                          desktop-name
                          (member desktop-name
                                  (mapcar #'car *frames-elscreen-session*)))
                         (progn
                           (message
                            "frame-session-set-this-location: NO need to call interactive (fmsession-read-location desktop-name=%s)"
                            desktop-name)
                           desktop-name)
                       (progn
                         (message
                          "frame-session-set-this-location: NEED to call interactive (fmsession-read-location desktop-name=%s)"
                          desktop-name)
                         ;; BUG: causing first emacsclient frame to be jammed which require pkill -USR2 emacs
                         (fmsession-read-location desktop-name)))))
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
          (if nframe (funcall session-unified-utils-select-frame-fn nframe) (error "nframe is nil"))
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

  (defun frame-session-restore-force (nframe)
    (frame-session-restore nframe t))

  (defun frame-session-apply (nframe)
    "Apply existing frame session to NFRAME."
    (interactive
     (list (selected-frame)))
    (progn
      (funcall session-unified-utils-select-frame-fn nframe)
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
    (add-hook 'after-make-frame-functions
              #'frame-session-restore-force
              t)
    (add-hook 'delete-frame-functions
              #'frame-session-save))

  (defun frame-session-restore-unhook-func ()
    "Add to hook"
    ;; (add-hook 'after-make-frame-functions 'frame-session-set-this-location t)
    (remove-hook 'after-make-frame-functions
                 #'frame-session-restore-force)
    (remove-hook 'delete-frame-functions
                 #'frame-session-save))

  (testing
   (frame-parameter (selected-frame) 'frame-spec-id)
   after-make-frame-functions
   delete-frame-functions
   *lotus-after-init-hook*))

  ;;}}


;;; fmsession.el ends here
