;; -*-lisp-*-
;;
;; Copyright 2009 Sharad Pratap
;;
;; Maintainer: Sharad Pratap
;;
;; This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA


;;;; remember-win

(in-package #:remember-win)

;;; "remember-win" goes here. Hacks and glory await!

;; (export '(*run-cli-program-hook*))


(export '(get-all-clis
          run-cli-command
          run-wcli-command
          process-pid))


(defvar *test-run* nil "Test")

(setq *test-run* t)

(defmacro test-run (&body body)
  `(when *test-run*
     ,@body))


(defun concat (&rest strings)
  (apply 'concatenate 'string strings))

(defparameter *testing*  nil "Set it when you need testing.")
(defmacro testing (&body body)
  "sdfdsf"
  (if (and (boundp '*testing*)
           *testing*)
      `(progn ,@body)))



(defvar *run-cli-program-hook* '()
  "A hook called whenever run-cli-progam called, it will be supplied with cli.")


;; TODO:
;;    * prepare all callee's to consider nil return vaule.
(defun cli-of-pid (pid)                    ;done.
  (if pid                                  ;yes pid could be nil.
      (let* ((statfilename (concatenate 'string "/proc/" (write-to-string pid) "/cmdline"))
             (cmdline (with-open-file (clifile statfilename :if-does-not-exist nil)
                        (if clifile
                            (read-line clifile)))))
                                        ; (testr cmdline)  ; yes for some pid return vaule is nil
        (if (and cmdline
                 (stringp cmdline) (not (string-equal "" cmdline)))
            (substitute #\Space #\Null (string-trim '(#\Null) cmdline))))))

;; test
(testing
  (string-trim '(#\Null) "sadfds[Ctrl-Q Space]sfdsf[Ctrl-Q Space]")
  (substitute #\Space #\Null "sfdsdf[Ctrl-Q Space]safd[Ctrl-Q Space]dsf")

  (defun testr (&optional x)
    "This function is for debugging."
    (let* ((tt (type-of x))
           (ttt (if (consp tt) (car tt) tt)))
      (read-one-line (current-screen) "test: " :initial-input
                     (if x
                         (concat (symbol-name ttt) " A" (if (stringp x) x "dn") "A ")
                         "empty"))))
  ;; test
  (test-run
    (symbol-name (car (type-of "")))
    (testr -1)
    (if (not (string-equal "" "")) "asfd")))

(defun process-pid (process)
  #+sbcl (sb-ext:process-pid process)
  #-sbcl (error 'not-implemented))

(defun process-status (process)
  #+sbcl (sb-ext:process-status process)
  #-sbcl (error 'not-implemented))


(defun pid-of-win (&optional (win (current-window)))
  (xproperty 'pid win))

(stumpwm::defcommand pid-of-window (&optional (win (current-window))) ()
  "pid-of-win"
  (if stumpwm::%interactivep%
      (message "~a" (xproperty 'pid win))
      (xproperty 'pid win)))

;; (defun set-property)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from window.lisp
;; (defun xwin-class (win)
;;   (multiple-value-bind (res class) (xlib:get-wm-class win)
;;     (declare (ignore res))
;;     class))
;;
;; (defun xwin-res-name (win)
;;   (multiple-value-bind (res class) (xlib:get-wm-class win)
;;     (declare (ignore class))
;;     res))
;;
;; Implement (defun (setf xwin-class)
;; take help of (xlib:set-wm-class (stumpwm::window-xwin (current-window)) "emacs" "Semacs")
;; could be found in /usr/share/common-lisp/source/clx
;; manager.lisp:68:(defun set-wm-class (window resource-name resource-class)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun get-windows-if (test window-list &key key) ;; return multiple ;; Correct it.
;;   (if key
;;       (remove-if-not test window-list :key key)
;;       (remove-if-not test window-list)))

;; (get-windows #'(lambda (p) (and p (= 4267 p))) (screen-windows (current-screen)) :key #'pid-of-win)

(defun all-windows ()
  "Windows in all screens."
  (mapcan (lambda (s) (copy-list (stumpwm::screen-windows s))) *screen-list*))


;; (defun get-windows-from-pid (pid &optional (window-list (screen-windows (current-screen)))) ;; return multiple ;; Correct it.
;;   (if pid                               ;yes here times when pid is nil
;;       (remove-if-not #'(lambda (p) (and p (= pid p)))
;;                      window-list
;;                      :key #'pid-of-win))
;;   (progn
;;     (stumpwm::dformat 3 "fun: get-windows-from-pid:: pid given is nil")
;;     nil))

(defun get-windows-from-pid (pid &optional (window-list (all-windows))) ;; return multiple ;; Correct it.
  (if pid                               ;yes here times when pid is nil
      (remove-if-not #'(lambda (p) (and p (= pid p)))
                     window-list
                     :key #'pid-of-win)
      (progn
        (stumpwm::dformat 3 "fun: get-windows-from-pid:: pid given is nil~%")
        nil)))

;; TEST optional form evaluated at run time.
;; (defun xx (&optional (w (screen-windows (current-screen))))
;;   w)
;; (xx)

;; (defun get-windows-from-cli (cli &optional (window-list (screen-windows (current-screen)))) ;; return multiple ;; Correct it.
;;   (remove-if-not #'(lambda (c) (and c (string-ignore-space= cli c)))
;;                  window-list
;;                  :key #'internal-cli-of-window))

(defun get-windows-from-cli (cli &optional (window-list (all-windows))) ;; return multiple ;; Correct it.
  (remove-if-not #'(lambda (c) (and c (string-ignore-space= cli c)))
                 window-list
                 :key #'internal-cli-of-window))

;; working.
;; (get-windows-from-cli "xterm -bg black")

;; (read-line (current-screen))

(defvar *gtd-debug-mesg* nil "Hi")

(progn
  (defun internal_intractive-msg (&rest args)
    (apply #'stumpwm::dformat 3 args)
    (read-one-line (current-screen)
                   (apply #'format nil args)
                   :initial-input "")
    (message ""))

  (defun intractive-msg (&rest args)
    (apply #'stumpwm::dformat 3 args))
  ;; (when nil (apply #'internal_intractive-msg args))

  (defun xintractive-msg (&rest args)
    (when t (apply #'internal_intractive-msg args))))

(progn
  (defun xlib-get-property (xwin prop
                            &key type (start 0) end delete-p (result-type 'list) transform)
    (handler-case
        (xlib:get-property xwin prop
                           :type type :start start :end end :delete-p delete-p :result-type result-type :transform transform)
      (xlib:window-error () nil)))

  (defun xlib-change-property (xwin prop data type format
                               &key (mode :replace) (start 0) end transform)
    (handler-case
        (xlib:change-property xwin prop data type format
                              :mode mode :start start :end end :transform transform)
      (xlib:window-error () nil))))

(defgeneric xproperty (prop obj)
  (:documentation "Get the property of object"))

(defgeneric (setf xproperty) (val prop obj)
  (:documentation "Get the property of object"))

(defmethod xproperty ((prop (eql 'cli)) (win stumpwm::window))
  (if win
      (let ((xwin (stumpwm::window-xwin win)))
        (when xwin
          (let ((utfcli (xlib-get-property xwin :STUMPWM_WCLI)))
            (if utfcli (stumpwm::utf8-to-string utfcli)))))))

(defmethod (setf xproperty) (val (prop (eql 'cli)) (win stumpwm::window))
  (when (and win
             (stringp val))
    (let ((xwin (stumpwm::window-xwin win)))
      (if xwin
          (xlib-change-property xwin
                                :STUMPWM_WCLI
                                (string-to-utf8 val)
                                :UTF8_STRING 8)))))

;; TODO: it return nil also. Yes do arrangement
(defmethod xproperty ((prop (eql 'pid)) (win stumpwm::window))
  (when win
    (let ((xwin (stumpwm::window-xwin win)))
      (first (xlib-get-property xwin :_NET_WM_PID)))))

(defmethod (setf xproperty) (val (prop (eql 'pid)) (win stumpwm::window))
  (when win
    (let ((pidlist (xproperty 'pid win)))
      (unless (member val pidlist)
        (let ((xwin (stumpwm::window-xwin win)))
          (xlib-change-property xwin
                                :_NET_WM_PID
                                (stumpwm::string-to-utf8 (append val pidlist))
                                :UTF8_STRING 8))))))

(defun get-all-clis (&optional
                     (getcli #'internal-cli-of-window)
                     (windows-list (all-windows))) ;supply unassigned cmdlines.
  (remove-if #'null (mapcar getcli windows-list)))

(defun complete-cmdline (&optional
                           (getcli #'internal-cli-of-window)
                           (prompt "command: " )
                           (initial-input "")
                           (windows-list (all-windows)) ;supply unassigned cmdlines.
                           (screen (current-screen)))
  (completing-read screen prompt
                   (remove-if #'null (mapcar getcli windows-list))
                   :initial-input initial-input))

(defun amend-list-to-table (list-items)
  (labels ((amend-make-string (item)
             (cond
               ((stringp item) item)
               ((symbolp item) (symbol-name item))
               (t "unknown")))

           (amend-list-make-cons (items)
             (mapcar #'(lambda (item)
                         (if (consp item)
                             item
                             (list item item)))
                     items)))
    (mapcar #'(lambda (item)
                (cons (amend-make-string (car item)) (cdr item)))
            (amend-list-make-cons list-items))))

(defun choose-or-provide (choices
                          &key
                            (dialog "select: ")
                            (autoselect-if-only t)
                            (choice-autoselect 1)
                            (choice-time-out-seconds 0)
                            (screen (current-screen))
                            (extra-choices `(("New" ,#'complete-cmdline)
                                             ("nothing" nil))))
  (let* ((mchoice (cond
                    ((and autoselect-if-only (= (length choices) 1)) (car choices))
                    ((and
                      choice-autoselect ;can be zero and that will be t
                      (zerop choice-time-out-seconds))
                     (nth-max  (- choice-autoselect 1) choices))
                    (t (handler-case
                           (sb-ext:with-timeout (if (or (zerop choice-autoselect)
                                                        (null choice-autoselect))
                                                    0
                                                    choice-time-out-seconds)            ;so it will not hang to wait for forever.
                             (stumpwm::select-from-menu screen
                                                        (amend-list-to-table (append choices extra-choices))
                                                        dialog))
                         (sb-ext:timeout ()
                           (nth-max  (- choice-autoselect 1) choices))))))
         (choice
           (cond
             ((listp mchoice) (second mchoice))
             (t mchoice))))
    (if (functionp choice)
        (funcall choice)
        choice)))

(progn
  ;; (defvar *a-mutex* (sb-thread:make-mutex :name "my lock"))

  (let (unresolved-cli-windows                     ;window created after run-wcli-command, and which did not got cli-of-window
        cmdlines
        assigned-cmdlines
        unassigned-cmdlines-current
        (choice-time-out-seconds 2)
        (focus-unresolved-cli-window-p t)
        (resolve-before-focus-p t)
        (autoselect-if-only-p t))


    (progn
      (progn
        (defcommand set-choice-time-out-seconds (&optional (level 1)) ((:number "Choice Timeuut Seconds: "))
          (setf choice-time-out-seconds level)))

      (progn                            ;autoselect-if-only-p
        (stumpwm::defcommand enable-autoselect-if-only () ()
          (setf
           autoselect-if-only-p t))

        (stumpwm::defcommand disable-autoselect-if-only () ()
          (setf
           autoselect-if-only-p nil))

        (stumpwm::defcommand toggle-autoselect-if-only () ()
          (setf
           autoselect-if-only-p (not autoselect-if-only-p))))

      (progn                            ;resolve-before-focus-p
        (stumpwm::defcommand enable-resolve-before-focus () ()
          (setf
           resolve-before-focus-p t))

        (stumpwm::defcommand disable-resolve-before-focus () ()
          (setf
           resolve-before-focus-p nil))

        (stumpwm::defcommand toggle-resolve-before-focus () ()
          (setf
           resolve-before-focus-p (not resolve-before-focus-p))))

      (progn                            ;focus-unresolved-cli-window-p
        (stumpwm::defcommand enable-focus-unresolved-cli-window () ()
          (setf
           focus-unresolved-cli-window-p t))

        (stumpwm::defcommand disable-focus-unresolved-cli-window () ()
          (setf
           focus-unresolved-cli-window-p nil))

        (stumpwm::defcommand toggle-focus-unresolved-cli-window () ()
          (setf
           focus-unresolved-cli-window-p (not focus-unresolved-cli-window-p)))))


    (progn



      (define-stumpwm-type :cmdline (input prompt)
        (or
         (choose-or-provide (get-unassigned-cmdlines)
                            :dialog prompt
                            :autoselect-if-only t
                            :choice-time-out-seconds 100
                            :extra-choices nil)
         (throw 'error "Abort.")))


      ;; I want both

      ;; In ideal case these two will not be required, as above two
      ;; function will always take care of unresolved-cli-windows

      ;; (defun set-new-window-before-focus (&optional window (current-window))
      ;;   (sb-ext:with-timeout 1
      ;;     (sb-thread:with-mutex (*a-mutex*) ; :value 1);  :wait-p t)
      ;;       (push window unresolved-cli-windows)
      ;;       (read-one-line (current-screen) "A Not able to find cli for you: " :initial-input  "")
      ;;     )))

      (stumpwm::defcommand find-new-window (&optional (win (current-window))) ()
        (message "ask ~a found ~a " win (find win unresolved-cli-windows)))

      (defun remove-new-window (&optional (window (current-window)))
        (if (find window unresolved-cli-windows)
            (setf
             unresolved-cli-windows (remove window unresolved-cli-windows))
            (stumpwm::dformat 3  "not found  ~a to delete, val of find ~a" window (find window unresolved-cli-windows))))
      ;; (if (find window unresolved-cli-windows)
      ;;     (let ((len (length unresolved-cli-windows)))
      ;;       ; (delete window unresolved-cli-windows)
      ;;       (setf unresolved-cli-windows (remove window unresolved-cli-windows))
      ;;       (if (> len (length unresolved-cli-windows))
      ;;           (message "removed ~a len ~a nowlen ~a" window len (length unresolved-cli-windows))
      ;;           (message "NOT removed ~a, len ~a nowlen ~a" window len (length unresolved-cli-windows))))
      ;;     (message "not found  ~a to delete, val of find ~a" window (find window unresolved-cli-windows))))

      (defun set-new-window-before-focus (&optional (window (current-window)))
        ;; (declare (ignore current-window))
        (intractive-msg  "set-new-window-before-focus: BEFORE: START")
        (when window
          (if (and
               (null (xproperty 'cli window))
               (get-unassigned-cmdlines))
              (progn
                (push window unresolved-cli-windows)
                (intractive-msg "set-new-window-before-focus: BEFORE: Success pushed window ~a" window))
              (progn
                (intractive-msg "set-new-window-before-focus: BEFORE: NULL: (xproperty 'cli window): ~a" (xproperty 'cli window))
                (intractive-msg "set-new-window-before-focus: BEFORE: T: (get-unassigned-cmdlines): ~a" (get-unassigned-cmdlines)))))
                                        ; (setf unresolved-cli-windows (append unresolved-cli-windows (list window))))
        (stumpwm::dformat 3 "set-new-window-before-focus: BEFORE: END set-new-window-before-focus ~w~%" window))

      (defun get-unresolved-cli-windows ()
        unresolved-cli-windows)
                                        ;(read-one-line (current-screen) "A Not able to find cli for you: " :initial-input  ""))

      (defun set-new-window-after-focus (current last)
        ;; (if (find current unresolved-cli-windows)
        ;;     (read-one-line (current-screen) (format nil "able to find current win ~w, last win: ~w~%~% allwin: ~%~{~w~%~}" current last unresolved-cli-windows) :initial-input "")
        ;;     (read-one-line (current-screen) (format nil "not able find current win ~w, last win: ~w~%~% allwin: ~%~{~w~%~}" current last unresolved-cli-windows) :initial-input ""))

        ;; TODO: need to investigate why
        ;; called from
        ;; set-cli-new-window
        ;; set-cli-focus-window
        ;;
        ;; where in set-cli-new-window no option given and displaying set cli
        ;; while in set-cli-focus-window displaying set cli and may ask options also.
        ;;
        (intractive-msg "set-new-window-after-focus: AFTER: START c=~a l=~a" current last)
        (when current
          (if (and
               (or
                (find current unresolved-cli-windows)
                (null last))                ;as *new-window-hook* not launched when last win is null.
               (get-unassigned-cmdlines)
               (null (xproperty 'cli current)))
              (let ((cli-huristic
                      (find-cli-huristic current
                                         :choice-time-out-seconds choice-time-out-seconds
                                         :autoselect-if-only      (and
                                                                   autoselect-if-only-p
                                                                   ;;Culprit
                                                                   (if (> (length (get-unresolved-cli-windows)) 1)
                                                                       nil
                                                                       t)))))
                (setf (cli-of-window current) cli-huristic)
                (setf unresolved-cli-windows (remove current unresolved-cli-windows))
                (intractive-msg "set-new-window-after-focus: AFTER: Success found cli for current window ~a" current))
              (progn
                (when (and              ;newly added code
                       (null (get-unassigned-cmdlines))
                       (null (xproperty 'cli current)))
                  (let* ((pid (pid-of-win current))
                         (cli (cli-of-pid pid)))
                    (setf unresolved-cli-windows (remove current unresolved-cli-windows))
                    (setf (cli-of-window current)
                          (or cli ":anonymous:"))))
                (intractive-msg "set-new-window-after-focus: AFTER: T: (or (find current unresolved-cli-windows) (null last)): ~a" (or (find current unresolved-cli-windows) (null last)))
                (intractive-msg "set-new-window-after-focus: AFTER: T: (get-unassigned-cmdlines): ~a" (get-unassigned-cmdlines))
                (intractive-msg "set-new-window-after-focus: AFTER: NULL: (xproperty 'cli current): ~a" (xproperty 'cli current)))))
        ;; (message "new ~a , old ~a" current last)
        (unless current
          (stumpwm::dformat 3 "why current is null ?"))
        (stumpwm::dformat 3 "AFTER: END set-new-window-after-focus ~w~%" current))

      (stumpwm::defcommand show/unresolved-cli-windows-for-focus () ()
        "show/unresolved-cli-windows-for-focus"
        (if unresolved-cli-windows
            (message "~{~w~%~}" unresolved-cli-windows)
            (message "No windows for focus.")))

      (stumpwm::defcommand clear/unresolved-cli-windows-for-focus () ()
        "clear/unresolved-cli-windows-for-focus"
        (setf unresolved-cli-windows nil))

      (defun set-focus-unresolved-cli-window (window &optional force)
        ;; TODO pending trying to add code to resolve case when wcli window not get focus
        ;; make it toggle-able.
        (stumpwm::dformat 3 "set-focus-unresolved-cli-window: BEGIN window is ~a force ~a~%" window force)
        (if window
            (when (or
                   force
                   (find window unresolved-cli-windows))
              ;; TODO: how to detect if window did not get focus
              (let ((frame (stumpwm::window-frame window)))
                (if frame
                    (stumpwm::focus-frame (stumpwm::window-group window) frame)
                    (stumpwm::dformat 3 "set-focus-unresolved-cli-window: window ~w not have frame~%" window))))
            (stumpwm::dformat 3 "set-focus-unresolved-cli-window: window is nil~%")))

      (defun set-cli-new-window (&optional (window (current-window)))
        (intractive-msg "set-cli-new-window: START")
        (set-new-window-before-focus window)

        (let ((win-belong-unrslv-cli
                (find window unresolved-cli-windows)))

          (when resolve-before-focus-p
            ;; TODO: need to investigate why see fun #'set-new-window-after-focus
            (set-new-window-after-focus window nil))

          (when focus-unresolved-cli-window-p
            (set-focus-unresolved-cli-window
             window
             win-belong-unrslv-cli)))

        (intractive-msg "set-cli-new-window: END"))

      (defun set-cli-focus-window (current last)
        (intractive-msg "set-cli-focus-window: START")

        (when t
          (set-new-window-before-focus current)
          (set-new-window-after-focus current last))

        (intractive-msg "set-cli-focus-window: END"))

      (when t
        (setf
         *new-window-hook* nil
         *focus-window-hook* nil
         *destroy-window-hook* nil)

        (setf *new-window-hook* nil)
        (setf *focus-window-hook* nil)
        (setf *destroy-window-hook* nil))

      )

    (add-hook *destroy-window-hook* #'remove-new-window)
    ;; (add-hook *new-window-hook* #'set-new-window-before-focus)
    ;; (add-hook *focus-window-hook* #'set-new-window-after-focus)

    (add-hook *new-window-hook* #'set-cli-new-window)
    (add-hook *focus-window-hook* #'set-cli-focus-window)


    (progn

      (stumpwm::defcommand internal-cli-of-window (&optional (win (current-window))) ()
        "Show cli of window"
        (let* ((window-cli
                 (or (xproperty 'cli win) ;it must have priority
                     (cli-of-pid (pid-of-win win)))))
          (if stumpwm::%interactivep%
              (let ((new-supplied-window-cli
                      (complete-cmdline #'internal-cli-of-window
                                        (if window-cli
                                            "wincli: "
                                            "NULL wincli is here, provide new wincli: ")
                                        (if window-cli window-cli ""))))
                (when new-supplied-window-cli
                  (setf (internal-cli-of-window win) new-supplied-window-cli)))
              window-cli)))

      (defun (setf internal-cli-of-window) (cli-string &optional (win (current-window)))
        "Set the window tag set for a window"

        (let ((cli-string (if cli-string (string-trim '(#\Space #\Tab #\Newline) cli-string)))
              (cli-pid    (cli-of-pid (pid-of-win win)))
              (cli-win    (internal-cli-of-window win))
              (oldcliprop (xproperty 'cli win)))

          (intractive-msg "internal-cli-of-window: cli-string ~a, cli-pid ~a, cli-win ~a, oldcliprop ~a" cli-string cli-pid cli-win oldcliprop)

          (prog1

              (when (and cli-string
                         (stringp cli-string))
                (setf (xproperty 'cli win) cli-string))

            ;; (unless (and
            ;;          cli-string (stringp cli-string)
            ;;          cli-pid    (stringp cli-pid)
            ;;          (string-ignore-space= cli-string cli-pid)
            ;;          (string-ignore-space= cli-string cli-win))
            ;;   (setf (xproperty 'cli win) cli-string))

            ;; clean unassigned-cmdlines-current for all cases
            (setf unassigned-cmdlines-current
                  (if (and oldcliprop
                           (null (string-ignore-space= oldcliprop cli-string)))
                      (if (member cli-string unassigned-cmdlines-current
                                  :test #'string-ignore-space=)
                          (substitute oldcliprop cli-string unassigned-cmdlines-current :test #'string-ignore-space=)
                          (append unassigned-cmdlines-current (list oldcliprop)))
                      (remove cli-string ;not working find reason.
                              unassigned-cmdlines-current
                              :test #'string-ignore-space=)))))))

    (stumpwm::defcommand run-cli-command (cmd
                                          &optional
                                          (output nil)
                                          (wait nil))
      ((:shell "program: "))
      "Run the specified shell command. If @var{collect-output-p} is @code{T}
then run the command synchonously and collect the output. Be
careful. If the shell command doesn't return, it will hang StumpWM. In
such a case, kill the shell command to resume StumpWM."
      (let* ((cmdlist (remember-win:parse cmd)))
        (stumpwm::run-prog (first cmdlist)
                           :args (rest cmdlist)
                           :wait wait
                           :output output
                           :search (getenv "PATH"))))

    ;; (let (unassigned-cmdlines-current) ;Command line that have not assigned
    (stumpwm::defcommand run-wcli-command (cmd
                                           &optional
                                           (background nil)
                                           (output nil)
                                           (wait nil))
      ((:shell "program: "))
      "Run the specified shell command. If @var{collect-output-p} is @code{T}
then run the command synchonously and collect the output. Be
careful. If the shell command doesn't return, it will hang StumpWM. In
such a case, kill the shell command to resume StumpWM."
      (let* ((cmdlist (remember-win:parse cmd))
             (unassigned-cmdlines-current-filtered
               (remove-if-not #'stringp unassigned-cmdlines-current))
                                        ;((cmdlist (cl-ppcre:split "(\w*): ?("?[\w\s\.]*"?)\s|(\w*): ?("?[\w\s\.]*"?)|("[\w\s]*")|([\w]+)" cmd))
             (cmd-window-list (get-windows-from-cli cmd))
             (current-group (current-group))
             cli-pid)

        (cond
          (cmd-window-list
           (progn
             (message "~{~%~a~}" (mapcar #'internal-cli-of-window cmd-window-list))        ;; correct it.
             (mapcar (lambda (w)
                       (stumpwm::move-window-to-group w current-group))
                     ;;(tag-window tag w)
                     ;; remember window in group
                     cmd-window-list)
             (unless background
               (stumpwm::move-window-to-head (current-group) (first cmd-window-list))
               (stumpwm::really-raise-window (first cmd-window-list)))))
          ((find cmd
                 unassigned-cmdlines-current-filtered
                 :test #'string-ignore-space=)
           (if (stumpwm::y-or-n-p
                (format nil "I am also waiting for~%~% ~a ~%~%Do you want to rerun it: " cmd))
               (setf cli-pid
                     (stumpwm::run-prog (first cmdlist)
                                        :args (rest cmdlist)
                                        :wait wait
                                        :output output
                                        :search (getenv "PATH")))))
          (t (setf cli-pid
                   (stumpwm::run-prog (first cmdlist)
                                      :args (rest cmdlist)
                                      :wait wait
                                      :output output
                                      :search (getenv "PATH")))
             (push cmd unassigned-cmdlines-current)
             (push cmd cmdlines)))

        (if (or cli-pid cmd-window-list)
            (run-hook-with-args *run-cli-program-hook* cmd))
        cli-pid))

    (defvar *not-choose-on-single-unassigned-cli* nil "Not choose on single unassigned cli")

    ;; Question left
    ;; When simulteniously more than one clis present in the
    ;; unassigned-cmdlines-current than it, do not able to find foundcli, find
    ;; the solution for it.

    (defun find-cli-huristic (window
                              &key
                                (autoselect-if-only nil)
                                (choice-autoselect 1)
                                (not-interactive t)
                                (choice-time-out-seconds 0))
      (let*
          ((oldcliprop (xproperty 'cli window))
           (win-pid (pid-of-win window))  ;remember donot go for internal-cli-of-window
           (pid-cli (cli-of-pid win-pid))
           (unassigned-cmdlines-current-filtered (remove-if-not #'stringp unassigned-cmdlines-current))
           (windows-list (all-windows))
           (clis-of-other-windows-of-win-pid
             ;; (remove-duplicates
             (compact
              (mapcar #'(lambda (win)
                          (let ((wcli (xproperty 'cli win)))
                            (if wcli (list (concat (stumpwm::window-name win) ": " wcli) wcli))))
                      (remove window (get-windows-from-pid win-pid windows-list))))))
        ;; :test #'(lambda (l m)
        ;;           (string-ignore-space= (cadr l) (cadr m))))

        (or
         (when (and
                (null oldcliprop)
                not-interactive
                pid-cli)
           (find pid-cli  ;yes found pid-cli in win and list same
                 unassigned-cmdlines-current-filtered
                 :test #'string-ignore-space=))
         (choose-or-provide
          (append (if oldcliprop
                      (list oldcliprop)
                      (if (and
                           (null not-interactive)
                           pid-cli)
                          (list pid-cli)))
                  unassigned-cmdlines-current
                  clis-of-other-windows-of-win-pid)
          :dialog                  "Which command created this window ?: "
          :autoselect-if-only      autoselect-if-only
          :choice-autoselect       choice-autoselect
          :choice-time-out-seconds choice-time-out-seconds))))

    (stumpwm::defcommand cli-of-window (&optional (window (current-window))) ()
      "Show cli of window"
      (let* ((window-cli
               (or (xproperty 'cli window) ;it must have priority
                   (cli-of-pid (pid-of-win window)))))
        (if stumpwm::%interactivep%
            (setf
             (cli-of-window window)
             ;; (find-cli-huristic window :choice-autoselect nil :not-interactive nil :choice-time-out-seconds 0) ;; forevert
             ;; after 4 seconds
             (find-cli-huristic window
                                :autoselect-if-only (and
                                                     nil
                                                     (if (> (length (get-unresolved-cli-windows)) 1)
                                                         nil
                                                         t))
                                :choice-autoselect 1
                                :not-interactive nil
                                :choice-time-out-seconds 30)))
        window-cli))

    ;; TODO:
    ;; Add support for stumpwm::%interactivep%
    ;; Add support for transposing other window or windows' group's cli with current window
    (defun (setf cli-of-window) (foundcli &optional (window (current-window)))          ;work on it.
      "It is new-window-hook Set cli property of window."
      (remove-new-window window)
      (if foundcli                      ;what if foundcli is nil
          (progn
            ;; (message (format nil "~% cli searched ~a" foundcli))
            (when (setf (internal-cli-of-window window) foundcli)
              (prog1
                  foundcli
                (setf *gtd-debug-mesg* (format nil "~% cli searched ~a" foundcli))
                (message (format nil "~% + ~a" foundcli)))))
          (progn ;(read-one-line (current-screen) "Not able to find cli for you: " :initial-input  "")
            (message "Not able to find any cli for you, removed window.")
            nil)))


    (stumpwm::defcommand rerun/unassigned-wcli (cmd &optional
                                                    (output nil)
                                                    (wait nil))
      ((:cmdline "rerun cmd: "))
      (let* ((cmdlist (remember-win:parse cmd))
             cli-pid)
        (setf cli-pid
              (stumpwm::run-prog (first cmdlist)
                                 :args (rest cmdlist)
                                 :wait wait
                                 :output output
                                 :search (getenv "PATH")))
        (if (or cli-pid window-list)
            (run-hook-with-args *run-cli-program-hook* cmd))
        cli-pid))

    (stumpwm::defcommand show/unassigned-cmdlines () ()
      "show/unassigned-cmdlines"
      (if unassigned-cmdlines-current
          (message "~{~a~%~}" unassigned-cmdlines-current)
          (message "No unassigned cmdlines")))

    (defun get-unassigned-cmdlines ()
      unassigned-cmdlines-current)

    (stumpwm::defcommand clear/unassigned-cmdlines () ()
      "clear/unresolved-cli-windows-for-focus"
      (setf unassigned-cmdlines-current nil))

    (stumpwm::defcommand clear/unassigned-wcli (cmd) ((:cmdline "rerun cmd: "))
      (when unassigned-cmdlines-current
        (let* ((cli-string (if cmd (string-trim '(#\Space #\Tab #\Newline) cmd))))
          (remove cli-string ;not working find reason.
                  unassigned-cmdlines-current
                  :test #'string-ignore-space=))))


    (stumpwm::defcommand test/choose-or-provide-on-unassigned-cmdlines-current () ()
      "test/choose-or-provide-on-unassigned-cmdlines-current"
      (choose-or-provide unassigned-cmdlines-current :dialog "Test: "))))

;; (stumpwm::defcommand reassign-cli (window) ((:shell "program: "))
;;   "dddd"
;;   (let* ((opt (choose-or-provide '(both unassigned-cmdlines-current wincmd)
;;                                  :dialog "sel: "
;;                                  :extra-choices nil))
;;          (cmdlist ())))
;;   (if unassigned-cmdlines-current
;;       (choose-or-provide :dialog "2. Which command created this window ?: ")
;;       ))


(stumpwm::defcommand disappear-window (&optional (win (current-window))) ()
  "disappear window"
  (progn

    (let ((g (stumpwm::find-group (current-screen) ".hold")))
      (unless g
        (stumpwm::add-group (stumpwm::current-screen) ".hold" :background t)))

    (let ((g (stumpwm::find-group (current-screen) ".hold")))
      (when g
        (stumpwm::move-window-to-group win g)))))


(let ((stop-signal     19)
      (continue-signal 18))
  (defmethod signal-window ((signal fixnum) (win window))
    (let ((pid (remember-win::pid-of-win win)))
      (if pid
          (let ((cmd (format nil "kill -~d ~d" signal pid)))
            (message "running ~a" cmd)
            (run-cli-command cmd))
          (message "Now able to find pid for win ~a" win))))

  (stumpwm::defcommand signal-win (signal &optional (win (current-window))) ((:number "signal number: "))
    "signal window"
    (if win
        (signal-window signal win)
        (message "no win selected.")))

  (stumpwm::defcommand pause-win (&optional (win (current-window))) ()
    "pause window"
    (if win
        (signal-window stop-signal win)
        (message "no win selected.")))

  (stumpwm::defcommand continue-win (&optional (win (current-window))) ()
    "continue window"
    (if win
        (signal-window continue-signal win)
        (message "no win selected.")))

  (defun fmt-window-signal (window)
    (let ((pid (remember-win::pid-of-win window)))
      (when pid
        (let* ((statfilename (concatenate 'string "/proc/" (write-to-string pid) "/wchan"))
               (wchan (with-open-file (wchanfile statfilename :if-does-not-exist nil)
                        (if wchanfile
                            (read-line wchanfile))))
               (wchanstr (if (and wchan
                                  (stringp wchan)
                                  (not (string-equal "" wchan)))
                             (substitute #\Space #\Null (string-trim '(#\Null) wchan)))))
                                        ; (testr cmdline)  ; yes for some pid return vaule is nil
          (if (string-equal wchanstr "do_signal_stop")
              "^71<STOPPED>^n"
              "")))))

  ;; (push '(#\S fmt-window-signal) stumpwm::*window-formatters*)
  (nconc stumpwm::*window-formatters* '((#\S fmt-window-signal)))

  (setf *window-format*
        (concatenate 'string *window-format* "%S")))




(defun pull-group-tag (current last)
  "Recall all windows in group."
  (let ((tag (remove #\Space (stumpwm::group-name current))))
    (pull-tag tag)))

;; (add-hook *focus-group-hook* #'pull-group-tag)

;; (add-hook *focus-window-hook* #'(lambda (win1 win2)
;;                                   (tag-window (remove #\Space (stumpwm::group-name (current-group))) win1)))

(stumpwm::defcommand gtd-gmove-with-window () ()
  "gt gmove with window"
  (gmove)
  (tag-window
   (remove #\Space (stumpwm::group-name (current-group)))
   (stumpwm::current-window)))

(stumpwm::defcommand get-match () ()
  "get match"
  (do* ((c #\a (stumpwm::read-one-char (current-screen)))
        (ar "" (concatenate 'string ar (list c))))
       ((char= c #\Return) ar)
    (stumpwm::message-no-timeout "~a" (format nil "~a~^~a~^~a" ar ar ar))))

(stumpwm::defcommand remove-window () ()
  "remove window"
  (stumpwm::clear-tags (space-stripped (stumpwm::group-name (current-group))))
  (stumpwm::push-window))
