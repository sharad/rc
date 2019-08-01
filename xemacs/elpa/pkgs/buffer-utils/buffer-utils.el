;;
;; buffer-utils.el
;; Login : <s@taj>
;; Started on  Tue Jul 27 00:19:15 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;


;; Follow mode.
;; Dead useful for wider-than-tall-hires monitors.  Will let you do a
;; C-x 3, then use both windows as a single tall window, just broken in
;; half and displayed side by side.  Neat-o.
;; get it from: http://www.andersl.com/emacs/follow.html

(provide 'buffer-utils)


;; (require 'cl)

(eval-when-compile
  '(require 'cl))
(require 'general-testing)

(require 'ibuffer)
(require 'uniquify)
;; (require 'planner)
;; believe it need not be here
;; (eval-after-load "ido"
;;   '(lotus-disable-startup-interrupting-feature))
(require 'buffer-move nil t)


;; Finally, to flip sequentially through buffers (like Alt-Tab in a
;; window manager) I use iflipb:

(progn ;; deh-require-maybe iflipb
  (setq iflipb-boring-buffer-filter 'my-bs-ignore-buffer))

(progn ;; deh-require-maybe ido
  ;; http://scottfrazersblog.blogspot.in/2010/01/emacs-filtered-buffer-switching.html

  (defvar my-bs-always-show-regexps '("\\*\\(scratch\\|info\\|grep\\|compilation\\)\\*")
    "*Buffer regexps to always show when buffer switching.")
  (defvar my-bs-never-show-regexps '("^\\s-" "^\\*" "TAGS$")
    "*Buffer regexps to never show when buffer switching.")
  (defvar my-ido-ignore-dired-buffers t
    "*If non-nil, buffer switching should ignore dired buffers.")

  (defun my-bs-str-in-regexp-list (str regexp-list)
    "Return non-nil if str matches anything in regexp-list."
    (let ((case-fold-search nil))
      (catch 'done
        (dolist (regexp regexp-list)
          (when (string-match regexp str)
            (throw 'done t))))))

  (defun my-bs-ignore-buffer (name)
    "Return non-nil if the named buffer should be ignored."
    (or (and (not (my-bs-str-in-regexp-list name my-bs-always-show-regexps))
             (my-bs-str-in-regexp-list name my-bs-never-show-regexps))
        (and my-ido-ignore-dired-buffers
             (save-excursion
               (set-buffer name)
               (equal major-mode 'dired-mode)))))


  ;; This is set up to ignore all buffers that start with a space or
  ;; '*', except for scratch, info, grep, and compilation buffers.
  ;; Dired buffers are also ignored.

  ;; []The function to toggle between the two most recently used buffers
  ;; is easy enough:

  (defun my-bs-toggle ()
    "Toggle buffers, ignoring certain ones."
    (interactive)
    (catch 'done
      (dolist (buf (buffer-list))
        (unless (or (equal (current-buffer) buf)
                    (my-bs-ignore-buffer (buffer-name buf)))
          (switch-to-buffer buf)
          (throw 'done t)))))


  ;; I use ido to switch buffers by name:

  ;; (setq ido-ignore-buffers '(my-bs-ignore-buffer))


  ;; I like bs for getting a list of buffers to choose from:

  (setq bs-configurations
        '(("all" nil nil nil nil nil)
          ("files" nil nil nil (lambda (buf) (my-bs-ignore-buffer (buffer-name buf))) nil)))
  (setq bs-cycle-configuration-name "files")


  ;; This sets up two bs configurations, one that shows all the
  ;; buffers and one that only shows my subset.  Somewhat off-topic, I
  ;; like bs but not the default look ... too much information.
  ;; Here's my simplified version:

  (setq bs-mode-font-lock-keywords
        (list
         ; Headers
         (list "^[ ]+\\([-M].*\\)$" 1 font-lock-keyword-face)
         ; Boring buffers
         (list "^\\(.*\\*.*\\*.*\\)$" 1 font-lock-comment-face)
         ; Dired buffers
         '("^[ .*%]+\\(Dired.*\\)$" 1 font-lock-type-face)
         ; Modified buffers
         '("^[ .]+\\(\\*\\)" 1 font-lock-warning-face)
         ; Read-only buffers
         '("^[ .*]+\\(\\%\\)" 1 font-lock-variable-name-face)))

  (setq bs-attributes-list
        (quote (("" 2 2 left bs--get-marked-string)
                ("M" 1 1 left bs--get-modified-string)
                ("R" 2 2 left bs--get-readonly-string)
                ("" 2 2 left "  ")
                ("Mode" 16 16 left bs--get-mode-name)
                ("" 2 2 left "  ")
                ("Buffer" bs--get-name-length 30 left bs--get-name)))))


(progn ;; deh-section "Setting ignore extention for file completion"
  (setq completion-ignored-extensions
        (append  completion-ignored-extensions '(".rem"))))


(progn ;; deh-require-maybe follow-mode
  ;; from http://www.dotemacs.de/dotfiles/DavidJolley.emacs.html
  (autoload 'follow-mode "follow.el") ;; "follow.el" nil t)
  (autoload 'follow-delete-other-windows-and-split "follow.el") ;; "follow.el" nil t)

  ;;
  ;; Some global keys so that I can activate Follow Mode fast.
  ;;
  (add-hook 'follow-mode-hook 'my-follow-mode-hook))


(defmacro set-assoc (key val alist)
  `(progn
     (when (null (assoc ,key ,alist))
       (setq ,alist (acons ,key nil ,alist)))
     (setcdr (assoc ,key ,alist) ,val)))


(progn ;; deh-require-maybe ibuffer

  ;; https://github.com/purcell/ibuffer-vc/blob/master/ibuffer-vc.el
  (xrequire 'ibuffer-vc)

  (global-set-key (kbd "C-x C-b") 'ibuffer) ;force
  ;; (autoload 'ibuffer "ibuffer" "List buffers." t)



  ;; It looks as though the default filterings are as follows:
  ;;
  ;;     predicate
  ;;     content
  ;;     size-lt
  ;;     size-gt
  ;;     filename
  ;;     name
  ;;     used-mode
  ;;     mode
  ;;
  ;; Since filename can work for any part of the path, if you filter
  ;; on a partial (or complete) directory, anything you have open from
  ;; the directory is now grouped:
  ;;
  ;;     ("journal" (filename . "/personal/journal/"))


  (setq ibuffer-saved-filter-groups
        `(("default"
           ("scratches" (or
                         (name . "^\\*.*\-scratch\\*$")))
           ("Config"
            (or
             (name . "^config$")
             (name . "^conf$")
             (name . "^cfg$")
             (name . "^\\.cfg$")
             (name . "^\\.conf$")
             (name . "^\\.config$")
             ;; (name . "^\\rc$")
             ))
           ("Logs"
            (or
             (name . "^\\.log$")))
           ("dired" (mode . dired-mode))
           ("perl" (mode . cperl-mode))
           ("erc" (mode . erc-mode))
           ,@(when (featurep 'planner)
               `(("planner" (or
                             (name . "^\\*Calendar\\*$")
                             (name . "^diary$")
                             (name . "planner-cyclic-diary-file")
                             (name . "private")
                             (name . "public")
                             (name . ,(concat planner-date-regexp ".muse"))
                             (mode . planner-mode)))))
           ("wiki" (or
                    (name . "^\\.muse$")
                    (mode . muse-mode)))
           ("org" (or
                   (name . "^\\.org$")
                   (name . "^\\.org")
                   (name . "^\\.rem$")
                   (mode . org-mode)))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))
           ("gnus" (or
                    (mode . message-mode)
                    (mode . bbdb-mode)
                    (mode . mail-mode)
                    (mode . gnus-group-mode)
                    (mode . gnus-summary-mode)
                    (mode . gnus-article-mode)
                    (name . "^\\.bbdb$")
                    (name . "^\\.newsrc-dribble")))
           ("emacs-lisp" (or
                          (name . "^\\.el$")
                          (mode . emacs-lisp-mode)))
           ("C" (or
                 (name . "^\\.c$")
                 (name . "^\\.cpp$")
                 (name . "^\\.cxx$")
                 (name . "^\\.C$")
                 (name . "^\\.h$")
                 (mode . cpp-mode)
                 (mode . c++-mode)
                 (mode . c-mode)))
           ("Javascript" (or
                          (name . "^\\.js$")
                          (mode . js-mode)
                          (mode . espresso-mode)
                          (mode . js2-mode)))
           ("Lisp" (or
                    (name . "^\\.lisp$")
                    (mode . lisp-mode)
                    (mode . slime-mode)))
           ("XML" (or
                   (name . "^\\.xml$")
                   (name . "^\\.xql$")
                   (name . "^\\.xq$")
                   (name . "^\\.xsl$")
                   (mode . nxml-mode)
                   (mode . xml-mode)
                   (mode . xslt-mode)
                   (mode . xql-mode)
                   (mode . xquery-mode)))
           ("Shell" (or
                     (name . "^\\.sh$")
                     (name . "^\\.zsh$")
                     (mode . sh-mode)
                     (mode . shell-script-mode)))
           ("programming" (or
                           (mode . emacs-lisp-mode)
                           (mode . cperl-mode)
                           (mode . c-mode)
                           (mode . java-mode)
                           (mode . idl-mode)
                           (mode . lisp-mode)))
           ("other" (or
                     (name . "^\\*.+\\*$")))
           ("debug" (or
                     (name . " .+debug.+$"))))))



  ;; nomaclature for `ibuffer-saved-filter-groups'

  '(("groups1"
     ("group1.1" filterset = (or filters))
     ("group1.2" filterset = (filter)))

    ("groups2"
     ("group2.1" filterset = (or filters))
     ("group2.2" filterset = (filter))
     ))


  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))


  ;; http://www.emacswiki.org/emacs/IbufferMode
  ;;When used with ElScreen, Ibuffer tends to remove it’s header line with tabs.
  ;;To prevent it, set ibuffer-use-header-line to nil and use the following:

  (defadvice ibuffer-update (around ibuffer-preserve-prev-header activate)
    "Preserve line-header used before Ibuffer if it doesn't set one"
    (let ((prev-line-header header-line-format))
      ad-do-it
      (unless header-line-format
        (setq header-line-format prev-line-header))))

  ;;IvanKorotkov



  (progn ;; deh-require-maybe ibuf-ext

    (require 'ibuf-ext)

    (defun lotus-get-ibuffer-filter-groups ()
      (cdr (assoc "default" ibuffer-saved-filter-groups)))


    (defun get-ibuffer-group (&optional default-group cmd)
      (ido-completing-read "iBuffer Group: "
                           (remove-if-not
                            '(lambda (g)
                               (funcall (cond
                                         ((eq cmd 'start) #'car)
                                         ((eq cmd 'stop) #'cdr)
                                         ((eq cmd nil) #'(lambda (x) t))
                                         (t #'identity))
                                        (cdr (assoc g group-start-fun-alist))))
                            (mapcar #'car (lotus-get-ibuffer-filter-groups)))
                           nil
                           nil
                           nil
                           nil
                           (or (if (stringp default-group) default-group)
                               (lotus-ibuffer-containing-group-of-buffer (current-buffer)))))



    (defun lotus-ibuffer-included-in-group-p (buf group &optional nodefault)
      (let* ((filter-group-alist (if nodefault
                                     (lotus-get-ibuffer-filter-groups)
                                   (append (lotus-get-ibuffer-filter-groups)
                                           (list (cons "Default" nil)))))
             (group-with-filterset (assoc group filter-group-alist))
             (filterset (cdr group-with-filterset)))
        (if (null group-with-filterset)
            (error "no such group: %s" group)
          (ibuffer-included-in-filters-p buf filterset))))

    (defun lotus-ibuffer-included-in-groups-p (buf &rest groups)
      (let (ret)
        (while (and (not ret) groups)
          (setq ret (lotus-ibuffer-included-in-group-p buf (car groups))
                groups (cdr groups)))
        ret))

    (defun lotus-ibuffer-containing-group-of-buffer (buf &optional default)
      (let (ret
            (filter-group-alist (if (not default)
                                    (lotus-get-ibuffer-filter-groups)
                                  (append (lotus-get-ibuffer-filter-groups)
                                          (list (cons "Default" nil))))))
        (while (and (not ret) filter-group-alist)
          (setq ret (if (lotus-ibuffer-included-in-group-p buf (caar filter-group-alist))
                        (caar filter-group-alist))
                filter-group-alist (cdr filter-group-alist)))
        ret))

    (defun lotus-ibuffer-get-group-buffers (group &optional current-last)
      (let* ((filter-group-alist (append (lotus-get-ibuffer-filter-groups)
                                         (list (cons "Default" nil))))
             (group-with-filterset (assoc group filter-group-alist))
             (filterset (cdr group-with-filterset))
             (buffers
              (if current-last
                  (reverse (buffer-list))
                (buffer-list))))
        (if (null group-with-filterset)
            (error "no such group: %s" group)
          (remove-if-not #'(lambda (buf)
                             (ibuffer-included-in-filters-p buf filterset)) buffers))))

    (testing
     (lotus-ibuffer-included-in-groups-p (current-buffer) "gnus" "Default")
     (lotus-ibuffer-containing-group-of-buffer (current-buffer) t)
     (lotus-ibuffer-get-group-buffers "gnus")
     (lotus-ibuffer-included-in-group-p (current-buffer) "Default"))


    (defun lotus-context-switch-buffer (&optional arg)
      (interactive "P")
      (let ((group (lotus-ibuffer-containing-group-of-buffer (current-buffer) t)))
        (switch-to-buffer
         (ido-completing-read
          (format "Buffer from %s group: " group)
          (mapcar #'buffer-name (lotus-ibuffer-get-group-buffers group t))))))



    (defvar group-window-configuration-alist nil "group and window-configuration alist")

    (defvar group-start-fun-alist nil "group start fun alist")

    (setq
     group-start-fun-alist
     '(("gnus"    . (gnus-unplugged . gnus-group-exit))
       ("erc"     . (lotus-erc-start-or-switch))
       ("planner" . (plan))))

    (defun lotus-ibuffer-bury-group (group &optional buflist)
      ;; Should use current buffer's group
      (interactive)
      (dolist (buf (or buflist (lotus-ibuffer-get-group-buffers group))
                   (bury-buffer buf))))

    (defun lotus-hide-group (&optional group call-stop-up-cmd)
      ;; Should use current buffer's group
      (interactive "P")
      (when (or call-stop-up-cmd
                (if (called-interactively-p 'any) group))
        (call-group-start-stop-alist-cmd group 'stop)
        ;; correct it
        (setq group-window-configuration-alist
              (remove-if #'(lambda (gc)
                             (string-equal group (car gc)))
                         group-window-configuration-alist)))
      (let* ((call-stop-up-cmd
              (or call-stop-up-cmd
                  (if (called-interactively-p 'any) group)))
             (group (or
                     (unless (called-interactively-p 'any) group)
                     (get-ibuffer-group nil (if call-stop-up-cmd 'stop))))
             (buflist (lotus-ibuffer-get-group-buffers group)))
        (when buflist
          (when (equal group (lotus-ibuffer-containing-group-of-buffer (current-buffer)))
            (set-assoc group (elscreen-current-window-configuration) group-window-configuration-alist))
          (lotus-ibuffer-bury-group group buflist)
          (delete-other-windows))))


    (defun call-group-start-stop-alist-cmd (group start-or-stop)
      (let ((fun (if (equal start-or-stop 'start)
                     (cadr (assoc group group-start-fun-alist))
                   (cddr (assoc group group-start-fun-alist))))
            (cmd-type (if (equal start-or-stop 'start)
                          "startup"
                        "stop")))
        (if fun
            (funcall fun)
          (message "No %s command associated with: `%s' group" cmd-type group))))

    (defun lotus-ibuffer-unbury-group (group &optional buflist)
      ;; should ask for group.
      (interactive))
    ;; (dolist (buf (or buflist (lotus-ibuffer-get-group-buffers group))
    ;;          (unbury-buffer buf))))

    (defun lotus-unhide-group (&optional group call-start-up-cmd)
      ;; should ask for group.
      (interactive "P")
      (let* ((call-start-up-cmd
              (or call-start-up-cmd
                  (if (called-interactively-p 'any) group)))
             (group (or
                     (unless (called-interactively-p 'any) group)
                     (get-ibuffer-group nil (if call-start-up-cmd 'start))))
             (buflist (lotus-ibuffer-get-group-buffers group)))
        (if buflist
            (progn
              (lotus-ibuffer-unbury-group group buflist)
              (switch-to-buffer (car buflist))
              (if (assoc group group-window-configuration-alist)
                  ;;                 (set-window-configuration (cdr (assoc group group-window-configuration-alist)))
                  (elscreen-apply-window-configuration (cdr (assoc group group-window-configuration-alist))))
              (if call-start-up-cmd
                  (call-group-start-stop-alist-cmd group 'start)))
          (call-group-start-stop-alist-cmd group 'start))))

    (testing
     (defvar xx nil "asfds")
     (set-assoc "qq" "pp" xx)
     (macroexpand '(set-assoc "qwewq" "pp" xx))
     (set-assoc "qwewq" "pp" xx)
     (progn (when (null (assoc "qq" xx)) (setq xx (acons "qq" nil xx))) (setcdr (assoc "qq" xx) "pp")))

    ;;{{ Good :: Excellent beautiful Great!! Thanks XSteve
    ;; Use the keybinding M-F7 to toggle between the gnus window configuration and your normal editing windows.

    (defun toggle-ibuffer-group (&optional group force-call-cmd)
      ;; Should use current buffer's group
      (interactive "P")
      (let* ((force-call-cmd
              (or force-call-cmd
                  (if (called-interactively-p 'any) group)))
             (group (or
                     (unless (called-interactively-p 'any) group)
                     (get-ibuffer-group nil (if force-call-cmd 'any)))))
        (if (lotus-ibuffer-included-in-group-p (current-buffer) group)
            (lotus-hide-group group force-call-cmd)
          (lotus-unhide-group group force-call-cmd)))))

  ;;}}

  (defvar lotus-context-ignore-buffer t "Allow to enable context-ignore-buffer")

  (defun toggle-context-ignore-buffer ()
    (interactive)
    (setq lotus-context-ignore-buffer (not lotus-context-ignore-buffer)))

  (defun lotus-context-ignore-buffer (name)
    (interactive "P")
    (let ((group (lotus-ibuffer-containing-group-of-buffer (current-buffer) t))
          (buff (get-buffer name)))
      (if (and lotus-context-ignore-buffer buff)
          (not (lotus-ibuffer-included-in-group-p buff group))
        t)))

  ;; (defun lotus-context-ignore-buffer (name)
  ;;   (interactive "P")
  ;;   (message "cib: %s" (type-of name))
  ;;   nil)

  ;; (type-of (current-buffer))

  (defun lotus-context-switch-other-buffer (buffer)
    (let ((group (lotus-ibuffer-containing-group-of-buffer buffer t)))
      (find-if
       (lambda (buff)
         (if (lotus-ibuffer-included-in-group-p buff group)
             (not (eq buff buffer))))
       (buffer-list))))

  (setq ido-ignore-buffers '(lotus-context-ignore-buffer
                             "\\` "))


  (defadvice other-buffer (around context-buffer (&optional buffer visible-ok frame) activate)
    (let* ((buffer (or buffer (current-buffer)))
           (group (lotus-ibuffer-containing-group-of-buffer buffer t))
           (obuff (get-buffer ad-do-it)))
      (setq
       ad-return-value
       (if (and lotus-context-ignore-buffer
                (lotus-ibuffer-included-in-group-p obuff group))
           obuff
         (let ((cbuff (lotus-context-switch-other-buffer buffer)))
           (or cbuff obuff))))))

  ;; (lotus-ibuffer-included-in-group-p (other-buffer (current-buffer)) "emacs-lisp")
  ;; (lotus-context-switch-other-buffer (current-buffer))
  ;; (other-buffer (current-buffer))
  )


(progn ;; deh-require-maybe uniquify
  ;; from https://github.com/purcell/emacs.d/blob/master/init-uniquify.el
  ;; from http://jasonmbaker.com/7-tools-for-working-with-python-in-emacs-and
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ; rename after killing uniquified
                                        ; don't muck with special buffers (or Gnus mail buffers
        uniquify-ignore-buffers-re "^\\*"))


(progn ;; deh-section "Buffer"
  (unless (fboundp 'next-buffer)
    (defun next-buffer ()
      "Switch to the next buffer in cyclic order."
      (interactive)
      (let ((buffer (current-buffer)))
        (switch-to-buffer (other-buffer buffer))
        (bury-buffer buffer))))
  (unless (fboundp 'prev-buffer)
    (defun prev-buffer ()
      "Switch to the previous buffer in cyclic order."
      (interactive)
      (let ((list (nreverse (buffer-list)))
            found)
        (while (and (not found) list)
          (let ((buffer (car list)))
            (if (and (not (get-buffer-window buffer))
                     (not (string-match "\\` " (buffer-name buffer))))
                (setq found buffer)))
          (setq list (cdr list)))
        (switch-to-buffer found)))))


;; (deh-require-maybe contentswitch)

(require 'contentswitch nil t)



(progn ;; deh-section "lock buffer with window"
  ;; http://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00978.html
  (define-minor-mode sticky-buffer-mode
      "Make the current window always display this buffer."
    nil " sticky" nil
    (set-window-dedicated-p (selected-window) sticky-buffer-mode))
  ;; then M-x sticky-buffer-mode.
  )
;; believe it need not be here
;; (lotus-disable-startup-interrupting-feature)

;; (setq debug-on-error t)


(progn ;; deh-section "schedule buffer killing"
  ;;;###autoload
  (defun ido-schedule-kill-buffer ()
    "Kill a buffer.
The buffer name is selected interactively by typing a substring.
For details of keybindings, see `ido-switch-buffer'."
    (interactive)
    (ido-buffer-internal 'kill 'schedule-kill-buffer "Kill buffer: " (buffer-name (current-buffer)) nil 'ignore))

  (defun schedule-kill-buffer ()
    ;; schedule killing buffer, if not accessed again in fixed time.
    ))

;;; buffer-utils.el ends here
