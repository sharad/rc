
;;
;; buffer.el
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


(deh-require-maybe follow-mode
  ;; from http://www.dotemacs.de/dotfiles/DavidJolley.emacs.html
  (autoload 'follow-mode) ;; "follow.el" nil t)
  (autoload 'follow-delete-other-windows-and-split) ;; "follow.el" nil t)

;;
;; Some global keys so that I can activate Follow Mode fast.
;;
  (add-hook 'follow-mode-hook 'my-follow-mode-hook))


(defmacro set-assoc (key val alist)
  `(progn
     (when (null (assoc ,key ,alist))
       (setq ,alist (acons ,key nil ,alist)))
     (setcdr (assoc ,key ,alist) ,val)))

(deh-require-maybe ibuffer

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
  ;; Since filename can work for any part of the path, if you filter on a partial (or complete) directory, anything you have open from the directory is now grouped:
  ;;
  ;;     ("journal" (filename . "/personal/journal/"))


  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dired" (mode . dired-mode))
           ("perl" (mode . cperl-mode))
           ("erc" (mode . erc-mode))
           ("planner" (or
                       (name . "^\\*Calendar\\*$")
                       (name . "^diary$")
                       (mode . muse-mode)))
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

           ("programming" (or
                           (mode . emacs-lisp-mode)
                           (mode . cperl-mode)
                           (mode . c-mode)
                           (mode . java-mode)
                           (mode . idl-mode)
                           (mode . lisp-mode))))))



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



  (deh-require-maybe ibuf-ext

    (defun sharad/get-ibuffer-filter-groups ()
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
                            (mapcar #'car (sharad/get-ibuffer-filter-groups)))
                           nil
                           nil
                           nil
                           nil
                           (or (if (stringp default-group) default-group)
                               (sharad/ibuffer-containing-group-of-buffer (current-buffer)))))



    (defun sharad/ibuffer-included-in-group-p (buf group &optional nodefault)
      (let* ((filter-group-alist (if nodefault
                                     (sharad/get-ibuffer-filter-groups)
                                     (append (sharad/get-ibuffer-filter-groups)
                                             (list (cons "Default" nil)))))
             (group-with-filterset (assoc group filter-group-alist))
             (filterset (cdr group-with-filterset)))
        (if (null group-with-filterset)
            (error "no such group: %s" group)
            (ibuffer-included-in-filters-p buf filterset))))

    (defun sharad/ibuffer-included-in-groups-p (buf &rest groups)
      (let (ret)
        (while (and (not ret) groups)
          (setq ret (sharad/ibuffer-included-in-group-p buf (car groups))
                groups (cdr groups)))
        ret))

    (defun sharad/ibuffer-containing-group-of-buffer (buf &optional default)
      (let (ret
            (filter-group-alist (if (not default)
                                    (sharad/get-ibuffer-filter-groups)
                                    (append (sharad/get-ibuffer-filter-groups)
                                            (list (cons "Default" nil))))))
        (while (and (not ret) filter-group-alist)
          (setq ret (if (sharad/ibuffer-included-in-group-p buf (caar filter-group-alist))
                        (caar filter-group-alist))
                filter-group-alist (cdr filter-group-alist)))
        ret))

    (defun sharad/ibuffer-get-group-buffers (group &optional current-last)
      (let* ((filter-group-alist (append (sharad/get-ibuffer-filter-groups)
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
     (sharad/ibuffer-included-in-groups-p (current-buffer) "gnus" "Default")
     (sharad/ibuffer-containing-group-of-buffer (current-buffer) t)
     (sharad/ibuffer-get-group-buffers "gnus")
     (sharad/ibuffer-included-in-group-p (current-buffer) "Default"))


    (defun sharad/context-switch-buffer (&optional arg)
      (interactive "P")
      (let ((group (sharad/ibuffer-containing-group-of-buffer (current-buffer) t)))
        (switch-to-buffer
         (ido-completing-read
          (format "Buffer from %s group: " group)
          (mapcar #'buffer-name (sharad/ibuffer-get-group-buffers group t))))))



  (defvar group-window-configuration-alist nil "group and window-configuration alist")

  (defvar group-start-fun-alist nil "group start fun alist")

  (setq
   group-start-fun-alist
   '(("gnus" . (gnus-unplugged . gnus-group-exit))
     ("erc" . (sharad/erc-start-or-switch))
     ("planner" . (plan))))

  (defun sharad/ibuffer-bury-group (group &optional buflist)
    ;; Should use current buffer's group
    (interactive)
    (dolist (buf (or buflist (sharad/ibuffer-get-group-buffers group))
              (bury-buffer buf))))

  (defun sharad/hide-group (&optional group call-stop-up-cmd)
    ;; Should use current buffer's group
    (interactive "P")
    (when (or call-stop-up-cmd
            (if (called-interactively-p) group))
        (call-group-start-stop-alist-cmd group 'stop)
        ;; correct it
        (setq group-window-configuration-alist
              (remove-if #'(lambda (gc)
                             (string-equal group (car gc)))
                         group-window-configuration-alist)))
    (let* ((call-stop-up-cmd
            (or call-stop-up-cmd
                (if (called-interactively-p) group)))
           (group (or
                   (unless (called-interactively-p) group)
                   (get-ibuffer-group nil (if call-stop-up-cmd 'stop))))
           (buflist (sharad/ibuffer-get-group-buffers group)))
      (when buflist
        (when (equal group (sharad/ibuffer-containing-group-of-buffer (current-buffer)))
          (set-assoc group (elscreen-current-window-configuration) group-window-configuration-alist))
        (sharad/ibuffer-bury-group group buflist)
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

  (defun sharad/ibuffer-unbury-group (group &optional buflist)
    ;; should ask for group.
    (interactive))
    ;; (dolist (buf (or buflist (sharad/ibuffer-get-group-buffers group))
    ;;          (unbury-buffer buf))))

  (defun sharad/unhide-group (&optional group call-start-up-cmd)
    ;; should ask for group.
    (interactive "P")
    (let* ((call-start-up-cmd
            (or call-start-up-cmd
                (if (called-interactively-p) group)))
           (group (or
                   (unless (called-interactively-p) group)
                   (get-ibuffer-group nil (if call-start-up-cmd 'start))))
           (buflist (sharad/ibuffer-get-group-buffers group)))
      (if buflist
          (progn
            (sharad/ibuffer-unbury-group group buflist)
            (switch-to-buffer (car buflist))
            (if (assoc group group-window-configuration-alist)
                ;;                 (set-window-configuration (cdr (assoc group group-window-configuration-alist)))
                (elscreen-apply-window-configuration (cdr (assoc group group-window-configuration-alist)))
                )
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
               (if (called-interactively-p) group)))
           (group (or
                   (unless (called-interactively-p) group)
                   (get-ibuffer-group nil (if force-call-cmd 'any)))))
      (if (sharad/ibuffer-included-in-group-p (current-buffer) group)
          (sharad/hide-group group force-call-cmd)
          (sharad/unhide-group group force-call-cmd)))))


;;}}

  )


(deh-require-maybe uniquify
  ;; from http://jasonmbaker.com/7-tools-for-working-with-python-in-emacs-and
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t ; rename after killing uniquified
        ; don't muck with special buffers (or Gnus mail buffers
        uniquify-ignore-buffers-re "^\\*"))

(deh-section "Buffer"
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




;; (setq debug-on-error t)

(provide 'buffer-config)







