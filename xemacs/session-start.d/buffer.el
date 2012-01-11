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


(deh-require-maybe 'follow-mode
  ;; from http://www.dotemacs.de/dotfiles/DavidJolley.emacs.html
  (autoload 'follow-mode) ;; "follow.el" nil t)
  (autoload 'follow-delete-other-windows-and-split) ;; "follow.el" nil t)

;;
;; Some global keys so that I can activate Follow Mode fast.
;;
  (add-hook 'follow-mode-hook 'my-follow-mode-hook))



(deh-require-maybe 'ibuffer

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



  (deh-require-maybe 'ibuf-ext
    (defun sharad/ibuffer-included-in-group-p (buf group &optional nodefault)
      (let* ((filter-group-alist (if nodefault
                                     ibuffer-filter-groups
                                     (append ibuffer-filter-groups
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
                                    ibuffer-filter-groups
                                    (append ibuffer-filter-groups
                                            (list (cons "Default" nil))))))
        (while (and (not ret) filter-group-alist)
          (setq ret (if (sharad/ibuffer-included-in-group-p buf (caar filter-group-alist))
                        (caar filter-group-alist))
                filter-group-alist (cdr filter-group-alist)))
        ret))

    (defun sharad/ibuffer-get-group-buffers (group)
      (let* ((filter-group-alist (append ibuffer-filter-groups
                                         (list (cons "Default" nil))))
             (group-with-filterset (assoc group filter-group-alist))
             (filterset (cdr group-with-filterset))
             (buffers (buffer-list)))
        (if (null group-with-filterset)
            (error "no such group: %s" group)
            (remove-if-not #'(lambda (buf)
                               (ibuffer-included-in-filters-p buf filterset)) buffers))))

    (testing
     (sharad/ibuffer-included-in-groups-p (current-buffer) "gnus" "Default")
     (sharad/ibuffer-containing-group-of-buffer (current-buffer) t)
     (sharad/ibuffer-get-group-buffers "Default")
     (sharad/ibuffer-included-in-group-p (current-buffer) "Default"))


    (defun sharad/context-switch-buffer ()
      (interactive)
      (let ((group (sharad/ibuffer-containing-group-of-buffer (current-buffer) t)))
        (switch-to-buffer
         (ido-completing-read
          (format "Buffer from %s group: " group)
          (mapcar #'buffer-name (sharad/ibuffer-get-group-buffers group)))))) )





;;{{ Good :: Excellent beautiful Great!! Thanks XSteve
;; Use the keybinding M-F7 to toggle between the gnus window configuration and your normal editing windows.
(defun xsteve-gnus ()
  (interactive)
  (let ((bufname (buffer-name)))
    (if (or
         (string-equal "*Group*" bufname)
         (string-equal "*BBDB*" bufname)
         (string-match "\*Summary" bufname)
         (string-match "\*Article" bufname))
        (progn
          (xsteve-bury-gnus))
                                        ;unbury
        (if (get-buffer "*Group*")
            (unless (xsteve-unbury-gnus)
              (gnus-plugged))
              ; (gnus-unplugged))
            ;; (progn
            ;;   (xsteve-unbury-gnus)
            ;;   (if (functionp 'gnus-summary-rescan-group)
            ;;       (gnus-summary-rescan-group)))
            (gnus-plugged)))))
            ;;(gnus-unplugged)))))

(defun xsteve-unbury-gnus ()
  (interactive)
  (when (and (boundp 'gnus-bury-window-configuration) gnus-bury-window-configuration)
    (set-window-configuration gnus-bury-window-configuration)))

(defun xsteve-bury-gnus ()
  (interactive)
  (setq gnus-bury-window-configuration nil)
  (let ((buf nil)
        (bufname nil))
    (dolist (buf (buffer-list))
      (setq bufname (buffer-name buf))
      (when (or
             (string-equal "*Group*" bufname)
             (string-equal "*BBDB*" bufname)
             (string-match "\*Summary" bufname)
             (string-match "\*Article" bufname))
        (unless gnus-bury-window-configuration
          (setq gnus-bury-window-configuration (current-window-configuration)))
        (delete-other-windows)
        (if (eq (current-buffer) buf)
            (bury-buffer)
          (bury-buffer buf))))))

;;}}




  )


(deh-require-maybe 'uniquify
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

(user-provide 'buffer)







