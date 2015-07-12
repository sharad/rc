;;; frame-config.el --- Anything Config

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


(deh-require-maybe elscreen

  ;; toggle-ibuffer-group
  (require 'buffer-config)

  (defun frame-launcher (name args &optional fun)
    (unless (progn
              (ignore-errors
              (select-frame-by-name name))
              (equal (get-frame-name) name))
      (let ((f (make-frame (list (cons 'name name))))
            (screennum 0)
            (first-screen t))
        (select-frame f)
        (dolist (a args)
          (when (or first-screen
                    (setq screennum (elscreen-create)))
            (setq first-screen nil)
            (condition-case e
                (progn
                  (if (if fun
                          (funcall fun a)
                          (funcall a))
                      (sticky-buffer-mode t))
                  (launcher-set-elscreen-altname (format "%s" a) f screennum))
              (quit  (message "Not able to start %s error %" a e))
              (error (message "Not able to start %s error %" a e))))))))

  ;; (frame-parameter (selected-frame) 'altscreen)

  (defun launcher-set-elscreen-altname (name &optional frame screennum)
    (interactive "sname:")
    (let* ((frame (or frame (selected-frame)))
           (screennum (or screennum (elscreen-get-current-screen)))
           (place (get-alist 'altscreen (frame-parameters frame))))
      (unless (frame-parameter frame 'altscreen)
        (set-frame-parameter frame 'altscreen nil))
      (set-frame-parameter frame 'altscreen
                           (put-alist screennum
                                      name
                                      place))))

  (defun launcher-del-elscreen-altname (&optional frame screennum)
    (interactive "sname: ")
    (let* ((frame (or frame (selected-frame)))
           (screennum (or screennum (elscreen-get-current-screen)))
           (place (get-alist 'altscreen (frame-parameters frame))))
      (unless (frame-parameter frame 'altscreen)
        (set-frame-parameter frame 'altscreen nil))
      (set-frame-parameter frame 'altscreen
                           (del-alist screennum place))))

  (defun launcher-get-elscreen-altname (&optional frame screennum)
    (interactive)
    (let* ((frame (or frame (selected-frame)))
           (screennum (or screennum (elscreen-get-current-screen)))
           (altscreen (frame-parameter frame 'altscreen)))
      (if altscreen
          (if (called-interactively-p 'any)
              (message "altcreen: %s" (get-alist screennum altscreen))
              (get-alist screennum altscreen)))))

  ;; (add-hook 'elscreen-kill-hook #'launcher-del-elscreen-altname)

  ;; advise (elscreen-kill-internal screen)

  (defadvice elscreen-kill-internal (after lanucher-del-altname (screen) activate)
    ;; (message "elscreen-kill-internal: in advise %d" screen)
    (launcher-del-elscreen-altname (selected-frame) screen))

  ;; (launcher-set-elscreen-altname "test" (selected-frame))
  ;; (launcher-get-elscreen-altname)
  ;; (launcher-del-elscreen-altname)
)


(deh-require-maybe 'elscreen

  (defun make-mail-chat-frame (&optional force)
    (interactive "P")
    (frame-launcher "mail-chat"
                    '("gnus" "erc")
                    (if force
                        #'(lambda (group)
                            (toggle-ibuffer-group group t))
                        #'toggle-ibuffer-group)))


  (defun make-mail-compose-frame ()
    ))


(provide 'frame-config)
;;; frame-config.el ends here
