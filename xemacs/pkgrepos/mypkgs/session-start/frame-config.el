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

  ;; (defmacro frame-launcher (args &optional fun)
  ;;   `(let ((f (make-frame)))
  ;;      (select-frame f)
  ;;      (dolist (a ,args)
  ;;        (when (or (= 0 (elscreen-get-current-screen))
  ;;                  (elscreen-create))
  ;;          (condition-case e
  ;;              (if fun
  ;;                  (funcall ,fun a)
  ;;                  (funcall a))
  ;;            ('quit (message "Not able to start %s" a)))))))

  (defun frame-launcher (name args &optional fun)
    (unless (progn
              (ignore-errors
              (select-frame-by-name name))
              (equal (get-frame-name) name))
      (let ((f (make-frame (list (cons 'name  name))))
            (screennum 0)
            (first-screen t))
        (select-frame f)
        (dolist (a args)
          (when (or first-screen
                    (setq screennum (elscreen-create)))
            (setq first-screen nil)
            (condition-case e
                (progn
                  (if fun
                      (funcall fun a)
                      (funcall a))
                  (launcher-set-elscreen-altname (format "%s" a) f
                                                 ;; screennum
                                                 ))
              ('quit (message "Not able to start %s" a))))))))




  ;; (set-frame-parameter (selected-frame) 'altscreen '())

  ;; (setq x '((altscreen (0) (1))))

  ;; (get-alist
  ;;  'altscreen
  ;;  (frame-parameters (selected-frame) ))

  ;; (let ((y (get-alist 'altscreen x)))
  ;;   (set-alist 'y 1 'z))

  ;; (frame-parameters (selected-frame) )

  ;; (frame-parameter (selected-frame) 'altscreen)

  (defun launcher-set-elscreen-altname (name &optional frame screennum)
    (interactive "sname:")
    (let* ((frame (or frame (selected-frame)))
           (screennum (or screennum 0))
           (place (get-alist 'altscreen (frame-parameters frame))))
      (unless (frame-parameter frame 'altscreen)
        (set-frame-parameter frame 'altscreen nil))
      (set-frame-parameter frame 'altscreen
                           (put-alist screennum
                                      name
                                      place))))

  (defun launcher-get-elscreen-altname (&optional frame screennum)
    (interactive)
    (let* ((frame (or frame (selected-frame)))
           (screennum (or screennum 0))
           (altscreen (frame-parameter frame 'altscreen)))
      (if altscreen
          (if (called-interactively-p)
              (message "altcreen: %s" (get-alist screennum altscreen))
              (get-alist screennum altscreen)))))



  ;; (launcher-set-elscreen-altname "test" (selected-frame))
  ;; (launcher-get-elscreen-altname)






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
