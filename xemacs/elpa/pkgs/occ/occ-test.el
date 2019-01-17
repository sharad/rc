;;; occ-test.el --- Occ Test                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
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



(require 'ert)
(require 'ert-x)

(ert-deftest ert-occ-test ()
  "Test"
  :expected-result :failed
  :tags '(sessions-unified)
  (should t))


(when nil                               ;occ-obj-method.el

  (occ-add-to-org-heading-when-idle (occ-make-ctx) 7)

  (length
   (occ-matching-ctxual-tsks
    (occ-collection-object)
    (occ-make-ctx
     (find-file-noselect "/home/s/paradise/git/main/src/wnc/security/authenticator/accounting.cpp"))))

  (occ-ctxual-tsk-tsk
   (car
    (occ-matching-ctxual-tsks
     (occ-collection-object)
     (occ-make-ctx
      (find-file-noselect "/home/s/paradise/git/main/src/wnc/security/authenticator/accounting.cpp")))))

  (length
   (occ-matching-ctxual-tsks
    (occ-collection-object)
    (occ-make-ctx (current-buffer)))))

(when nil                               ;occ-util-common.el
  (defun time-consuming ()
    (cl-loop for i below (* 1000 1000 1) sum i))

  (defun test-no-input ()
    (let ((retval))
      (message "last-input-event %s retval %s" last-input-event retval)))

  (progn
    (run-with-idle-timer 3 nil #'test-no-input)
    (run-with-idle-timer 6 nil #'test-no-input)
    (run-with-idle-timer 18 nil #'test-no-input)))




(when nil                             ;while-no-input occ-main.el
  (defun time-consuming ()
    (cl-loop for i below (* 1000 1000 1) sum i)

    (defun test-no-input ()
      (let ((retval
             (while-no-input
               (redisplay)
               (message "started")
               (prog1
                   (time-consuming)
                 (message "stopped")))))
        (message "last-input-event %s retval %s" last-input-event retval))

      (progn
        (run-with-idle-timer 3 nil #'test-no-input)
        (run-with-idle-timer 6 nil #'test-no-input)
        (run-with-idle-timer 18 nil #'test-no-input)))))



(when nil                               ;occ-interactive.el
  ;; testing verification
 (defun occ-files-with-null-regex ()
   (interactive)
   (let ((files
          (remove-if
           #'(lambda (f)
               (with-current-buffer (find-file-noselect f)
                 org-complex-heading-regexp))
           (occ-included-files))))
     (message "files with null regex %s" files)))

 ;; testing verification;; testing verification
 (defun occ-files-not-in-org-mode ()
   (interactive)
   (let ((files
          (remove-if
           #'(lambda (f)
               (with-current-buffer (find-file-noselect f)
                 (eq major-mode 'org-mode)))
           (occ-included-files))))
     (message "files not in org-mode %s" files))))





(provide 'occ-test)
;;; occ-test.el ends here
