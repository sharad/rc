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

(provide 'occ-test)


(require 'ert)
(require 'ert-x)
(require 'el-mock)




;; https://www.gnu.org/software/emacs/manual/html_node/ert/index.html
;;;###auoload
(ert-deftest ert-occ-test-occ-insinuated ()
  "Test"
  :expected-result :passed
  :tags '(occ)
  (should
   (member #'occ-switch-buffer-run-curr-ctx-timer-function
           switch-buffer-functions))
  (should
   (member #'occ-add-after-save-hook-fun-in-org-mode
           org-mode-hook)))



(ert-deftest ert-occ-test ()
  "Test"
  :expected-result :failed
  :tags '(occ)
  (should t))



;; testing verification
;; occ-files-with-null-regex
;; occ-files-not-in-org-mode


(when nil                               ;occ-obj-method.el

  (occ-add-to-org-heading-when-idle (occ-make-ctx-at-point) 7)

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
      (occ-debug :debug "last-input-event %s retval %s" last-input-event retval)))

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
               (occ-debug :debug "started")
               (prog1
                   (time-consuming)
                 (occ-debug :debug "stopped")))))
        (occ-debug :debug "last-input-event %s retval %s" last-input-event retval))

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
           (occ-files))))
     (occ-debug :debug "files with null regex %s" files)))

 ;; testing verification;; testing verification
 (defun occ-files-not-in-org-mode ()
   (interactive)
   (let ((files
          (remove-if
           #'(lambda (f)
               (with-current-buffer (find-file-noselect f)
                 (eq major-mode 'org-mode)))
           (occ-files))))
     (occ-debug :debug "files not in org-mode %s" files))))


(defun functions-in-file-test ()
  ;; https://stackoverflow.com/questions/26330363/how-do-i-get-a-list-of-functions-defined-in-an-emacs-lisp-file
  (let ((funclist ()))
    (mapatoms
     (lambda (x)
       (when (and (fboundp x)                     ; does x name a function?
                  (let ((f (symbol-file x)))
                    (and f (string= (file-name-base f) "occ-main.el"))))
         (push x funclist))))
    funclist))



(progn                                  ;method
  (when nil
    (occ-tree-collection-files (occ-collection-object))

    (occ-tree-collection-files (make-occ-tree-collection))))




(progn                                  ;from method

  (when nil

    (cl-defmethod occ-rank (tsk-pair ctx)
      0)

    (cl-defmethod occ-rank ((tsk-pair (head root)) (ctx list))
      (occ-debug :debug "%s" tsk-pair))

    (occ-rank '(root  1) nil)

    (occ-rank '(n  1) nil)

    (cl-defmethod occ-rank ((tsk occ-tsk)
                            (ctx occ-ctx))
      (occ-debug :debug "match occ-rank"))

    (occ-rank (make-occ-tree-tsk) (make-occ-ctx))))



(when nil
 (progn                                  ;; from obj common

  (when nil ;; https://curiousprogrammer.wordpress.com/2010/07/19/emacs-defstruct-vs-other-languages/

    (defun cl-get-field (object field)
      (cl-struct-slot-value (cl-classname object) field object))

    (defun cl-set-field (object field value)
      (setf (cl-struct-slot-value (cl-classname object) field object) value))

    (get-field dave 'name)
    (set-field dave 'name "Simon Smith"))

  (progn
    (when nil
      (occ-readprop-props)
      (cl-method-matched-arg 'occ-readprop nil)
      (cl-method-matched-arg 'occ-readprop (occ-make-ctx-at-point))
      (occ-obj-defined-slots-with-value (occ-make-ctx-at-point))))

  (progn
    (cl-method-sig-matched-arg '(occ-readprop (`((head ,val) occ-ctx) val)) nil)

    (cl-method-param-signs 'occ-ctx-property-get)
    (cl-method-sigs-matched-arg
     '(occ-readprop (`((head ,val) occ-ctx) val))
     '(occ-ctx-property-get (`((head ,val)) val))
     (occ-make-ctx-at-point)))

  ;; (cl-method-param-case '(occ-readprop (`((head ,val) occ-ctx) val)))
  (setq xxnaaa
        (mapcar
         #'(lambda (x) (aref x 1))
         (aref (cl--generic 'occ-readprop) 3)))

  (setq xxnaaa
        (aref (cl--generic 'occ-readprop) 3))))


;; ctor.el
(when nil
  (progn
    (setq occ-global-tsk-collection nil)
    (occ-make-tsk-collection occ-global-tsk-collection-spec)
    (occ-tree-collection-tree occ-global-tsk-collection)
    (occ-collect-tsks occ-global-tsk-collection t)
    (occ-tree-collection-roots occ-global-tsk-collection)
    (setf occ-gtree
          (occ-tree-collection-tree occ-global-tsk-collection)))


  (cl-get-field occ-gtree 'subtree)

  (cl-get-field occ-gtree 'plist)

  (cl-get-field (make-occ-tree-tsk :name "empty tree tsk" :subtree nil) 'subtree)

  (cl-set-field occ-gtree 'subtree 1)

  (cl-class-slots (cl-classname occ-gtree))
  ;; (type-of occ-gtree)

  (setf
   occ-test-gtree
   (occ-tsk-tree-build
    #'(lambda ()
        (or
         (occ-make-tsk-at-point #'make-occ-tree-tsk)
         (make-occ-tree-tsk :name "empty tree tsk" :subtree nil))) ;; note: only using first file of roots
    "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tsks/xx.org"))

  (setq occ-test-gtree
        (occ-tsk-tree-build
         #'(lambda ()
             (or
              (occ-make-tsk-at-point #'make-occ-tree-tsk)
              (make-occ-tree-tsk :name "empty tree tsk" :subtree nil))) ;; note: only using first file of roots
         ;; todo: occ-global-tsk-collection-spec
         org-ctx-clock-tsk-tree-tsk-root-org-file))

  (with-current-buffer (find-file-noselect "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tsks/xx.org")
    (goto-char (point-min))
    (setf occ-file-subtree
          (occ-org-map-subheading
           #'(lambda ()
               (occ-tsk-tree-collect-tsk
                #'(lambda ()
                    (or
                     (occ-make-tsk-at-point #'make-occ-tree-tsk)
                     (make-occ-tree-tsk :name "empty tree tsk" :subtree nil)))))))))
;; ctor.el




;; obj.el
;; (when nil

;;   (cl-defstruct xpoint
;;     x y)

;;   (setf zpoint (make-xpoint :x 5 :y 3))

;;   (setf (cl-struct-slot-value 'xpoint 'x point) 3)

;;   (cl--find-class 'xpoint)

;;   (cl-defstruct base
;;     baseattr)

;;   (cl-defstruct (drived (:include base))
;;     drivedattr)

;;   (setf baseobj1 (make-base :baseattr "xbaseattr"))



;;   (setf drivedobj1
;;         (make-drived
;;          :baseattr "xbaseattr"
;;          :drivedattr "xdrivedattr")))
;; obj.el

;; (helm :sources 'h-source)

;; (occ-collect-list occ-global-tsk-collection)
;; (occ-tree-collection-list occ-global-tsk-collection)



(when nil
  (defun occ-capture-test ()
    (interactive)
    (let* ((ctsk (occ-select (occ-make-ctx nil) #'occ-list))
           (tsk  (if ctsk (occ-ctsk-tsk ctsk)))
           (mrk  (if tsk (occ-tsk-marker tsk))))
      (before-org-capture+ 'entry `(marker ,mrk) 'occ-capture+-helm-select-template '(:empty-lines 1)
        (read-from-minibuffer "Test: ")
        t)))


  (defun occ-capture-test ()
    (interactive)
    (let* ((ctsk (occ-select (occ-make-ctx nil) #'occ-list))
           (ctx  (if ctsk (occ-ctsk-ctx ctsk)))
           (tsk  (if ctsk (occ-ctsk-tsk ctsk)))
           (mrk  (if tsk (occ-tsk-marker tsk))))
      (org-capture-plus 'entry `(marker ,mrk) 'occ-capture+-helm-select-template
                        :finalize (lambda ()
                                    (occ-props-window-edit-with tsk ctx 7)
                                    t)
                        :empty-lines 1)))

 (macroexpand-1
  '(before-org-capture-plus mrk 'entry `(marker ,org-clock-marker) 'occ-capture+-helm-select-template '(:empty-lines 1)
     t))

 (let* ((finalize (function (lambda nil t))))
   (org-capture-plus 'entry
                     `(marker ,org-clock-marker)
                     'occ-capture+-helm-select-template
                     :finalize nil
                     '(:empty-lines 1)))


 (before-org-capture-plus mrk 'entry `(marker org-clock-marker) 'occ-capture+-helm-select-template '(:empty-lines 1)
   t))



;; (occ-delayed-select-obj-prop-edit-when-idle (occ-make-ctx nil) (occ-make-ctx nil) occ-idle-timeout)
;; (occ-delayed-select-obj-prop-edit-when-idle nil (occ-make-ctx nil) occ-idle-timeout)


(when nil
  (cl-method-first-arg 'occ-ctx-property-get)
  (occ-readprop-props)
  (cl-method-matched-arg 'occ-readprop 'occ-ctx-property-get (occ-make-ctx-at-point))
  (funcall 'occ-ctx-property-get (cons 'file (occ-make-ctx-at-point))))

(when nil
  (cl-method-sigs-matched-arg
   '(occ-readprop (`((head ,val) occ-ctx) val))
   '(occ-ctx-property-get (`((head ,val) val)))
   (occ-make-ctx-at-point)))

;; move to occ-test.el
;; (cl-method-param-case-with-value-new '(occ-get-property  (`(occ-ctx (eql ,val)) val)) (occ-make-ctx-at-point))
;; (occ-match-prop-method-args (occ-make-ctx-at-point))

;; (cl-method-param-signs 'occ-readprop-with)

;; (cl-method-param-signs 'occ-get-property)




;; (org-entry-get-multivalued-property (point) "ZEGMA")
;; (org-entry-put-multivalued-property (point) "ZEGMA" "T1" "T2")
;; (org-entry-add-to-multivalued-property (point) "ZEGMA" "TEST1")
;; (org-entry-get (point) "ZEGMA")
;; (org-entry-set (point) "ZEGMA" "VALUE")



;;; occ-test.el ends here
