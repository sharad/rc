
;; (defpackage :pa
;;   ;; I am not using stumpwm any one.
;;   ;; stumpwm or any wm is going to use me.
;;   ;; I have to use drivers.
;;   (:use :common-lisp :cl-ppcre :pa.driver.emacs.planner)
;;   ;(:export #:*net-device*)
;;   )
;; (in-package :pa)




;; ;; Put here all real code,
;; ;; PERT, CPM all OR
;; ;; Advising etc.


;; ;; (defvar *driver* 'emacs-planner "Backend Driver")

;; (defclass project-manager ()
;;   ((name :initarg :name :accessor name)))

;; (defgeneric get-projects (manager)
;;   )

;; (defclass emacs-planner (project-manager)
;;   )

;; (defmethod get-projects (manager emacs-planner)
;;   (get-emacs-plans-today))

