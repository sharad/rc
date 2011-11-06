;;
;; load-path.el
;; 
;; Made by sh4r4d
;; Login   <s@taj>
;; 
;; Started on  Wed Sep  2 02:03:45 2009 sh4r4d
;; Last update Wed Sep  2 02:04:21 2009 sh4r4d


(defun have-el-file (dir)               ;test
  (directory-files dir t "[a-zA-Z0-9]+.el"))

;; Common Lisp note: Common Lisp allows the function to specify what default value
;; to use when an optional argument is omitted; Emacs Lisp always uses nil.
;; Emacs Lisp does not support csupplied-p variables that tell you whether an argument
;; was explicitly passed.

(defun get-recursive-dir (dir &optional test full)
  "Return recursive directory listing."
  (append
   (if (file-directory-p dir)
       (if (functionp test)
           (if (funcall test dir) (list dir))
         (list dir)))
   (apply                               ; implement it by loop collect.
    #'append                            ; else it will exceed (setq max-lisp-eval-depth 2000)
    (mapcar #'(lambda (d)
                (get-recursive-dir d test full))
            (delete-if-not #'file-directory-p (directory-files dir full "^[^.]+"))))))

(defun get-recursive-dir (dir &optional test full)
  "Return recursive directory listing."
  (append
   (if (file-directory-p dir)
       (if (functionp test)
           (if (funcall test dir) (list dir))
         (list dir)))
   (loop for dir in (delete-if-not #'file-directory-p (directory-files dir full "^[^.]+"))
         append (get-recursive-dir dir test full))))

;;(setq mypath (get-recursive-dir "~/.xemacs/packages" #'have-el-file t))

(if (not running-xemacs) 
    (let ((max-lisp-eval-depth 1500))
     (setq load-path
           (append                           ; added all subdir's
            (get-recursive-dir "~/.xemacs/packages" #'have-el-file t)
            load-path))))
