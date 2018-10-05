;;; tree.el --- Tree utils

;; Copyright (C) 2013  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d@gmail.com>
;; Keywords: lisp, convenience

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

;; Tree utils

;;; Code:

(require 'general-testing)

(defun get-tree-node (tree &rest keys)
  (reduce (lambda (xtree k)
            ;; (message "tree %s k %s ret (cdr (assoc k xtree)) %s" xtree k (cdr (assoc k xtree)))
            (cdr (assoc k xtree)))
          keys
          :initial-value tree))

(defmacro* tree-node (tree &rest keys &key (test 'eql))
  (if keys
      `(cdr
        (assoc ,(car (last keys))
               (pushnewx
                (list ,@(last keys))
                (tree-node ,tree ,@(butlast keys) :test ,test)
                :key 'car :test ,test)))
      tree))


(defmacro* test-tree-node (tree &rest xkeys &key test)
  `(list ,tree ,@xkeys ,test ))

;; (macroexpand-all '(test-tree-node  'testa 11 'testbug 'interAAAA 'bug_status))


(defmacro* tree-node (tree &rest keys)
  (if keys
      `(cdr
        (assoc ,(car (last keys))
               (pushnew
                (list ,@(last keys))
                (tree-node ,tree ,@(butlast keys))
                :key 'car)))
      tree))

;; (macroexpand-all '(tree-node testbug "interAAAA" "bug_status" :test 'qqq))

(defmacro* tree-node (tree &rest xkeys)
  (let* ((pos (position-if 'keywordp xkeys))
         (keys (if pos (subseq xkeys 0 pos) xkeys))
         (okeys (if pos (subseq xkeys pos))))
    (if keys
        `(cdr
          (assoc* ,(car (last keys))
                  (tree-node ,tree ,@(butlast keys) ,@okeys)
                  ,@okeys))
        tree)))

(defmacro* tree-node* (tree &rest xkeys)
  (let* ((pos (position-if 'keywordp xkeys))
         (keys (if pos (subseq xkeys 0 pos) xkeys))
         (okeys (if pos (subseq xkeys pos))))
    (if keys
        `(cdr
          (assoc* ,(car (last keys))
                  (pushnew
                   (list ,@(last keys))
                   (tree-node* ,tree ,@(butlast keys) ,@okeys)
                   :key 'car ,@okeys)
                  ,@okeys))
        tree)))

;; (defmacro* tree-node-front* (tree &rest xkeys)
;;   (let* ((pos (position-if 'keywordp xkeys))
;;          (keys (if pos (subseq xkeys 0 pos) xkeys))
;;          (okeys (if pos (subseq xkeys pos))))
;;     (if keys
;;         `(cdr
;;           (assoc* ,(car (last keys))
;;                   (pushnew
;;                    (list ,@(last keys))
;;                    (tree-node* ,tree ,@(butlast keys) ,@okeys)
;;                    :key 'car ,@okeys)
;;                   ,@okeys))
;;         tree)))

;; (defun endcons (a v)
;;    (if (null v) (cons a nil) (cons (car v) (endcons a (cdr v)))))

;; ;; (endcons 'a '(b c d))

;; (defmacro* tree-node-back* (tree &rest xkeys)
;;   (let* ((pos (position-if 'keywordp xkeys))
;;          (keys (if pos (subseq xkeys 0 pos) xkeys))
;;          (okeys (if pos (subseq xkeys pos))))
;;     (if keys
;;         `(cdr
;;           (assoc* ,(car (last keys))
;;                   (pushnew
;;                    (list ,@(last keys))
;;                    (tree-node* ,tree ,@(butlast keys) ,@okeys)
;;                    :key 'car ,@okeys)
;;                   ,@okeys))
;;         tree)))

;; (progn
;;   (setq u nil)
;;   (setf (tree-node-back* u "a" "b" "c" :test 'string-equal)  'ww )
;;   (setf (tree-node-back* u 'a 'b 'c)  'ww )
;;   u)

;; (setq u nil)
;; (setf (tree-node* u "a" "b" "c" :test 'string-equal)  'ww )
;; (setf (tree-node* u 'a 'b 'c)  'ww )

(defun read-mb (prompt collection)
  (let ((finish-reading nil))
   (flet (;; (finish-reading nil)
          (minibuffer-local-map (copy-keymap minibuffer-local-map))
          (read-done ()
            (throw 'goforlist
              (let (input-string)
                (move-beginning-of-line nil)
                (setq
                 finish-reading t
                 input-string (buffer-substring (point) (point-max)))
                (catch 'exit (exit-minibuffer))
                input-string))))
     (define-key minibuffer-local-map (kbd "S-RET") 'read-done)
     (let ((ret (catch 'goforlist
                  (completing-read prompt collection nil t))))
       (if finish-reading
           (list ret t)
           (if (string-equal ret "")
               (list nil t)
               (list ret nil)))))))

;; (read-mb "af: " '("afdf" "dasfdsf"))


(defun depth (tree)
  ;; http://www.lispforum.com/viewtopic.php?p=5372&sid=e117daaa584b63c64864135d178ea654#p5372
  (if (atom tree)
      0
    (if (cdr tree)
        (if (atom (cdr tree))
            1
          (1+ (apply 'max (mapcar #'depth tree))))
      (if (atom (car tree))
          1
        (1+ (apply 'max (mapcar #'depth tree)))))))

(testing
 (depth '(nil .  (a (b  ( c . d) ))))

 (depth '( (b) a ))

 (depth '())
 (depth nil)
 (depth 'nil)
 (depth '(nil)))

(defun* get-tree-leaves (tree &optional (depth 0))
  (let ((tdepth (depth tree)))
    (if (>= depth tdepth)
        (if (= depth tdepth)
            (list tree))
      (let (ret)
        (if (and (cdr tree)
                 (atom (cdr tree)))
            (and (cdr tree)
                 (list (cdr tree)))
          (dolist (tr (or (cdr tree)
                          (list (car tree))) ret)
            ;; (dolist (tr tree ret)
            (setq
             ret
             (append ret
                     (get-tree-leaves tr depth)))))))))

(defun* tree-leaves (tree &optional (depth 0))
  (let ((tdepth (depth tree)))
    (if (>= depth tdepth)
        (if (= depth tdepth)
            (list tree))
      (let (ret)
        (if (and (cdr tree)
                 (atom (cdr tree)))
            (and (cdr tree)
                 (list (cdr tree)))
          ;; (dolist (tr (or (cdr tree)
          (dolist (tr (or tree
                          (list (car tree))) ret)
            ;; (dolist (tr tree ret)
            (setq
             ret
             (append ret
                     (tree-leaves tr depth)))))))))


(testing


 (get-tree-leaves '(a
                    (z)
                    (b (c . d) (e . d) i)
                    (o (n (p . q) (w . x)))) 1)

 (get-tree-leaves '((a
                     (z)
                     (b (c . d) (e . d) i)
                     (o (n (p . q) (w . x))))) 1)


 ;; (((a (z) (b (c . d) (e . d) i) (o (n (p . q) (w . x))))))


 (car tree)

 (depth '((a (b (c . d) (e . d))
             (o (n (p . q) (w . x))))))


  (progn
    (get-tree-node '((a .((b ((c . d)))))) 'a 'b 'c)
    (get-tree-node '((a (b (c . d)))) 'a 'b 'c)
    (get-tree-node '((a)) 'a 'b 'c)
    (cdr (assoc 'a '((a ((b ((c . d)))))))))

  (progn
    (setq jt '((a (b (c . d)))))
    ;; (setq jt '((a . b)))
    (set-tree-node jt 'o 'n 'b 'c)
    jt)


  (progn
    (setq ol '((k p) (a b)))
    (assoc 'k (pushnew '(f . c) ol :key 'car))))



(provide 'tree)
;;; tree.el ends here
