(require '@)


(setf @test
      (@extend :name "test"))

(setf (@ @test :_node)
      (@extend @test :name "node"
               :slot 'a))

(def@ @test :node ()
      @:_node)




(defmacro def@ (object method params &rest body)
  "Define METHOD body on OBJECT."
  (declare (indent defun))
  `(progn
     (setf (@ ,object ,method)
           (function* (lambda ,(cons '@@ params)
             ,@(if (stringp (car body)) (list (car body)) ())
             (let ((@@@ ,object))
               (with-@@ @@
                   ,@(if (stringp (car body)) (cdr body) body))))))
     ,method))

(def@ @activity :add-def (method fn-body)
      (eval
       `(progn
          (setf (@ @@ ,method)
                (function* ,fn-body)))))

(defmacro defun@ (object method params &rest body)
  "Define METHOD body on OBJECT."
  (declare (indent defun))
  `(progn
     (@! ,object :add-def ',method
         ',`(lambda ,(cons '@@ params)
              ,@(if (stringp (car body)) (list (car body)) ())
              (with-@@ @@
                    ,@(if (stringp (car body)) (cdr body) body)
                  )))))





(defun@ @activity :init-y ()
  (unless (boundp '@:y)
    (@:fn))
  @:y)

(defun@ @activity :note ()
        (unless (boundp '@:_note)
          (@:init-note))
        @:_note)

(@! @activity :note)

(macroexpand-all
 '(defun@ @test :init-x ()
   "test"
   (message "test%s" @:x)
   (message "test %s" @:slot)))

(progn (@! @test :add-def (quote :init-x) (quote (lambda (@@) "test" (with-@@ @@ (message "test%s" 2) (message "test %s" 1))))))

(progn (@! @test :add-def :init-x (quote (lambda (@@) "test" "test" (message "test%s" (@:x)) (message "test %s" @:slot)))))

(setf (@ @test :x) 1)
(setf (@ @test :slot) 2)

(@! @test :init-x)



(with-@@ @test
    (message "test%s" (@:node))
  (message "test %s" @:_node))



(progn (@! @test :add-def :init-x () '(lambda (@@) (let ((@@ @@)) (message "test%s" (@! @@ :x)) (message "test %s" (@ @@ :slot))))))



(@! @test :add-def :init-z '()
    '9)

(defmacro test2 (x)
  `(with-@@ '@@ ,x ))

(defmacro test1 (x)
  `(,x))

(macroexpand-all
 '(test2 z))


(macroexpand-1
 '(with-@@ @@
   (message "test" (@:x))))




(@ @test :init-x)

(def@ @test :init-y ()
      10)





(@ @test :init-x)


(defun@ @test :init-x ()
        (message "test%s" (@:x))
        (message "test %s" @:slot))

(defun@ @activity :note ()
        (unless (boundp '@:_note)
          (@:init-note))
        @:_note)

(@! @test :init-x)

(defun@ (@! @test :node) :init-x ()
        (message "test x"))

(@! (@! @test :node) :init-x)

(defun@ (@ @test :_node) :init-z ()
        (message "test %s" @:slot))

(defun@ (@! @test :node) :init-alpha ()
        (message "test %s" @:slot))


(let ((note (@! @activity :note)))
 (def@ note :destination ()
      (if (boundp '@:_destination)
          @:_destination
        (@:init-destination))))




(def@ @test :def (method fn)
      (setf (@ @@ method) fn))

(@! (@ @test :_node) :def :init-x
    (lambda (@@) (message "test %s" (@ @@ :slot))))

(@! (@ @test :_node) :init-x)

(@ (@ @test :_node) :slot)

(@! (@ @test :_node) :keys)


(def@ (@ @test :_node) :init-y ()
      (message  "test %s" @:slot))


(@! (@ @test :_node) :init-y)

(@ (@ @test :_node) :init-y)


(@! @test :keys)



(defun xdef (x)
  (lexical-let ((N x))
    (function*
     (lambda (x N)
      (message "N: %s x: %s" N x)))))

(funcall (xdef "Test") "AAII" "x")


(function* 'a)
