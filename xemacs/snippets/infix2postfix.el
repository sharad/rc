;; https://gist.github.com/kuoe0/5664977
;;; Filename:    expr.lsp
;;; Author:      KuoE0 <kuoe0.tw@gmail.com>
;;; Description: Convert infix expression to postfix expression
;;;
;;; Distruted under terms of the BSD license.

; split string by one space
(defun split-by-space (str)
	(let ((len (length str))
		  (ret nil))
		(do ((i 0 (+ i 1))
			 (j 0))
			; stop condition
			((> i len)
			 ; return reversed result
			 (reverse ret))
			; find position of next space
			(setf j (position ?\s (subseq str i)))
			; Does there is any space exist?
			(if (null j)
				; if there is no space (means that arrived end of string)
				(setf j len)
				; there is still any space
				(setf j (+ i j)))
			; append the substring into result
			(setf ret (cons (subseq str i j) ret))
			(setf i j))))

; return operator priority
(defun op-priority (op)
  (cond
    ((or (equal op "+") (equal op "-")) 1)
    ((or (equal op "*") (equal op "/")) 2)))

; check the token is operator or not
(defun isOperator (op)
	(if (or (equal op "+")
			(equal op "-")
			(equal op "*")
			(equal op "/"))
		t))

; formating output
(defun* formating (tokens)
	(let ((ret ""))
		(loop for tok in tokens
       do (setf ret (concatenate 'string ret " " tok)))
		; remove leadding space
		(return-from formating (subseq ret 1))))

; convert infix expression to postfix expression
(defun infixToPostfix (expr)
	; split expression by space
	(let ((tokens (split-by-space expr))
		  (stk ())
		  (ret ()))
		(loop for op in tokens
			do (cond
					; operator
					((isOperator op)
						(do ()
							; pop until a low-priority operator appeared or stack is empty or left parentheses appeared
							((or (null stk) (null (isOperator (car stk))) (> (op-priority op) (op-priority (car stk)))))
							(setf ret (append ret (list (pop stk)))))
						(push op stk))
					; left parentheses
					((equal op "(")
						(push op stk))
					; right parentheses
					((equal op ")")
						; pop until left parentheses appeared
						(do ()
							((equal (car stk) "("))
							(setf ret (append ret (list (pop stk)))))
						; pop left parentheses
						(pop stk))
					; numbers, directly put them
					(t (setf ret (append ret (list op))))))
		(setf ret (append ret stk))
		(formating ret)))

; read input until EOF
(do ((expr (read-line) (read-line)))
	((equal expr ""))
	(princ (infixToPostfix expr))
  (terpri))


(infixToPostfix "1 * 2 + 4")
