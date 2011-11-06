;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;Functions, aliases, macros;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Useful things. Usually defining new Stumpwm commands.

(defun shellcommand (command) "Run a shell command and display output to screen.
    This must be used in a functional side-effects-free style! If a program does not
    exit of its own accord, Stumpwm might hang!"
       (check-type command string)
       (echo-string (current-screen) (run-shell-command command t)))

(defcommand shell-command (command) ((:string "sh: "))
  (check-type command string)
  (shellcommand command))

   
(define-stumpwm-command "shell-command" ((command :string "sh: " :string))
  (check-type command string)
  (shell-command command))

(defun cat (&rest strings) "Concatenates strings, like the Unix command 'cat'.
    A shortcut for (concatenate 'string foo bar)."
       (apply 'concatenate 'string strings))

(defun stump-send-click (button iterations)
  "Send a click to the current pointer location.
    `button' is which mouse button to use and `iterations' is how many times to click
    (so twice would be a double-click, for example)."
  (loop while (> iterations 0) do
       (shell-command (cat "xte 'mouseclick " (write-to-string button) "'"))
       (setq iterations (- iterations 1))))

(define-stumpwm-command "new-ratclick" ((button :number "Button: ") (iterations :number "How many times? "))
  (when (current-window)
    (stump-send-click button iterations)))

;; I prefer to bind these to a key, since I use a separate keymap for all
;; my mouse control needs. If you provide no argument, it will prompt for
;; both. If you provide one, it'll prompt for the second, and if you
;; provide both it'll just click.

(setf *rat-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "s") "new-ratclick 1 1")
        (define-key m (kbd "d") "new-ratclick 2 1")
        (define-key m (kbd "f") "new-ratclick 3 1")
        (define-key m (kbd "S") "new-ratclick 1")
        (define-key m (kbd "D") "new-ratclick 2")
        (define-key m (kbd "F") "new-ratclick 3")
        m))
