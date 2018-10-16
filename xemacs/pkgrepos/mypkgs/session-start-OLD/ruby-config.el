;;
;; ruby.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Mon Jul 26 18:58:12 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;

;; from http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-talk/114878
(deh-require-maybe ruby-mode




;;; Execute Ruby Command with f3
  (define-key ruby-mode-map [f3] 'ruby-load-file)

  (defun ruby-xmp-region (reg-start reg-end)
    "Pipe the region through Ruby's xmp utility and replace the region
with the result."
    (interactive "r")
    (shell-command-on-region reg-start reg-end
                             "ruby -r xmp -n -e 'xmp($_, \"%l\t\t# %r\n\")'" t))

  (global-set-key [(meta f10)] 'ruby-xmp-region)


  (defun ruby-custom-setup ()
    (add-to-list 'hs-special-modes-alist
                 '(ruby-mode
                   "\\(def\\|do\\)"
                   "end"
                   "#"
                   (lambda (arg) (ruby-end-of-block))
                   nil
                   ))
    (hs-minor-mode t))

  (add-hook 'ruby-mode-hook
            '(lambda ()
              (define-key ruby-mode-map "\M-q" 'jw-rb-fill-comment-region) ))

  (add-hook 'ruby-mode-hook 'ruby-custom-setup)

                                        ; Chmod of scripts to u+x
  (add-hook 'after-save-hook
            '(lambda ()
              (progn
                (and (save-excursion
                       (save-restriction
                         (widen)
                         (goto-char (point-min))
                         (save-match-data
                           (looking-at "^#!"))))
                     (shell-command (concat "chmod u+x " buffer-file-name))
                     (message (concat "Saved as script: " buffer-file-name))
                     ))))

                                        ;JeffreyRadcliffe -- Here is a method which calls the interpreter on the entire buffer, putting the output in another window:
  (defun ruby-eval-buffer () (interactive)
         "Evaluate the buffer with ruby."
         (shell-command-on-region (point-min) (point-max) "ruby"))

  (defun ruby-xmp-region (reg-start reg-end)
    "Pipe the region through Ruby's xmp utility and replace
      the region with the result."
    ;; PragDave -- This li'l beaut runs Gotoken's xmp processor on a region.
    ;; This adds the results of evaluating each line to the end of that line.
    ;; It's how I add values to code examples in my e-mail. Here it's bound to
    ;; M-F10
    (interactive "r")
    (shell-command-on-region reg-start reg-end
                             "ruby -r xmp -n -e 'xmp($_, \"%l\t\t# %r\n\")'" t))

  (global-set-key [(control f10)] 'ruby-xmp-region)

  (global-set-key [dead-tilde] "~")

  (setq ri-ruby-script "~/.xemacs/packages/ruby/ri-emacs.rb")
  ;; (autoload 'ri "/home/bschroed/.xemacs/ri-ruby.el" nil t)
  ;;
  ;;  You may want to bind the ri command to a key.
  ;;  For example to bind it to F1 in ruby-mode:
  ;;
  (deh-require-maybe ri-ruby
    (add-hook 'ruby-mode-hook (lambda () (local-set-key (kbd "<f1>") 'ri))))

  ;; (setq ruby-mode-hook (remove (nth 4 ruby-mode-hook) ruby-mode-hook))

  ;; http://www.emacswiki.org/emacs/FlymakeRuby
  ;; http://github.com/purcell/emacs.d/blob/master/site-lisp/flymake-ruby/flymake-ruby.el
  (xrequire 'flymake-ruby)


;;{{ from: http://stackoverflow.com/questions/1282501/running-irb-in-emacs-via-run-ruby-echos-everything-i-type
  ;; Define Ruby Mode Hook
  (defun my-ruby-mode-hook ()
    (progn
      (setq comint-process-echoes t)
      (turn-on-font-lock)
      (auto-fill-mode)
      (yas/minor-mode)
      (inf-ruby-keys)))

  ;; Register Ruby Mode Hook
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
;;}}

  ;; (deh-require-maybe rinari
  ;;   t)
  )



(provide 'ruby-config)
