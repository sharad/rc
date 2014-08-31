;;; javascript-config.el --- javascript

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@sdfsdgfsdfgdfg>
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



;; FlymakeJavaScript

;; There are multiple options for checking Javascript syntax that can be used within flymake. The javascript engines SpiderMonkey and Rhino can both check Javascript syntax. Alternatively, you can use Douglas Crockford’s rather handy JSLint, which is much less forgiving of bad syntax, and can enforce a sensible coding style. Another option is JSHint, which is a fork of JSLint, modified to be not as strict and more configurable in its requirements than the original. The following describe some of these options.
;; Contents

;;    1. With JSLint on Rhino
;;    2. With jslint command line from node.js
;;    3. With JSLint server on node.js (lintnode)
;;    4. With SpiderMonkey
;;    5. With Rhino
;;    6. With JSLINT or JSHINT on Windows using Cscript.exe

;; With JSLint on Rhino

;; First, you will need to install Rhino (**not necessary on Windows, see below), and download jslint.js for Rhino [1]. I’ve got it located in ~/soft/jslint, and you will want to update the code below to match where you’ve put it.

;; Next you will want to create a file called flymake-jslint.el on your LoadPath like the following

(deh-require-maybe flymake-js


;; http://lapin-bleu.net/riviera/?p=191


  ;; (setq flymake-err-line-patterns
  ;;       (cons '("Error:\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
  ;;       	nil 1 2 3)
  ;;             flymake-err-line-patterns))

    (defun flymake-jslint-init ()
      "Construct a command that flymake can use to check javascript source."
      (list flymake-jslint-command (list ;; "-process"
                                         (flymake-init-create-temp-buffer-copy
                                          'flymake-jslint--create-temp-in-system-tempdir))))


    (setq flymake-jslint-command "jslint"
          flymake-jslint-err-line-patterns
          '(("Error:\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$" nil 1 2 3)
            ("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" nil 2 nil 3)
            ("^\\(.+\\)(\\([0-9]+\\)): \\(SyntaxError:.+\\)$" nil 2 nil 3)
            ("^\\(.+\\)(\\([0-9]+\\)): \\(lint \\)?\\(warning:.+\\)$" nil 2 nil 4)
            )
          flymake-jslint-trailing-comma-err-line-pattern
          '("^\\(.+\\)\:\\([0-9]+\\)\: strict \\(warning: trailing comma.+\\)\:$" nil 2 nil 3))



;; (defun flymake-jslint-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		     'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;; 		      temp-file
;; 		      (file-name-directory buffer-file-name))))
;;     (list "rhino" (list (expand-file-name "~/soft/jslint/jslint.js") local-file))))

;; (setq flymake-allowed-file-name-masks
;;       (cons '(".+\\.js$"
;; 	      flymake-jslint-init
;; 	      flymake-simple-cleanup
;; 	      flymake-get-real-file-name)
;; 	    flymake-allowed-file-name-masks))

;; (setq flymake-err-line-patterns
;;       (cons '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"
;; 	      nil 1 2 3)
;; 	    flymake-err-line-patterns))

;; (provide 'flymake-jslint)

;; and import it from your DotEmacs like so:

;; (require 'flymake-jslint)
;; (add-hook 'javascript-mode-hook
;; 	  (lambda () (flymake-mode t)))

;; You can control options of JSLint by using special comments in
;; your source code which are described in the documentation
;; [2]. For example, the following is the same as using the
;; recommended options on jslint.com, and also the assume a browser
;; option. It also defines the name MochiKit, which is imported
;; elsewhere.

;; /*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */

;; /*global MochiKit */

;; Seems like you really want (flymake-mode 1) in that hook,
;; not (flymake-mode t). According to the documentation, to turn on
;; the minor mode explicitly requires that the arg be positive, not
;; non-nil. --DinoChiesa

;; With jslint command line from node.js

;; Install jslint from node

;; $ npm -g install jslint

;; Now you should have a jslint command on your path, you can set it
;; up in a similar way as the Rhino example above or put this in your
;; .emacs

;; (when (load "flymake" t)
;;   (defun flymake-jslint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		       'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "jslint" (list local-file))))

;;   (setq flymake-err-line-patterns
;; 	(cons '("^  [[:digit:]]+ \\([[:digit:]]+\\),\\([[:digit:]]+\\): \\(.+\\)$"
;; 		nil 1 2 3)
;; 	      flymake-err-line-patterns))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.js\\'" flymake-jslint-init)))

;; Latest versions of node-jslint (as of 10-dec-2011) have changed
;; the error reporting format, breaking the pattern regexp above
;; and, even worse, reporting errors on two lines. See
;; http://lapin-bleu.net/riviera/?p=191


;; You can do

;; $ npm -g install jshint

;; and then install flymake-jshint using marmalade
;; (http://marmalade-repo.org/). You may need to do the following in
;; /usr/local/bin:

;; ln -s /usr/local/Cellar/node/0.6.6/lib/node_modules/csslint/cli.js

;; and then put (add-to-list ‘exec-path “/usr/local/bin”) somwhere in your .emacs.

;; -Dave Dreisigmeyer
;; With JSLint server on node.js (lintnode)

;; If you run the above in a persistent server on v8 instead of
;; invoking rhino every time, it goes about twice as fast. Server code
;; with instructions is available at http://github.com/keturn/lintnode
;; With SpiderMonkey

;; JavaScript? syntax checking using spidermonkey. This also detects object trailing comma like:

;; var obj = {
;;   a: 1,
;;   b: 2,
;; }; // this line is highlighted.

;; You’ll need Karl’s JavaScriptMode, spidermonkey 1.5 or greater and emacs 22.

  ;; (defconst flymake-allowed-js-file-name-masks '(("\\.json$" flymake-js-init)
  ;;                                                ("\\.js$" flymake-js-init)))
  ;; (defcustom flymake-js-detect-trailing-comma t nil :type 'boolean)
  ;; (defvar flymake-js-err-line-patterns '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3)))
  ;; (when flymake-js-detect-trailing-comma
  ;;   (setq flymake-js-err-line-patterns (append flymake-js-err-line-patterns
  ;;                                              '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$" 1 2 nil 3)))))

  ;; (defun flymake-js-init ()
  ;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
  ;;                      'flymake-create-temp-inplace))
  ;;          (local-file (file-relative-name
  ;;                       temp-file
  ;;                       (file-name-directory buffer-file-name))))
  ;;     (list "js" (list "-s" local-file))))
  ;; (defun flymake-js-load ()
  ;;   (interactive)
  ;;   (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  ;;     (setq flymake-check-was-interrupted t))
  ;;   (ad-activate 'flymake-post-syntax-check)
  ;;   (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-js-file-name-masks))
  ;;   (setq flymake-err-line-patterns flymake-js-err-line-patterns)
  ;;   (flymake-mode t))

  ;; (add-hook 'javascript-mode-hook '(lambda () (flymake-js-load)))

;; With Rhino

;; There’s another implementation of Flymake JavaScript mode that sits on top of the Rhino engine.

;; I have some problems with this one. I assume it is a problem with the path to rhino.js and env.js, but I am not sure. I have placed those file in the folder c:/emacs/rhino-related/ which I mention in rhino.js (see below, is this correct?):

;;   // Where you store your files
;;   var project_folder = 'c:/emacs/rhino-related/';
;;   // Browser environment wrapper over Rhino
;;   load(project_folder + 'env.js');
;;   // For DOM constructing
;;   window.location = project_folder + 'blank.html';
;;   var my_script = arguments[0];
;;   // If DOM ready
;;   window.onload = function(){
;;       // Avoid recursive inclusion
;;       if ("rhino_flymake.js" != my_script) {
;;           load(my_script);
;;       }
;;   }

;; I get strange errors, like:

;;   parsed 'Exception in thread "Thread-0" org.mozilla.javascript.WrappedException:
;;   Wrapped java.net.MalformedURLException: unknown protocol: c',
;;   no line-err-info

;; It looks like Rhino can not take care of the c: in the path. Anyone who understands what is happening? – LennartBorgman

;; I do not have a Windows box to verify the fix below, but according to some forum posts, you should declare the path in scheme file:///C:/path/to/file.

;;   var project_folder = 'file:///c:/emacs/rhino-related/';

;; – Nyuhuhuu

;; Thanks Nyuhuhuu, that made it work on Windows.

;; Before I fixed the issue you speak of, I had to fix an apparent problem with ‘compilation-error-regexp-alist-alist’ in ‘flymake.el’. For details on that, please see this post on gnu.emacs.help.

;; After that was fixed, I was able to get the Java Rhino process running on target with this (using the Windows build of Emacs, on Windows XP):


;;     var project_folder = 'c:\\Progra~1\\emacs\site\\rhino-web-browser-js-environment\\';

;; Then in my ‘flymake-js-init’ function definition, I have this:

    ;; (list "java" (list "-jar" "c:/Progra~1/rhino1_6R7/js.jar" "c:/Progra~1/emacs/site/rhino-web-browser-js-environment/rhino_flymake.js" local-file))

;; That said, after said success I ran into other problems, apparently unrelated to any of my previous fixes and configurations, down the road. Please see the last half of the third comment on this post on gnu.emacs.help.

;; It would be very cool if this could be made to work. If we can figure it out, we could make a nice how-to on this that I bet a lot of people would benefit from.

;;   -- ChristopherMBalz

;; I get a lot of errors on the first line, like

;;   - Context.Java(1757)
;;   - MemberBox.java(187)
;;   etc.


;; That is this?
;; With JSLINT or JSHINT on Windows using Cscript.exe

;; There’s no need to install Rhino on Windows; Windows has a built-in Javascript engine in WSH. Therefore you can run a JS program on any Windows, via WSH.

;; Relying on WSH, the community provides flymake-for-jslint-for-wsh.el, to allow you to use jslint or JSHINT as the flymake tool for .js buffers on windows.

;; A sample of using jslint-for-wsh.js as a “compile” command in emacs:

;; http://i49.tinypic.com/11kuypx.jpg

;; A sample of using jslint-for-wsh in emacs, with flymake-for-jslint :

;; http://i47.tinypic.com/2mk1eh.jpg


)


(unless (require 'js2-mode nil t)
  (defalias 'js2-mode 'js-mode))

(setq
 js-indent-level 2 ;;4
      )
(add-to-list 'interpreter-mode-alist '("node" . js-mode))

(deh-require-maybe (and sws-mode jade-mode)
  ;; https://github.com/brianc/jade-mode

  (deh-section "flymake"
    (defun flymake-jade-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-intemp))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name)))
             (arglist (list local-file)))
        (list "jade" arglist)))
    (setq flymake-err-line-patterns
          (cons '("\\(.*\\): \\(.+\\):\\([[:digit:]]+\\)$"
                  2 3 nil 1)
                flymake-err-line-patterns))
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.jade\\'" flymake-jade-init))))

(provide 'javascript-config)
;;; javascript-config.el ends here
