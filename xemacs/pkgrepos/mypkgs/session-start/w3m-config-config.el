;;
;; w3m-config.el
;;
;; Made by sh4r4d
;; Login   <s@taj>
;;
;; Started on  Wed Sep  2 23:41:57 2009 sh4r4d
;; Last update Wed Sep  2 23:42:43 2009 sh4r4d

;; WThreeMKeymap

;; I found the default keymap somewhat confusing. The point of doing
;; web browsing in emacs is to use familiar keys for navigation. So,
;; here’s the keymap I use that feels more “emacs-like” for
;; me. Sweeten to taste.

 ;; (let ((map (make-keymap)))
 ;;   (suppress-keymap map)
 ;;   (define-key map [backspace] 'w3m-scroll-down-or-previous-url)
 ;;   (define-key map [delete] 'w3m-scroll-down-or-previous-url)
 ;;   (define-key map "\C-?" 'w3m-scroll-down-or-previous-url)
 ;;   (define-key map "\t" 'w3m-next-anchor)
 ;;   (define-key map [(shift tab)] 'w3m-previous-anchor)
 ;;   (define-key map [(shift iso-left-tab)] 'w3m-previous-anchor)
 ;;   (define-key map "\C-m" 'w3m-view-this-url)
 ;;   (define-key map [(shift return)] 'w3m-view-this-url-new-session)
 ;;   (define-key map [(shift kp-enter)] 'w3m-view-this-url-new-session)
 ;;   (define-key map [(button2)] 'w3m-mouse-view-this-url)
 ;;   (define-key map [(shift button2)] 'w3m-mouse-view-this-url-new-session)
 ;;   (define-key map " " 'scroll-up)
 ;;   (define-key map "a" 'w3m-bookmark-add-current-url)
 ;;   (define-key map "\M-a" 'w3m-bookmark-add-this-url)
 ;;   (define-key map "+" 'w3m-antenna-add-current-url)
 ;;   (define-key map "A" 'w3m-antenna)
 ;;   (define-key map "c" 'w3m-print-this-url)
 ;;   (define-key map "C" 'w3m-print-current-url)
 ;;   (define-key map "d" 'w3m-download)
 ;;   (define-key map "D" 'w3m-download-this-url)
 ;;   ;; (define-key map "D" 'w3m-download-with-wget)
 ;;   ;; (define-key map "D" 'w3m-download-with-curl)
 ;;   (define-key map "g" 'w3m-goto-url)
 ;;   (define-key map "G" 'w3m-goto-url-new-session)
 ;;   (define-key map "h" 'describe-mode)
 ;;   (define-key map "H" 'w3m-gohome)
 ;;   (define-key map "I" 'w3m-toggle-inline-images)
 ;;   (define-key map "\M-i" 'w3m-save-image)
 ;;   (define-key map "M" 'w3m-view-url-with-external-browser)
 ;;   (define-key map "n" 'w3m-view-next-page)
 ;;   (define-key map "N" 'w3m-namazu)
 ;;   (define-key map "o" 'w3m-history)
 ;;   (define-key map "O" 'w3m-db-history)
 ;;   (define-key map "p" 'w3m-view-previous-page)
 ;;   (define-key map "q" 'w3m-close-window)
 ;;   (define-key map "Q" 'w3m-quit)
 ;;   (define-key map "R" 'w3m-reload-this-page)
 ;;   (define-key map "s" 'w3m-search)
 ;;   (define-key map "S" (lambda ()
 ;;        		  (interactive)
 ;;        		  (let ((current-prefix-arg t))
 ;;        		    (call-interactively 'w3m-search))))
 ;;   (define-key map "u" 'w3m-view-parent-page)
 ;;   (define-key map "v" 'w3m-bookmark-view)
 ;;   (define-key map "W" 'w3m-weather)
 ;;   (define-key map "=" 'w3m-view-header)
 ;;   (define-key map "\\" 'w3m-view-source)
 ;;   (define-key map "?" 'describe-mode)
 ;;   (define-key map ">" 'scroll-left)
 ;;   (define-key map "<" 'scroll-right)
 ;;   (define-key map "." 'beginning-of-buffer)
 ;;   (define-key map "^" 'w3m-view-parent-page)
 ;;   (define-key map "]" 'w3m-next-form)
 ;;   (define-key map "[" 'w3m-previous-form)
 ;;   (define-key map "}" 'w3m-next-image)
 ;;   (define-key map "{" 'w3m-previous-image)
 ;;   (define-key map "\C-c\C-c" 'w3m-submit-form)
 ;;   (setq dka-w3m-map map))

 ;; (add-hook 'w3m-mode-hook '(lambda () (use-local-map dka-w3m-map)))



(when (and (xrequire 'w3m-load)
           (xrequire 'w3m)
           (xrequire 'w3m-session)
           (xrequire 'w3m-lnum))

  (setq browse-url-browser-function 'w3m-browse-url
        browse-url-new-window-flag t
        w3m-use-cookies t
        w3m-key-binding 'info
        w3m-home-page "http://www.google.com"
        w3m-session-file "~/.emacs.d/w3m-session"
        w3m-session-save-always nil
        w3m-session-load-always nil
        w3m-session-show-titles t
        w3m-session-duplicate-tabs 'ask
        w3m-use-cookies t)

        (defun jao-w3m-go-to-linknum ()
          "Turn on link numbers and ask for one to go to."
          (interactive)
          (let ((active w3m-link-numbering-mode))
            (when (not active) (w3m-link-numbering-mode))
            (unwind-protect
                (w3m-move-numbered-anchor (read-number "Anchor number: "))
              (when (not active) (w3m-link-numbering-mode)))))

        (defun local-w3m-mode-hook ()
          (define-key w3m-mode-map "f" 'jao-w3m-go-to-linknum)
          (define-key w3m-mode-map (kbd "C-x b") nil))

        (add-hook 'w3m-mode-hook 'local-w3m-mode-hook)

        ;; http://www.emacswiki.org/emacs/emacs-w3m
        (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
        ;; optional keyboard short-cut
        (global-set-key-if-unbind "\C-xm" 'browse-url-at-point))

(provide 'w3m-config-config)
