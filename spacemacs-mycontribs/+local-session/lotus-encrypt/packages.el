;;; packages.el --- lotus-encrypt layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-encrypt-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-encrypt/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-encrypt/pre-init-PACKAGE' and/or
;;   `lotus-encrypt/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-encryptS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-encrypt-packages
  '(
    ;; (PACKAGE :location local)
    epa
    (epa-file :location local)
    )
  "The list of Lisp packages required by the lotus-encrypt layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-encrypt/init-epa ()
  ;; from: http://www.emacswiki.org/emacs/GnusEncryptedAuthInfo
  (use-package epa
      :defer t
      :config
      (progn
        (progn                          ;disable at first
          (epa-file-disable))
        (progn ;; "make-passwd"
          ;; http://www.emacswiki.org/emacs/PasswordGenerator
          (defun* make-password (length &optional (upper t) (lower t) (number t) (symbol nil) (ambiguous nil))
            "Return a string of LENGTH random characters.  If UPPER is non-nil,
use uppercase letters.  If lower is non-nil, use lowercase letters.
If NUMBER is non-nil, use numbers.  If SYMBOL is non-nil, use one of
\"!\"#$%&'()*+'-./:;<=>?@`{}|~\".  If AMBIGUOUS is nil, avoid
characters like \"l\" and \"1\", \"O\" and \"0\"."
            (interactive (make-password-prompt-for-args))
            (let ((char-list (make-password-char-list upper lower number symbol ambiguous))
                  position password)
              (random t)
              (loop for i from 1 to length
                 do (setq position (random (length char-list))
                          password (concat password (string (nth position char-list)))))
              (if (interactive-p)
                  (let* ((strength (make-password-strength length upper lower number symbol ambiguous))
                         (bits (car strength))
                         (number (cadr strength)))
                    (message "The password \"%s\" is one of 10^%d possible and has a bit equivalence of %d"
                             password (round number) (round bits)))
                  password)))

          (defun make-password-char-list (upper lower number symbol ambiguous)
            (let* ((upper-chars-ambiguous '(?I ?O ?G))
                   (upper-chars (loop for i from ?A to ?Z unless
                                     (member i upper-chars-ambiguous)
                                   collect i))
                   (lower-chars-ambiguous '(?l ?o))
                   (lower-chars (loop for i from ?a to ?z unless
                                     (member i lower-chars-ambiguous)
                                   collect i))
                   (number-chars-ambiguous '(?0 ?1 ?6))
                   (number-chars (loop for i from ?0 to ?9 unless
                                      (member i number-chars-ambiguous)
                                    collect i))
                   (symbol-chars '(?! ?@ ?# ?$ ?% ?& ?* ?( ?) ?+ ?= ?/
                                   ?{ ?} ?[ ?] ?: ?\; ?< ?>))
                   (symbol-chars-ambiguous '(?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\"))
                   char-list)
              (if upper
                  (setq char-list (append char-list upper-chars)))
              (if lower
                  (setq char-list (append char-list lower-chars)))
              (if number
                  (setq char-list (append char-list number-chars)))
              (if symbol
                  (setq char-list (append char-list symbol-chars)))
              (if ambiguous
                  (setq char-list (append char-list
                                          upper-chars-ambiguous
                                          lower-chars-ambiguous
                                          number-chars-ambiguous
                                          symbol-chars-ambiguous)))
              char-list))

          (defun make-password-prompt-for-args ()
            (interactive)
            (list
             (string-to-number (read-from-minibuffer "Number of Characters: "))
             (y-or-n-p "User uppercase: ")
             (y-or-n-p "User lowercase: ")
             (y-or-n-p "User numbers: ")
             (y-or-n-p "User symbols: ")
             (y-or-n-p "User ambiguous characters: ")))

          (defun* make-password-strength (length &optional (upper t) (lower t) (number t) (symbol nil) (ambiguous nil))
            "Calculate the number of possible passwords that could be generated
given the criteria of LENGTH and use of UPPER, LOWER, NUMBER, SYMBOL,
and AMBIGUOUS characters"
            (interactive (make-password-prompt-for-args))
            (let* ((char-list (make-password-char-list upper lower number symbol ambiguous))
                   (bits (/ (* length (log (length char-list))) (log 2)))
                   (number (/ (* bits (log 2)) (log 10))))
              (if (interactive-p)
                  (message "number of combinations is 10^%d with a bit equivalence of %d" (round number) (round bits))
                  (list bits number)))))
        )))

(defun lotus-encrypt/init-epa-file ()
  (use-package epa-file
      :defer t
      :config
      (progn
        (progn

          (require 'utils-custom)

          (defun epa-set-file-regex (prefix)
            (interactive "sprefix: ")
            (let ((regex (concat "\\." prefix "\\(~\\|\\.~[0-9]+~\\)?\\'")))
              (add-to-list 'auto-mode-alist
                           (list regex nil 'epa-file))
              (setq
               epa-file-name-regexp (regexp-or "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" regex))
              (epa-file-name-regexp-update)))



          ;; (epa-file-enable)

          (setq epa-file-cache-passphrase-for-symmetric-encryption t)

          ;; now I do not want to disable agent ....
          ;;{{ http://www.enigmacurry.com/2009/01/14/extending-emacs-with-advice/
          (defadvice epg--start (around advice-epg-disable-agent disable)
            "Make epg--start not able to find a gpg-agent"
            (let ((agent (getenv "GPG_AGENT_INFO")))
              (setenv "GPG_AGENT_INFO" nil)
              ad-do-it
              (setenv "GPG_AGENT_INFO" agent)))
          (defun epg-disable-agent ()
            "Make EasyPG bypass any gpg-agent"
            (interactive)
            (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
            (ad-activate 'epg--start)
            (message "EasyPG gpg-agent bypassed"))

          (defun epg-enable-agent ()
            "Make EasyPG use a gpg-agent after having been disabled with epg-disable-agent"
            (interactive)
            (ad-disable-advice 'epg--start 'around 'advice-epg-disable-agent)
            (ad-activate 'epg--start)
            (message "EasyPG gpg-agent re-enabled"))
          ;;}}
          )

                (progn                          ;passphrase management
          (defun forget-passphrase (&optional buffer)
            "thisandthat."
            (interactive "bbuffer: ")
            (let* ((buffer (or buffer (current-buffer)))
                   (file-name (file-truename (buffer-file-name (get-buffer buffer)))))
              (when file-name
                (setq epa-file-passphrase-alist (assq-delete-all-test file-name epa-file-passphrase-alist #'string-equal))
                (kill-buffer buffer))))

          (defalias 'epa-forget-passphrase 'forget-passphrase)

          (defvar epa-file-passphrase-suspend-cleanup nil "Stop cleanup for now.")

          (defun epa-file-passphrase-cleanup ()
            (interactive)
            (when  epa-file-passphrase-alist
              (let* ((exceptitions-alist (mapcar
                                          '(lambda (p) (cons (file-truename (car p)) (cdr p)))
                                          epa-file-passphrase-cleanup-exceptitions-alist))
                     (exceptitions (mapcar 'car exceptitions-alist)))
                (dolist (a epa-file-passphrase-alist)
                  (let* ((file-name (file-truename (car a)))
                         (buffer-of-file (find file-name (buffer-list) :key #'(lambda (f)
                                                                                (if (stringp f) (file-truename (buffer-file-name f))))
                                               :test #'string-equal)))
                    (unless (member file-name exceptitions)
                      (setq epa-file-passphrase-alist (assq-delete-all-test file-name epa-file-passphrase-alist #'string-equal))
                      (if buffer-of-file
                          (kill-buffer buffer-of-file)))))

                (dolist (buff (remove-if-not '(lambda (b)
                                               (let ((bn (buffer-file-name b)))
                                                 (if bn
                                                     (string-match ".gpg\$" bn))))
                                             (buffer-list)))
                  (let ((buff-file (file-truename (buffer-file-name buff))))
                    (unless (member buff-file exceptitions)
                      ;; (message "found %s" buff-name)
                      (kill-buffer buff)
                      ;; (message "killed %s" buff)
                      (setq epa-file-passphrase-alist
                            (assq-delete-all-test buff-file epa-file-passphrase-alist #'string-equal))))))

              (dolist (v epa-file-passphrase-cleanup-exceptitions-alist)
                (if (<= (cdr v) 0)
                    (remove-alist 'epa-file-passphrase-cleanup-exceptitions-alist (car v))
                    (decf (cdr v))))
              ;; (setq epa-file-passphrase-cleanup-exceptitions-alist exceptitions-alist)
              ))

          (defvar epa-file-passphrase-cleanup-exceptitions-alist nil "Epa file passphrase cleanup exceptitions")

          (require 'common-info)
          (defvar epa-file-passphrase-cleanup-timer nil "epa file passphrase cleanup timer")

          (setq
           epa-file-passphrase-cleanup-timer
           (run-with-idle-timer 10 t 'epa-file-passphrase-cleanup))

          (defun epa-passphrase-cleanup-suspend ()
            (interactive)
            (timer-activate epa-file-passphrase-cleanup-timer t))

          (defun epa-passphrase-cleanup-resume ()
            (interactive)
            (timer-activate epa-file-passphrase-cleanup-timer))

          (defun epa-passphrase-cleanup-start ()
            (interactive)
            (setq
             epa-file-passphrase-cleanup-timer
             (run-with-idle-timer 10 t 'epa-file-passphrase-cleanup)))

          (defun epa-passphrase-cleanup-stop ()
            (interactive)
            (cancel-timer epa-file-passphrase-cleanup-timer)
            (setq epa-file-passphrase-cleanup-timer nil))

          (epa-passphrase-cleanup-start)

          (defun epa-add-exception-for (file times)
            (interactive "Ffile: \nnCount: ")
            (let ((tfile (file-truename file)))
              (if (member tfile
                          (mapcar
                           '(lambda (f)
                             (file-truename (car f)))
                           epa-file-passphrase-cleanup-exceptitions-alist))
                  (remove-alist 'epa-file-passphrase-cleanup-exceptitions-alist file))
              (pushnew (cons file times) epa-file-passphrase-cleanup-exceptitions-alist)))

          (defun epa-remove-exception-for (file)
            (interactive "Ffile: ")
            (let ((tfile (file-truename file)))
              (if (member tfile
                          (mapcar
                           '(lambda (f)
                             (file-truename (car f)))
                           epa-file-passphrase-cleanup-exceptitions-alist))
                  (remove-alist 'epa-file-passphrase-cleanup-exceptitions-alist file))))

          (defun epa-file-passphrase-delay-cleanup (file count)
            (interactive
             (let* ((tf (file-truename (buffer-file-name (current-buffer))))
                    (dir (file-name-directory tf))
                    (fn (file-name-nondirectory tf))
                    (file (read-file-name "filename: "
                                          dir
                                          nil
                                          nil
                                          fn))
                    (count (read-number "How many times escape: " 6)))
               (list file count)))
            (if file
                (pushnew (cons (expand-file-name file) count) epa-file-passphrase-cleanup-exceptitions-alist)))

          (epa-add-exception-for "~/.authinfo.gpg" 100000)

          ;; test
          ;; (setq epa-file-passphrase-alist nil)

          ;; (assq-delete-all-test
          ;;  (concat (getenv "HOME") "/.setup-trunk/spaa.cresh")
          ;;  epa-file-passphrase-alist
          ;;  'string-equal)

          ;; (assq-delete-all-test
          ;;  (concat (getenv "HOME") "/.Private/secure.d/mail/authinfo")
          ;;  epa-file-passphrase-alist
          ;;  'string-equal)

          ;; epa-file-passphrase-alist


          ))))

(defun lotus-encrypt/init-PACKAGE ()
  (use-package PACKAGE
      :defer t
      :config
      (progn
        )))

;;; packages.el ends here
