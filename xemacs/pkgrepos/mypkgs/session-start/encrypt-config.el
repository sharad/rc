;; from: http://www.emacswiki.org/emacs/GnusEncryptedAuthInfo

(eval-after-load "epa"
    '(epa-file-disable))

(deh-require-maybe epa-file


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



(defun forget-passphrase (&optional buffer)
   "thisandthat."
   (interactive "bbuffer: ")
   (let* ((buffer (or buffer (current-buffer)))
          (file-name (file-truename (buffer-file-name (get-buffer buffer)))))
     (when file-name
       (setq epa-file-passphrase-alist (assq-delete-all-test file-name epa-file-passphrase-alist #'string-equal))
       (kill-buffer buffer))))

(defvar epa-file-passphrase-suspend-cleanup nil "Stop cleanup for now.")

(defun epa-file-passphrase-cleanup (&optional exceptitions-alist)
  (interactive)
  (let* ((exceptitions-alist (or exceptitions-alist epa-file-passphrase-cleanup-exceptitions-alist))
         (exceptitions (mapcar 'car exceptitions-alist)))
    (dolist (a epa-file-passphrase-alist)
      (let* ((file-name (car a))
             (buffer-of-file (find file-name (buffer-list) :key #'buffer-file-name :test #'string-equal)))
        (unless (member file-name
                        (mapcar
                         '(lambda (f)
                           (file-name-sans-extension
                            (file-truename f)))
                         exceptitions))
          (setq epa-file-passphrase-alist (assq-delete-all-test file-name epa-file-passphrase-alist #'string-equal))
          (if buffer-of-file
              (kill-buffer buffer-of-file)))))

    (dolist (buff (remove-if-not '(lambda (b)
                                   (let ((bn (buffer-file-name b)))
                                     (if bn
                                         (string-match ".gpg\$" bn))))
                                 (buffer-list)))
      (let ((buff-name (buffer-file-name buff)))
        (unless (member buff-name exceptitions)
          ;; (message "found %s" buff-name)
          (kill-buffer buff)
          ;; (message "killed %s" buff)
          (setq epa-file-passphrase-alist (assq-delete-all-test buff-name epa-file-passphrase-alist #'string-equal)))))
    (dolist (v exceptitions-alist)
      (if (<= (cdr v) 0)
          (remove-alist 'epa-file-passphrase-cleanup-exceptitions-alist (car v))
          (decf (cdr v))))
    (setq epa-file-passphrase-cleanup-exceptitions-alist exceptitions-alist)))

(defvar epa-file-passphrase-cleanup-exceptitions-alist nil "Epa file passphrase cleanup exceptitions")

(require 'common-info)
(defvar epa-file-passphrase-cleanup-timer nil "epa file passphrase cleanup timer")

(setq
 epa-file-passphrase-cleanup-timer
 (run-with-idle-timer 10 t 'epa-file-passphrase-cleanup epa-file-passphrase-cleanup-exceptitions-alist))

(defun epa-passphrase-cleanup-suspend ()
  (interactive)
  (timer-activate epa-file-passphrase-cleanup-timer t))

(defun epa-passphrase-cleanup-resume ()
  (interactive)
  (timer-activate epa-file-passphrase-cleanup-timer))

(pushnew '("~/.authinfo.gpg" . 100000) epa-file-passphrase-cleanup-exceptitions-alist)

(defun epa-add-exception-for (file times)
  (interactive "Ffile: \nnCount: ")
  (let ((file (file-name-sans-extension
               (file-truename file))))
    (if (member file
                (mapcar
                 '(lambda (f)
                   (file-name-sans-extension
                    (file-truename (car f))))
                 epa-file-passphrase-cleanup-exceptitions-alist))
        (remove-alist 'epa-file-passphrase-cleanup-exceptitions-alist file))
    (pushnew (cons file times) epa-file-passphrase-cleanup-exceptitions-alist)))

(defun epa-remove-exception-for (file times)
  ;; tbd
  )

;; (cancel-timer epa-file-passphrase-cleanup-timer)




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



(deh-section "make-passwd"
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

(provide 'encrypt-config)
