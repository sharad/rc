;;; citation-config.el --- citation from X Steve

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@arubanetworks.com>
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


(require 'nnheader)

;;{{ XSteve, insert Hi Hello Name
;; The message-citation-line-function is responsible to display a
;; message citation. The following Code allows to switch

;; (setq message-citation-line-function #'(lambda () ; was message-insert-citation-line
;;                                          (message-insert-formatted-citation-line) ; put wrote:
;;                                          (message-goto-body)
;;                                          (xsteve-message-citation))) ;put hi

(setq message-citation-line-function 'message-insert-formatted-citation-line
      message-cite-function 'message-cite-original-without-signature)
;; (add-hook 'gnus-message-setup-hook 'xsteve-message-citation t)
(add-hook 'message-setup-hook 'xsteve-message-citation t)
;; (setq message-cite-function 'sc-cite-original)
;;

;; message-setup-hook

;; (defun gettoto ()
;;   (interactive)
;;   (message message-reply-headers))

(defun sharad-message-citation-delete ()
  "Delete Hi."
  (message-goto-body)
  (search-forward-regexp "Hi")
  (move-beginning-of-line 1)
  (if (looking-at "Hi")
      (kill-line)))

(deh-require-maybe gnus-junk

  (defun get-proper-citation-name (email name)
    "Get proper name."
    ;; name can be null for "xxxx@xxxxxx.xxx"
    ;; but email will be there.

    ;; in other case like "Xxxxx Xxxxx <xxxx@xxxxxx.xxx>"
    ;; name and email both will be there.
    (let ((first-name-in-email
           (if (string-match "^\\(\\w\+\\)" email)
               (match-string 0 email)))
          (first-name-in-name
           (if name (car (split-string name)))))

      (if (and
           first-name-in-name
           (string-caseless-equal first-name-in-email first-name-in-name))
          first-name-in-name
          (capitalize first-name-in-email)))))


(defun xsteve-message-citation ()
  (interactive)
  (when message-reply-headers
    (xsteve-message-citation-delete)
    (message-goto-body)
    (let* ((from-address
            (mail-header-parse-address (mail-header-from message-reply-headers)))
           (parsed-address
            (if (member (car from-address)
                        message-dont-reply-to-names)
                (mail-header-parse-address
                 (car
                  (remove-if (lambda (s)
                               (string-match "^\s*$" s))
                             (split-string (message-fetch-field "to") "[,;]") )))
              from-address)))
      (if (functionp 'bbdb-search-simple)
          (let ((my-bbdb-record (bbdb-search-simple (cdr parsed-address) (car parsed-address)))
                (start-pos (point))
                following-text
                (following-newlines 2)
                (overlay)
                (anrede (when my-bbdb-record (bbdb-record-getprop my-bbdb-record 'anrede)))
                (first-name (funcall 'get-proper-citation-name (car parsed-address) (cdr parsed-address)))
                (name-to-use
                 (or (if my-bbdb-record
                         (bbdb-record-name my-bbdb-record)
                       first-name)
                     "Sharad Pratap")))
            (progn
              (if anrede
                  (insert (format "%s\n\n" anrede))
                (funcall xsteve-message-citation-function first-name))
              (if following-text (insert following-text))
              (when following-newlines
                (dotimes (v following-newlines)
                  (insert "\n"))
                (forward-line (- following-newlines 1))))
            (unless (eq start-pos (point))
              (setq overlay (make-overlay start-pos (point)))
              (overlay-put overlay 'xsteve-message-citation nil)))
        (message "bbdb3 not have #'bbdb-search-simple use #'bbdb-search-mail from bbdb3 for name %s address %s"
                 (cdr parsed-address)
                 (car parsed-address))))))

(defun xsteve-message-citation-hallo (name)
  (insert "Hallo " name "!"))

(defun xsteve-message-citation-hi (name)
  (insert "Hi " name "!"))

(defun xsteve-message-citation-herr (name)
  (insert "Hallo Herr " (or name "Fred Namenlos ") "!"))

(defun xsteve-message-citation-default (name)
  (message-insert-citation-line))

;; correct it
;; (xsteve-define-alternatives 'xsteve-message-citation-function '(xsteve-message-citation-hallo
;;                                                                 xsteve-message-citation-herr
;;                                                                 xsteve-message-citation-hi
;;                                                                 xsteve-message-citation-default))

(setq xsteve-message-citation-function
      'xsteve-message-citation-hi)

(defun xsteve-message-citation-delete ()
  (interactive)                         ;http://www.gnu.org/s/emacs/manual/html_node/elisp/Overlays.html#Overlays
  (let ((overlay)
        (start-pos))
    (goto-char (point-min))
    (goto-char (next-overlay-change (point)))
    (setq overlay (car-safe (overlays-at (point)))) ;; do not use car...
    (when overlay
      (overlay-get overlay 'xsteve-message-citation)
      (setq start-pos (point))
      (goto-char (next-overlay-change (point)))
      (delete-region start-pos (point)))))

(defun xsteve-message-citation-toggle ()
  (interactive)
  (save-excursion
    ;; (toggle-xsteve-message-citation-function)  ;; implement it
    (xsteve-message-citation)))

(define-key message-mode-map [f6] 'xsteve-message-citation-toggle)
;;}}



(provide 'citation-config)
;;; citation.el ends here
