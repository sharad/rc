;;; wordcloud.el --- Generate a word cloud -*- lexical-binding: t -*-
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.00
;; Keywords: games
;; URL: https://github.com/davep/wordcloud.el
;; Package-Requires: ((cl-lib "0.5") (emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; wordcloud.el generates a simple word cloud in a buffer.

;;; Code:

(require 'cl-lib)

(defgroup wordcloud nil
  "Simple word cloud generator."
  :group 'games)

(defface wordcloud-face-0
  '((t :height 0.9))
  "Face to use in the word cloud. Faces run from
`wordcloud-face-0' (the smallest) to `wordcloud-face-9' (the
biggest)."
  :group 'wordcloud)

(defface wordcloud-face-1
  '((t :height 0.92))
  "Face to use in the word cloud. Faces run from
`wordcloud-face-0' (the smallest) to `wordcloud-face-9' (the
biggest)."
  :group 'wordcloud)

(defface wordcloud-face-2
  '((t :height 0.94))
  "Face to use in the word cloud. Faces run from
`wordcloud-face-0' (the smallest) to `wordcloud-face-9' (the
biggest)."
  :group 'wordcloud)

(defface wordcloud-face-3
  '((t :height 0.96))
  "Face to use in the word cloud. Faces run from
`wordcloud-face-0' (the smallest) to `wordcloud-face-9' (the
biggest)."
  :group 'wordcloud)

(defface wordcloud-face-4
  '((t :height 0.98))
  "Face to use in the word cloud. Faces run from
`wordcloud-face-0' (the smallest) to `wordcloud-face-9' (the
biggest)."
  :group 'wordcloud)

(defface wordcloud-face-5
  '((t :height 1.0))
  "Face to use in the word cloud. Faces run from
`wordcloud-face-0' (the smallest) to `wordcloud-face-9' (the
biggest)."
  :group 'wordcloud)

(defface wordcloud-face-6
  '((t :height 1.4))
  "Face to use in the word cloud. Faces run from
`wordcloud-face-0' (the smallest) to `wordcloud-face-9' (the
biggest)."
  :group 'wordcloud)

(defface wordcloud-face-7
  '((t :height 1.8))
  "Face to use in the word cloud. Faces run from
`wordcloud-face-0' (the smallest) to `wordcloud-face-9' (the
biggest)."
  :group 'wordcloud)

(defface wordcloud-face-8
  '((t :height 2.2))
  "Face to use in the word cloud. Faces run from
`wordcloud-face-0' (the smallest) to `wordcloud-face-9' (the
biggest)."
  :group 'wordcloud)

(defface wordcloud-face-9
  '((t :height 2.5))
  "Face to use in the word cloud. Faces run from
`wordcloud-face-0' (the smallest) to `wordcloud-face-9' (the
biggest)."
  :group 'wordcloud)

(defcustom wordcloud-min-word-length 4
  "Minimum length of a word to include in the cloud."
  :type 'integer
  :group 'wordcloud)

(defun wordcloud-get-word-frequency-hash ()
  "Get a hash of word frequency counts."
  (save-excursion
    (setf (point) (point-min))
    (cl-loop with words = (make-hash-table :test #'equal)
             while (re-search-forward "\\w+" nil t)
             if (>= (length (match-string 0)) wordcloud-min-word-length)
             do (cl-incf (gethash (downcase (match-string 0)) words 0))
             finally return words)))

(defun wordcloud-get-word-frequency-list ()
  "Get the word count for the current buffer as a list."
  (let ((freq (wordcloud-get-word-frequency-hash)))
    (cl-loop for word being the hash-key of freq
             collect (cons word (gethash word freq)))))

(defun wordcloud-get-word-frequency-list-descending ()
  "Get the word count for the current buffer as a list.

The list is sorted in descending frequency order."
  (sort (wordcloud-get-word-frequency-list)
        (lambda (word1 word2)
          (> (cdr word1) (cdr word2)))))

(defun wordcloud-get-word-frequency-list-alpha ()
  "Get the word count for the current buffer as a list.

The list is sorted in alphabetic order."
  (sort (wordcloud-get-word-frequency-list)
        (lambda (word1 word2)
          (string< (car word1) (car word2)))))

(defun wordcloud-compress (words)
  "\"Compress\" the counts.

This makes a list of words, from WORDS, with their frequency and
a value related to the frequency that is compressed into the font
range."
  (let* ((counts (mapcar #'cdr words))
         (min    (apply #'min counts))
         (max    (apply #'max counts))
         (dist   (/ (- max min) (1- 10.0))))
    (mapcar (lambda (word)
              (cons (car word) (cons (cdr word) (/ (cdr word) dist))))
            words)))

(defun wordcloud (by-frequency)
  "Show a word cloud for the current buffer."
  (interactive "P")
  (let ((words (if by-frequency
                   (wordcloud-get-word-frequency-list-descending)
                 (wordcloud-get-word-frequency-list-alpha))))
    (with-help-window "*wordcloud*"
      (with-current-buffer standard-output
        (visual-line-mode 1)
        (cl-loop for word in (wordcloud-compress words)
                 do (insert
                     (propertize (format "%s(%d) "
                                         (car word)
                                         (cadr word))
                                 'font-lock-face (intern (format "wordcloud-face-%d" (cddr word))))))))))

(provide 'wordcloud)

;;; wordcloud.el ends here
