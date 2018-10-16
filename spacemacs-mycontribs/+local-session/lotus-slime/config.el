;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

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



(defvar use-slime-config 'ubuntu "Config")

(defvar quicklisp-path
  (first (remove-if-not
          #'file-directory-p
          '("/all/res/share/common-lisp/quicklisp"
            "/atlantic/opt/share/common-lisp/quicklisp"
            "/atlantic/home/s/res/share/common-lisp/quicklisp"
            )))
  "Quicklisp path.")

(defvar
 available-slime-configs
 `(
   ,(if (and
         quicklisp-path
         (file-directory-p quicklisp-path))
        `(quicklisp .
                    ((slime-path . ,(expand-file-name
                                     (concat (car (last (directory-files
                                                         (concat quicklisp-path "/dists/quicklisp/software" ) t "slime[a-zA-Z0-9-]+cvs"))) "/")))
                     (slime-backend . "swank-loader.lisp"))))
    (ubuntu .
           ((slime-path . ,(expand-file-name
                            (concat "/usr/share/"
                                    (if (boundp 'flavor) (symbol-name flavor)
                                        "emacs")
                                    "/site-lisp/slime")))
            (slime-backend . "/usr/share/common-lisp/source/slime/swank-loader.lisp")))



    ;; "/usr/share/emacs/site-lisp/elpa-src/slime-2.20/slime.el"
    )
  "Available")

(setf use-slime-config 'ubuntu)

;; (testing
;;  (setq b 1)
;;  (assoc 'a `(nil (a . ,b))))

(defun get-slime-config-ele-from-configs (config ele)
  "Get slime config."
  (cdr (assoc ele (cdr (assoc config available-slime-configs)))))

(defun get-slime-config (ele)
  "Get slime confiog."
  (get-slime-config-ele-from-configs use-slime-config ele))

(defun display-slime-config ()
  "Display"
  (interactive)
  (dolist (v (list slime-path slime-backend)
           (format "%s: %s" (symbol-name v) v))))


(defun load-slime ()
  (interactive)
  (load-file (concat
              (if (file-directory-p  (get-slime-config 'slime-path))
                  (get-slime-config 'slime-path)
                  "/usr/share/emacs/site-lisp/elpa-src/slime-2.20")
              "/slime.el")))

(load-slime)

;;; config.el ends here
