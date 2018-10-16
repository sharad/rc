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

(progn                                  ;change default directory
  (defconst *workdirectory* (expand-file-name "paradise/" "~/.."))
  (when (and
         (boundp '*workdirectory*)
         (file-directory-p *workdirectory*))
    (cd *workdirectory*)))

(progn ;; "Tramp read-only file problem"
  (defun set-file-mode-to-truename (file)
    (interactive
     (list (ido-read-file-name "file: " default-directory buffer-file-name)))
    (when (file-remote-p file)
      (let ((tfile (file-truename file)))
        (when (not (string-equal file tfile))
          (let ((fileattr (tramp-get-file-property (tramp-file-connection file) file "file-attributes-integer" 'undef))
                (tfileattr (tramp-get-file-property (tramp-file-connection tfile) tfile "file-attributes-integer" 'undef)))
            ;; (message "XXXX file: %s, fileattr: %s tfile %s, tfileattr: %s" file fileattr tfile tfileattr)
            (if (eq fileattr 'undef)
                (unless (eq tfileattr 'undef)
                  (tramp-set-file-property (tramp-file-connection tfile) tfile "file-attributes-integer" 'undef))
                (unless (string-equal (nth 8 fileattr) (nth 8 tfileattr))
                  (setf (nth 8 tfileattr) (nth 8 fileattr))
                  (tramp-set-file-property (tramp-file-connection tfile) tfile "file-attributes-integer" tfileattr))))))))


  (defadvice set-file-modes (after set-file-mode-to-truename (filename mode) activate)
    (set-file-mode-to-truename filename))

  (when nil
    (ad-disable-advice 'set-file-modes 'after 'set-file-mode-to-truename)
    (ad-update 'set-file-modes)))


(progn ;; "ff-mode"
  (defvar ff-mode-map
    (let ((map (make-sparse-keymap)))
      ;; These bindings roughly imitate those used by Outline mode.
      ;;(define-key map "\C-c@\C-c"	      'hs-toggle-hiding)
      ;;(define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
      map)
    "Keymap for hideshow minor mode.")

  (unless (and
           (boundp 'ff-mode-map)
           ff-mode-map) ; if it is not already defined
    ;; from: http://ergoemacs.org/emacs/elisp_menu_for_major_mode.html
    ;; assign command to keys
    (setq ff-mode-map (make-sparse-keymap)))

  (define-minor-mode ff-mode
      "Prepare for working with collarative office project."
    :init-value 1
    ;; :lighter " all finder"
    :global t
    :keymap ff-mode-map
    (when office-mode
      (message "calling ff mode"))))

;; (provide 'config)
;;; config.el ends here
