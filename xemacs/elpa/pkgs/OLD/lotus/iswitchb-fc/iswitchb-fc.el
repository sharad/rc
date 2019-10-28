;;; iswitchb-fc.el --- files

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad @>
;; Keywords:

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

(require 'filecache)
(require 'iswitchb)


;; iswitchb-fc: Integrate file-cache with iswitchb

;; http://tao.uab.es/cgi-bin/archzoom.cgi/jao@gnu.org--2004/unix--emacs--0--patch-23/other/iswitchb-fc.el?download

;; That site seems no longer to be available (2012-07-23). This seems to work: https://github.com/emacsmirror/iswitchb-fc/blob/master/iswitchb-fc.el – DrewAdams

;; I think iswitchb-fc is awesome!! – Anonymous

(defun iswitchb-fc-read-buffer (prompt &optional default existing)
  (save-window-excursion (buffer-name (iswitchb))))
(defadvice read-buffer (around iswitchb-fc-read-buffer)
  (setq ad-return-value (iswitchb-fc-read-buffer prompt)))

;; patch to integrate find-file – rubikitch
;; Using iswitchb to open files from file name cache -- take two

;; Filecache rules but I did not like the way it completes the file names. Iswitchb-fc above is a really cool enhancement to filecache but I did not like that it was integrated with ‘C-x b’ (I guess I want to know what files I actually have open). Hence this little hack:

(defun file-cache-iswitchb-file ()
  "Using iswitchb, interactively open file from file cache'.
First select a file, matched using iswitchb against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive)
  (let* ((file (file-cache-iswitchb-read "File: "
                                         (mapcar
                                          (lambda (x)
                                            (car x))
                                          file-cache-alist)))
         (record (assoc file file-cache-alist)))
    (find-file
     (concat
      (if (= (length record) 2)
          (car (cdr record))
          (file-cache-iswitchb-read
           (format "Find %s in dir: " file) (cdr record))) file))))

(defun file-cache-iswitchb-read (prompt choices)
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt)))

;; I bind ‘C-c f’ to it:

;; (global-set-key "\C-cf" 'file-cache-iswitchb-file)

;; Happy happy, joy joy! – MaDa



(provide 'iswitchb-fc)
;;; files-config.el ends here
