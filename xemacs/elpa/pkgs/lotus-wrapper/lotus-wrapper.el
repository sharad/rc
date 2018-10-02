;;; lotus-wrapper.el --- rcs autosave backup

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad>
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

(provide 'lotus-wrapper)

(defvar replace-file-truename-link-cycle-counter 201)

(defun replace-file-truename (filename &optional counter prev-dirs)
  "Return the truename of FILENAME.
If FILENAME is not absolute, first expands it against `default-directory'.
The truename of a file name is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.

\(fn FILENAME)"  ;; Don't document the optional arguments.
  ;; COUNTER and PREV-DIRS are only used in recursive calls.
  ;; COUNTER can be a cons cell whose car is the count of how many
  ;; more links to chase before getting an error.
  ;; PREV-DIRS can be a cons cell whose car is an alist
  ;; of truenames we've just recently computed.
  (cond ((or (string= filename "") (string= filename "~"))
	 (setq filename (expand-file-name filename))
	 (if (string= filename "")
	     (setq filename "/")))
	((and (string= (substring filename 0 1) "~")
	      (string-match "~[^/]*/?" filename))
	 (let ((first-part
		(substring filename 0 (match-end 0)))
	       (rest (substring filename (match-end 0))))
	   (setq filename (concat (expand-file-name first-part) rest)))))

  (or counter (setq counter (list replace-file-truename-link-cycle-counter)))
  (let (done
	;; For speed, remove the ange-ftp completion handler from the list.
	;; We know it's not needed here.
	;; For even more speed, do this only on the outermost call.
	(file-name-handler-alist
	 (if prev-dirs file-name-handler-alist
	   (let ((tem (copy-sequence file-name-handler-alist)))
	     (delq (rassq 'ange-ftp-completion-hook-function tem) tem)))))
    (or prev-dirs (setq prev-dirs (list nil)))

    ;; andrewi@harlequin.co.uk - on Windows, there is an issue with
    ;; case differences being ignored by the OS, and short "8.3 DOS"
    ;; name aliases existing for all files.  (The short names are not
    ;; reported by directory-files, but can be used to refer to files.)
    ;; It seems appropriate for file-truename to resolve these issues in
    ;; the most natural way, which on Windows is to call the function
    ;; `w32-long-file-name' - this returns the exact name of a file as
    ;; it is stored on disk (expanding short name aliases with the full
    ;; name in the process).
    (if (eq system-type 'windows-nt)
	(unless (string-match "[[*?]" filename)
	  ;; If filename exists, use its long name.  If it doesn't
	  ;; exist, the recursion below on the directory of filename
	  ;; will drill down until we find a directory that exists,
	  ;; and use the long name of that, with the extra
	  ;; non-existent path components concatenated.
	  (let ((longname (w32-long-file-name filename)))
	    (if longname
		(setq filename longname)))))

    ;; If this file directly leads to a link, process that iteratively
    ;; so that we don't use lots of stack.
    (while (not done)
      (setcar counter (1- (car counter)))
      (if (< (car counter) 0)
	  (error "Apparent cycle of symbolic links for %s" filename))
      (let ((handler (find-file-name-handler filename 'file-truename)))
	;; For file name that has a special handler, call handler.
	;; This is so that ange-ftp can save time by doing a no-op.
	(if handler
	    (setq filename (funcall handler 'file-truename filename)
		  done t)
	  (let ((dir (or (file-name-directory filename) default-directory))
		target dirfile)
	    ;; Get the truename of the directory.
	    (setq dirfile (directory-file-name dir))
	    ;; If these are equal, we have the (or a) root directory.
	    (or (string= dir dirfile)
		(and (memq system-type '(windows-nt ms-dos cygwin nacl))
		     (eq (compare-strings dir 0 nil dirfile 0 nil t) t))
		;; If this is the same dir we last got the truename for,
		;; save time--don't recalculate.
		(if (assoc dir (car prev-dirs))
		    (setq dir (cdr (assoc dir (car prev-dirs))))
		  (let ((old dir)
            (new (file-name-as-directory (file-truename dirfile counter prev-dirs))))
		    (setcar prev-dirs (cons (cons old new) (car prev-dirs)))
		    (setq dir new))))
	    (if (equal ".." (file-name-nondirectory filename))
		(setq filename
		      (directory-file-name (file-name-directory (directory-file-name dir)))
		      done t)
	      (if (equal "." (file-name-nondirectory filename))
		  (setq filename (directory-file-name dir)
			done t)
		;; Put it back on the file name.
		(setq filename (concat dir (file-name-nondirectory filename)))
		;; Is the file name the name of a link?
		(setq target (file-symlink-p filename))
		(if target
		    ;; Yes => chase that link, then start all over
		    ;; since the link may point to a directory name that uses links.
		    ;; We can't safely use expand-file-name here
		    ;; since target might look like foo/../bar where foo
		    ;; is itself a link.  Instead, we handle . and .. above.
		    (setq filename
			  (if (file-name-absolute-p target)
			      target
			    (concat dir target))
			  done nil)
		  ;; No, we are done!
		  (setq done t))))))))
    filename))

;;;###autoload
(defun lotus-wrapper-insinuate ()
  (interactive)
  (add-function
   :override (symbol-function 'file-truename)
   #'replace-file-truename))

;;;###autoload
(defun lotus-wrapper-uninsinuate ()
  (interactive)
  (remove-function
   (symbol-function 'file-truename)
   #'replace-file-truename))


;; (file-truename "~/.mailbox")

;;; lotus-wrapper.el ends here
