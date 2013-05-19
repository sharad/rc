;; $Id$

;; This module allows for 'bookmarking' of certain directories so that the
;; files they contain can be quickly opened. For instance if you have code in
;; this directory that you access often:
;; 
;;   C:\Documents and Settings\Bozo\My Documents\My Code\Bogumaton\src\main.cpp
;; 
;; You might like being able to open it as simply
;; 
;;   Bogumaton/main.cpp
;; 
;; rather than having to type the full path each time your current buffer
;; happens to be in a different CWD.
;; 
;; This module provides a function that shows an alternative prompt, in which
;; you can enter the short form of file names. That prompt provides
;; auto-complete, and when you hit enter the correct file will be opened.

;;-----------------------------------------------------------------------------
;; global vars

(defvar locations-list
  '()
  "The locations that will be searched. Contains a list of the
  data structures returned by locations-make")

;;-----------------------------------------------------------------------------
;; main data structure

;; Each location is defined as:
;; 
;;  1. A fixed base path, giving the root of this location
;; 
;;  2. A list of regular expressions used to define both which files found in
;;     the root's subtree are included, and what their short name is.
;; 
;;     Every regular expression in the list must match its respective
;;     subdirectory of the root: the 1st regex is applied to directories that
;;     are found in the base path directory itself, then the 2nd regex is
;;     applied to the contents of those directories that matched the 1st regex,
;;     etc.
;; 
;;     If any of the regexes capture something, what they capture will be made
;;     part of the short name for the matched files.

(defun locations-make (root regexes &rest opt)
  "Assembles and return a location 'object'"
  (list
   :root root
   :regexes regexes
   :rename-buffer (plist-get opt :rename-buffer)
   ))

;;-----------------------------------------------------------------------------
;; public functions

(defun locations-open ()
  "Prompts for a short file name, looks it up in a freshly
compiled lookup table, and opens the file that corresponds to
that short name."
  (interactive)

  (defun hash-to-completions-alist (hash)
    ;; Isn't there a built-in for this?  Also, why doesn't maphash return a
    ;; list of what the lambda it takes returns?
    (let ((ret '()))
      (maphash
       (lambda (key val)
         (setq ret (cons (cons key key) ret)))
       hash)
      ret))

  (let* (
         ;; Scan the file system, making a list of all files that match the
         ;; defined locations and giving each of them a "short name"
         (locations-hash (locations-expand))
         (completions-table
          (if (< emacs-major-version 23)
              (hash-to-completions-alist locations-hash)
            locations-hash))

         ;; Prompt for a short name
         (selected-short
          (completing-read
           "Location: "
           completions-table
           nil
           1))

         ;; Map it back to the full path and open it
         (selected-full (gethash selected-short locations-hash)))
      (find-file selected-full)))

(defun locations-find-file-hook ()
  "Checks whether the newly opened file matches any of the user's
locations, and if so, and if the location has :rename-buffer set
to a true value, renames the buffer to the file's short name"

  (let* ((file-name buffer-file-truename)

         ;; Scan the file system looking for a location that matches the file
         ;; that was just opened. If there is such a location, the search
         ;; function also returns its short name
         (match (locations-find-matching
                 file-name
                 (lambda (loc) (plist-get loc :rename-buffer)))))

    (if match
        (let* ((matching-loc (car match))
               (short-path (cdr match)))
          ;; If the location is configured with buffer renaming, rename it
          (if (plist-get matching-loc :rename-buffer)
              (rename-buffer short-path))))))

;;-----------------------------------------------------------------------------
;; helpers

(defun locations-walk (loc accept-path save-match)
  "Searches the file space under the
given loc's root for matching files. save-match will be called
for every match found. If it returns a true value, search stops
and that value is what we return. Else this returns nil."

  (defun walk-dir (path regexes short-path)
    ;; Recursive function that does the searching.
    ;; 
    ;; 'path' is the directory we're scanning (the root on 1st invocation, then
    ;; subdirectories, then files), 'regexes' is the list of regular
    ;; expressions we're consuming, and 'short-path' is the (reverse) list of
    ;; matching short name elements we're building.
    ;; 
    ;; save-match will be called for every match found. If it returns a true
    ;; value, search stops and that value is what we return
    ;; 
    ;; Serves as the basis for locations-expand and locations-find-matching

    ;; We're walking down the directory tree and the regex list in
    ;; parallel. If we still have regexes it means we expect to go
    ;; deeper still.
    (if regexes
        (let* ((regex (car regexes))
               (children (remove-if
                          (lambda (child) (string-match "^\\." child))
                          (directory-files path)))
               ;; If `retval' is every set to a true value, we stop
               ;; searching completely. It is ultimately set by the
               ;; `save-match' callback
               (retval nil))

          ;; The '* can match 0 directories, so if it's there, try to
          ;; search the same dir with the rest of the regexes
          (when (eq regex '*)
            (setq retval (walk-dir path (cdr regexes) short-path)))

          ;; Inspect all children in this directory. Written in
          ;; imperative style because Emacs Lisp doesnt support tail
          ;; recursion (http://www.cliki.net/elisp)
          (while (and children (not retval))
            (let* ((child (car children))
                   (child-path (concat path "/" child)))
              (if (apply accept-path (list child-path))
                  (if (eq regex '*)
                      (setq retval (walk-dir child-path
                                             (cdr regexes)
                                             (cons child short-path)))
                    (if (string-match regex child)
                        (let* ((match (match-data))
                               (short-path
                                (if (> (length match) 2)
                                    (cons (substring child
                                                     (nth 2 match)
                                                     (nth 3 match))
                                          short-path)
                                  short-path)))
                          (setq retval (walk-dir child-path
                                                 (cdr regexes)
                                                 short-path)))))))
            (setq children (cdr children)))
          retval)

      ;; if there are no more regexes and `path' is a file, then we've
      ;; found a match
      (if (and short-path
               (not (file-directory-p path)))
          (let ((short-path (mapconcat 'concat (reverse short-path) "/")))
            (if (eq system-type 'windows-nt)
                (setq short-path (downcase short-path)))
            ;; TEACHME Why do I need "apply" here?
            (apply save-match (list path short-path)))
        nil)))

  (if (apply accept-path (list (plist-get loc :root)))
      (let ((max-lisp-eval-depth 10000))
        (walk-dir
         (plist-get loc :root)
         (plist-get loc :regexes)
         '()))))

(defun locations-expand ()
  "Compiles the current locations-list into a hash that maps
short names to full paths. This is not cached as we want the
results to reflect whatever's in the file system whenever the
function is called."

  (define-hash-table-test 'contents-hash 'string= 'sxhash)
  (let ((hash (make-hash-table :test 'contents-hash)))
    (dolist (loc locations-list hash)
      (locations-walk
       loc
       (lambda (path) 1)
       (lambda (path short-path)
         (puthash short-path path hash)
         nil)))))

(defun locations-find-matching (file-name accept-location)
  "Returns a cons containing the location object that matches the
given file name, and the short path given by that loc to that
file. Returns nil if no match found"

  (defun begins-with-ignore-case (str prefix)
    ;; FIXME isn't there a built-in for this?
    (and (<= (length prefix) (length str))
         (string= (downcase (substring str 0 (length prefix)))
                  (downcase prefix))))

  (defun do-locations (locations)
    (if locations
        (let ((loc (car locations)))
          (or (and (apply accept-location (list loc))
                   (locations-walk
                    loc

                    ;; Instruct the filesystem-walking function only to
                    ;; consider paths that match the name of the file that was
                    ;; just opened. Results are the same, but this avoids
                    ;; wasting CPU cycles.
                    (lambda (path) (string-begins-with file-name path))

                    (lambda (path short-path)
                      (if (string= (downcase path) (downcase file-name))
                          (cons loc short-path)))))
              (do-locations (cdr locations))))))

  (do-locations locations-list))

;;-----------------------------------------------------------------------------
