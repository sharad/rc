;;; files-config.el --- files

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

(require 'find-file-config)

(deh-require-maybe find-dired
  )

(deh-require-maybe find-file-in-project
  ;; If non-nil, this function is called to determine the project root.
  (setq
   ;; ffip-project-root-function nil
   ffip-project-root-function (lambda () (let ((dir (ido-read-directory-name "dir: " default-directory)))
                                           (message "dir: %s" dir)
                                           dir))
   ;; define suitable functon for it.
   ;; ffip-project-root "~/"
   ffip-project-root nil
   ffip-patterns (append '("*.cpp" "*.h" "*.c") ffip-patterns))

  (defun ffip-set-project-root ()
    (interactive)
    (setq
     ffip-project-root (ido-read-directory-name "FFip Root Dir: " ffip-project-root)))


  (defun ffip-project-files ()          ;make it for tramp files
    "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
    (let ((file-alist nil)
          (root (expand-file-name (or ffip-project-root (ffip-project-root)
                                      (error "No project root found")))))
      (mapcar (lambda (file)
                (if ffip-full-paths
                    (cons (substring (expand-file-name file) (length root))
                          (expand-file-name file))
                    (let ((file-cons (cons (file-name-nondirectory file)
                                           (expand-file-name file))))
                      (when (assoc (car file-cons) file-alist)
                        (ffip-uniqueify (assoc (car file-cons) file-alist))
                        (ffip-uniqueify file-cons))
                      (add-to-list 'file-alist file-cons)
                      file-cons)))
              (split-string (shell-command-to-string
                             (format "find %s -type f \\( %s \\) %s | head -n %s"
                                     root (ffip-join-patterns)
                                     ffip-find-options ffip-limit)))))))

(deh-require-maybe lusty-explorer

  (find-file-wizard-add
   "lusty"
   ;; ido-find-file
   (lambda (initstr)
     (setq minibuffer-history (delete 'fallback-wizard minibuffer-history))
     (lusty-file-explorer))

   (lambda (ff initial-string)
     (let ((lusty-setup-hook
            (cons
             (lambda ()
               (define-key lusty-mode-map
                   (kbd "s-f") ;; (plist-get plist :key)
                 (lambda (arg)
                   (interactive "P")
                   (setq initial-string ido-text
                         ;; ido-text 'fallback-wizard
                         ;; ido-exit 'done
                         )
                   ;; (exit-minibuffer)
                   (throw 'nextff (list 'next (list (cons :initial-string initial-string)))))))
             lusty-setup-hook)))
       (funcall ff initial-string)))))

(deh-require-maybe ff-paths
  (ff-paths-install))

(when nil
;; (deh-section "File no writable problem"
  ;; not solved the problems. code now more complex in files.el
  (make-variable-buffer-local
   (defvar my-override-mode-on-save nil
     "Can be set to automatically ignore read-only mode of a file when saving."))

  (defadvice file-writable-p (around my-overide-file-writeable-p act)
    "override file-writable-p if `my-override-mode-on-save' is set."
    (setq ad-return-value (or
                           my-override-mode-on-save
                           ad-do-it)))

  (defun sharad/override-toggle-read-only ()
    "Toggle buffer's read-only status, keeping `my-override-mode-on-save' in sync."
    (interactive)
    (setq my-override-mode-on-save (not my-override-mode-on-save))
    (toggle-read-only))

  (ad-disable-advice 'file-writable-p 'around 'my-overide-file-writeable-p)
  (ad-remove-advice 'file-writable-p 'around 'my-overide-file-writeable-p)
  (ad-update 'file-writable-p)
  )


(deh-require-maybe filecache



  ;; {{ http://emacswiki.org/emacs/FileNameCache#toc2
  (defun file-cache-save-cache-to-file (file)
    "Save contents of `file-cache-alist' to FILE.
For later retrieval using `file-cache-read-cache-from-file'"
    (interactive "FFile: ")
    (with-temp-file (expand-file-name file)
      (prin1 file-cache-alist (current-buffer))))

  ;;   (defun file-cache-read-cache-from-file (file)
  ;;     "Clear `file-cache-alist' and read cache from FILE.
  ;; The file cache can be saved to a file using
  ;; `file-cache-save-cache-to-file'."
  ;;     (interactive "fFile: ")
  ;;     (file-cache-clear-cache)
  ;;     (let ((buf (find-file-noselect file)))
  ;;       (setq file-cache-alist (read buf))
  ;;       (kill-buffer buf)))

  (defun file-cache-read-cache-from-file (file)
    "Clear `file-cache-alist' and read cache from FILE.
  The file cache can be saved to a file using
  `file-cache-save-cache-to-file'."
    (interactive "fFile: ")
    (file-cache-clear-cache)
    (save-excursion
      (set-buffer (find-file-noselect file))
      (beginning-of-buffer)
      (setq file-cache-alist (read (current-buffer)))))

  (add-hook 'after-init-hook '(lambda ()
                               (file-cache-read-cache-from-file "~/.file_cache")))

  (add-hook 'kill-emacs-hook '(lambda ()
                               (file-cache-save-cache-to-file "~/.file_cache")))

  ;;}}


  ;; http://emacswiki.org/emacs/FileNameCache
  (defun file-cache-add-this-file ()
    (and buffer-file-name
         (file-exists-p buffer-file-name)
         (file-cache-add-file buffer-file-name)))
  (add-hook 'find-file-hook 'file-cache-add-this-file)


  ;; (file-cache-read-cache-from-file "~/.file_cache")

  (defun file-cache-ido-find-file (file)
    "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
    (interactive (list (file-cache-ido-read "File: "
                                            (mapcar car file-cache-alist))))
    (let* ((record (assoc file file-cache-alist)))
      (find-file
       (expand-file-name
        file
        (if (= (length record) 2)
            (car (cdr record))
            (file-cache-ido-read
             (format "Find %s in dir: " file) (cdr record)))))))

  (defun file-cache-ido-read (prompt choices)
    (let ((ido-make-buffer-list-hook
           (lambda ()
             (setq ido-temp-list choices))))
      (ido-read-buffer prompt)))


  (defun jcl-file-cache-ido-find-file ()
    "Open a file from the file cache.
First select a file from `file-cache-alist'.  If the file exist
in more than one directory one is asked to select which to open.
If you find out that the desired file is not present in the file
cache then you may want to fallback to normal ido find file with
C-f.
Bind this command to C-x C-f to get:

 C-x C-f         -> Open file in filecache.
 C-x C-f C-f     -> Open file with normal ido.
 C-x C-f C-f C-f -> Open file with vanilla find-file.
"
    (interactive)
    (let* (jcl-ido-text
           (file (let ((ido-setup-hook (cons (lambda ()
                                               (define-key ido-completion-map [(control ?f)]
                                                 (lambda (arg)
                                                   (interactive "P")
                                                   (if jcl-ido-text
                                                       (ido-magic-forward-char arg)
                                                       (setq jcl-ido-text ido-text
                                                             ido-text 'fallback-from-cache
                                                             ido-exit 'done)
                                                       (exit-minibuffer)))))
                                             ido-setup-hook)))
                   (ido-completing-read "Cached File: "
                                        (mapcar 'car file-cache-alist)))))
      (if (eq file 'fallback-from-cache)
          (progn
            (setq minibuffer-history (delete 'fallback-from-cache minibuffer-history))
            (ido-file-internal ido-default-file-method
                               nil
                               nil
                               "Ido Find File: "
                               nil
                               jcl-ido-text))
          (let ((record (assoc file file-cache-alist)))
            (find-file
             (expand-file-name
              file
              (if (= (length record) 2)
                  (cadr record)
                  (ido-completing-read (format "Find %s in dir: " file)
                                       (cdr record)
                                       nil
                                       t))))))))

  )

(deh-require-maybe iswitchb-fc)

(deh-require-maybe (and filecache iswitchb)
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
  )

(deh-section "Find file in other dirs."
  (defun find-file-in-other-dir (&optional not-same-location)
    (interactive)
    (let ((dirs
           (remove* (file-name-directory buffer-file-name)
                    (cdr (assoc-string (file-name-nondirectory buffer-file-name) file-cache-alist))
                    :test #'string-equal)))
      (if dirs (find-file-existing (concat (ido-completing-read "dirs: " dirs) "/" (file-name-nondirectory buffer-file-name)))))))


(eval-after-load "ifind-mode"
  '(progn
    (defvar workspace-dir nil)
    (setq workspace-dir nil)))


(deh-section "ff-mode"
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


(defun dirname-of-file (file &optional final-slash)
  ;; (ido-no-final-slash
  (if final-slash
      (expand-file-name
       (file-name-directory file))
      (directory-file-name
       (expand-file-name
        (file-name-directory file)))))


(defun dir-final-slash (dir &optional noerror)
  (if dir
      (expand-file-name (concat dir "/"))
      (unless noerror
          (error "dir is nil"))))

;; (defun dirname-of-file (dir)
;;   (concat
;;    (directory-file-name
;;    (if (file-directory-p dir)
;;        dir
;;        (file-name-directory dir)))
;;    "/"))

(defun call-times (count fn arg)
  (if (eq count 0)
      arg
      (call-times (1- count)
                  fn (funcall fn arg))))

;; (dirname-of-file "/")
;; (call-times 0 'dirname-of-file "/www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html")
;; (call-times 1 'dirname-of-file buffer-file-name)
;; (call-times 2 'dirname-of-file "/")


(defun get-subdirs (dir pos filename)
  (let* ((tpos 0)
         (epos 0)
         (spos (dotimes (c (1+ (cdr dirnu)) tpos)
                 (string-match (car dirnu) filename tpos)
                 (setq tpos (match-beginning 0)
                       epos (match-end 0))))
         (prefix-filename (substring filename 0 spos))
         (suffix-filename (substring filename epos))
         (subdirs (remove
                   (car dirnu)
                   (remove-if-not
                    #'(lambda (d)
                        (and
                         (file-directory-p (concat prefix-filename d))
                         (not
                          (or
                           (string-equal (concat  ".") d)
                           (string-equal (concat  "..") d)))))
                    (directory-files prefix-filename))))
         (existing-subdirs (remove-if-not
                            #'(lambda (sd)
                                (file-exists-p (concat prefix-filename sd suffix-filename)))
                            subdirs)))
    (message "existing-subdirs %s" existing-subdirs)
    (list existing-subdirs prefix-filename suffix-filename)))

(defun find-same-file-in-relative-dir (&optional updircount)
  (interactive "P")
  (if buffer-file-name
    (let* ((filename (call-times (cond
                                   ((and (consp updircount) (eq (car wprif) 4)) 1)
                                   ((null updircount) 0)
                                   (t (prefix-numeric-value updircount)))
                                 'dirname-of-file buffer-file-name))
           (dircomponents
            (let ((count 0))
              (mapcar
               (lambda (c)
                 (prog1
                     (cons c count)
                   (incf count)))
               (split-string (if (file-directory-p filename)
                                 filename
                                 (dirname-of-file filename)) "/" t))))
           (matchd (ido-completing-read "dir: " (mapcar #'car dircomponents)))
           (matcheddircomponents
            (remove-if-not
             #'(lambda (e)
                 (string-equal (car e) matchd))
             dircomponents))
           (dirnu
            (if (= (length matcheddircomponents) 1)
                (car matcheddircomponents)
                (rassoc
                 (ido-completing-read "which one: " (mapcar #'cdr matcheddircomponents))
                 matcheddircomponents))))
      (let* ((results (get-subdirs (car dirnu) (cdr dirnu) filename))
             (existing-subdirs (car results))
             (prefix-filename (nth 1 results))
             (suffix-filename (nth 2 results))
             (select-subdir (if existing-subdirs
                                (ido-completing-read "select subdir: " existing-subdirs)))
             (selected-file-name (if select-subdir
                                     (concat prefix-filename select-subdir suffix-filename))))
        ;; (testing
        ;;  (message "%s %s %s %s %s %s %s" tpos spos dirnu prefix-filename suffix-filename subdirs existing-subdirs))
        (if selected-file-name
            (progn
              (if (file-directory-p selected-file-name)
                  (let ((default-directory selected-file-name))
                    (call-interactively
                     (or (command-remapping 'find-file)
                         'find-file)))
                  (find-file selected-file-name))
              t)
            (message "No match present."))))
    (message "buffer <%s> is not associate to any file." (current-buffer))))





(defun find-truefile (&optional file)
  "Useful in case of when vc-follow-symlink is nil
to do VC operation."
  (interactive)
  (if buffer-file-name
      (if (not (string-equal buffer-file-name (file-truename buffer-file-name)))
          (find-alternate-file (file-truename buffer-file-name))
          (message "file %s is true file, not doing anything." buffer-file-name))
      (message "No file is associated with buffer.")))





(defun testpa (c x)
  (interactive "PP")
  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Command-Arguments.html
  (message "prefix c %d"
           (if c
               (prefix-numeric-value c)
               0)))



(deh-section "Buffer with Multiple files"

  (defvar buffer-linked-files nil "list of buffer-linked-files")
  (make-local-variable 'buffer-linked-files)

  (defun copy-all-file ()
    (if buffer-linked-files
        (dolist (f buffer-linked-files)
          ;; (write-region nil nil f nil)
          (unless (file-exists-p (file-name-directory f))
            (make-directory (file-name-directory f) t))
          (copy-file (buffer-file-name (current-buffer)) f)
          (message "copied %s to %s" (file-name-nondirectory buffer-file-name) f))))


  (defun add-linked-file (tfile)
    (interactive "Flink file: ")
    (if buffer-file-name
        (let* ((srcfilename (file-name-nondirectory buffer-file-name))
               (file (file-truename tfile))
               (file   (if (file-exists-p file)
                           (if (file-directory-p file)
                               (concat (dir-final-slash file) srcfilename)
                               file)
                           (if (= (aref file (1- (length file))) ?\/)
                               (concat (dir-final-slash file) srcfilename)
                               file))))
          (if (string-equal (file-truename buffer-file-name) (file-truename file))
              (error "backup can not be same."))
          (pushnew file buffer-linked-files))
        (message "buffer is not associated with any file.")))

  (add-hook 'after-save-hook 'copy-all-file))




(provide 'files-config)
;;; files-config.el ends here
