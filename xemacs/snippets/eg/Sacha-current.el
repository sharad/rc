;; This sets up the load path so that we can override it
(package-initialize)
(add-to-list 'load-path "~/code/org2blog")
(add-to-list 'load-path "~/Dropbox/2014/presentations/org-reveal")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(setq user-full-name "Sacha Chua"
      user-mail-address "sacha@sachachua.com")

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

(add-to-list 'load-path "~/elisp")
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(load "~/.emacs.secrets" t)

(use-package dash)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(tool-bar-mode -1)

(use-package winner
  :defer t)

(setq sentence-end-double-space nil)

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(defvar my/book-notes-directory "~/Dropbox/books")
(defun my/helm-do-grep-book-notes ()
  "Search my book notes."
  (interactive)
  (helm-do-grep-1 (list my/book-notes-directory)))

(ert-deftest my/org-capture-prefill-template ()
  (should
   ;; It should fill things in one field at ia time
   (string=
    (my/org-capture-prefill-template
     "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
     "Hello World")
    "* TODO Hello World\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
    ))
  (should
   (string=
    (my/org-capture-prefill-template
     "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
     "Hello World" "<2015-01-01>")
    "* TODO Hello World\nSCHEDULED: <2015-01-01>\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"))
  (should
   (string=
    (my/org-capture-prefill-template
     "* TODO %^{Task}\nSCHEDULED: %^t\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n%?\n"
     "Hello World" "<2015-01-01>" "0:05")
    "* TODO Hello World\nSCHEDULED: <2015-01-01>\n:PROPERTIES:\n:Effort: 0:05\n:END:\n%?\n")))

(defun my/org-capture-prefill-template (template &rest values)
  "Pre-fill TEMPLATE with VALUES."
  (setq template (or template (org-capture-get :template)))
  (with-temp-buffer
    (insert template)
    (goto-char (point-min))
    (while (re-search-forward
            (concat "%\\("
                    "\\[\\(.+\\)\\]\\|"
                    "<\\([^>\n]+\\)>\\|"
                    "\\([tTuUaliAcxkKInfF]\\)\\|"
                    "\\(:[-a-zA-Z]+\\)\\|"
                    "\\^\\({\\([^}]*\\)}\\)"
                    "?\\([gGtTuUCLp]\\)?\\|"
                    "%\\\\\\([1-9][0-9]*\\)"
                    "\\)") nil t)
      (if (car values)
          (replace-match (car values) nil t))
      (setq values (cdr values)))
    (buffer-string)))

(defun my/org-get-current-refile-location ()
  "Return the current entry as a location understood by org-refile."
  (interactive)
  (list (elt (org-heading-components) 4)
        (or buffer-file-name
            (with-current-buffer (buffer-base-buffer (current-buffer))
              buffer-file-name))
        nil
        (point)))

(defun my/helm-org-create-task (candidate)
  "Creates the task and returns the location."
  (let ((entry (org-capture-select-template "T")))
    (org-capture-set-plist entry)
    (org-capture-get-template)
    (org-capture-set-target-location)
    (condition-case error
        (progn
          (org-capture-put
           :template
           (org-capture-fill-template
            (my/org-capture-prefill-template (org-capture-get :template)
                                             candidate)))
          (org-capture-place-template
           (equal (car (org-capture-get :target)) 'function))
          (setq org-refile-target-table (org-refile-get-targets))
          ;; Return the new location
          (my/org-get-current-refile-location))
      ((error quit)
       (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
       (error "Capture abort: %s" error)))))

;; (my/org-refile-get-location-by-substring "Try again")

(defvar my/helm-org-refile-locations nil)
  (defvar my/org-refile-last-location nil)

  (defun my/helm-org-clock-in-and-track-from-refile (candidate)
    (let ((location (org-refile--get-location candidate my/helm-org-refile-locations)))
      (save-window-excursion
        (org-refile 4 nil location)
        (my/org-clock-in-and-track)
        t)))

(defun my/org-get-todays-items-as-refile-candidates ()
  "Return items scheduled for today, ready for choosing during refiling."
  (delq
   nil
   (mapcar
    (lambda (s)
      (if (get-text-property 0 'org-marker s)
          (list
           s
           (buffer-file-name (marker-buffer (get-text-property 0 'org-marker s)))
           nil
           (marker-position (get-text-property 0 'org-marker s)))))
    (save-window-excursion (my/org-get-entries-fn (calendar-current-date) (calendar-current-date))))))

;; Based on http://emacs.stackexchange.com/questions/4063/how-to-get-the-raw-data-for-an-org-mode-agenda-without-an-agenda-view
(defun my/org-get-entries-fn (begin end)
"Return org schedule items between BEGIN and END.
USAGE:  (org-get-entries-fn '(6 1 2015) '(6 30 2015))"
  (require 'calendar)
  (require 'org)
  (require 'org-agenda)
  (require 'cl)
  (unless
      (and
        (calendar-date-is-valid-p begin)
        (calendar-date-is-valid-p end))
    (let ((debug-on-quit nil))
      (signal 'quit `("One or both of your gregorian dates are invalid."))))
  (let* (
      result
      (org-agenda-prefix-format "  • ")
      (org-agenda-entry-types '(:scheduled))
      (date-after
        (lambda (date num)
          "Return the date after NUM days from DATE."
          (calendar-gregorian-from-absolute
           (+ (calendar-absolute-from-gregorian date) num))))
      (enumerate-days
        (lambda (begin end)
          "Enumerate date objects between BEGIN and END."
          (when (> (calendar-absolute-from-gregorian begin)
                   (calendar-absolute-from-gregorian end))
            (error "Invalid period : %S - %S" begin end))
          (let ((d begin) ret (cont t))
            (while cont
              (push (copy-sequence d) ret)
              (setq cont (not (equal d end)))
              (setq d (funcall date-after d 1)))
            (nreverse ret)))) )
    (org-agenda-reset-markers)
    (setq org-agenda-buffer
      (when (buffer-live-p org-agenda-buffer)
        org-agenda-buffer))
    (org-compile-prefix-format nil)
    (setq result
      (loop for date in (funcall enumerate-days begin end) append
        (loop for file in (org-agenda-files nil 'ifmode)
          append
          (progn
            (org-check-agenda-file file)
            (apply 'org-agenda-get-day-entries file date org-agenda-entry-types)))))
    (unless (buffer-live-p (get-buffer org-agenda-buffer-name))
      (get-buffer-create org-agenda-buffer-name))
    (with-current-buffer (get-buffer org-agenda-buffer-name)
      (org-agenda-mode)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (mapcar
        (lambda (x)
          (let ((inhibit-read-only t))
            (insert (format "%s" x) "\n")))
        result))
;;    (display-buffer org-agenda-buffer-name t)
    result))

  (defun my/helm-org-refile-read-location (tbl)
    (setq my/helm-org-refile-locations tbl)
    (helm
     (list
      ;; (helm-build-sync-source "Today's tasks"
      ;;   :candidates (mapcar (lambda (a) (cons (car a) a))
      ;;                       (my/org-get-todays-items-as-refile-candidates))
      ;;   :action '(("Select" . identity)
      ;;             ("Clock in and track" . my/helm-org-clock-in-and-track-from-refile)
      ;;             ("Draw index card" . my/helm-org-prepare-index-card-for-subtree))
      ;;   :history 'org-refile-history)
      (helm-build-sync-source "Refile targets"
        :candidates (mapcar (lambda (a) (cons (car a) a)) tbl)
        :action '(("Select" . identity)
                  ("Clock in and track" . my/helm-org-clock-in-and-track-from-refile)
                  ("Draw index card" . my/helm-org-prepare-index-card-for-subtree))
        :history 'org-refile-history)
      (helm-build-dummy-source "Create task"
        :action (helm-make-actions
                 "Create task"
                 'my/helm-org-create-task)))))

  (defun my/org-refile-get-location (&optional prompt default-buffer new-nodes no-exclude)
    "Prompt the user for a refile location, using PROMPT.
  PROMPT should not be suffixed with a colon and a space, because
  this function appends the default value from
  `org-refile-history' automatically, if that is not empty.
  When NO-EXCLUDE is set, do not exclude headlines in the current subtree,
  this is used for the GOTO interface."
    (let ((org-refile-targets org-refile-targets)
          (org-refile-use-outline-path org-refile-use-outline-path)
          excluded-entries)
      (when (and (derived-mode-p 'org-mode)
                 (not org-refile-use-cache)
                 (not no-exclude))
        (org-map-tree
         (lambda()
           (setq excluded-entries
                 (append excluded-entries (list (org-get-heading t t)))))))
      (setq org-refile-target-table
            (org-refile-get-targets default-buffer excluded-entries)))
    (unless org-refile-target-table
      (user-error "No refile targets"))
    (let* ((cbuf (current-buffer))
           (partial-completion-mode nil)
           (cfn (buffer-file-name (buffer-base-buffer cbuf)))
           (cfunc (if (and org-refile-use-outline-path
                           org-outline-path-complete-in-steps)
                      'org-olpath-completing-read
                    'org-icompleting-read))
           (extra (if org-refile-use-outline-path "/" ""))
           (cbnex (concat (buffer-name) extra))
           (filename (and cfn (expand-file-name cfn)))
           (tbl (mapcar
                 (lambda (x)
                   (if (and (not (member org-refile-use-outline-path
                                         '(file full-file-path)))
                            (not (equal filename (nth 1 x))))
                       (cons (concat (car x) extra " ("
                                     (file-name-nondirectory (nth 1 x)) ")")
                             (cdr x))
                     (cons (concat (car x) extra) (cdr x))))
                 org-refile-target-table))
           (completion-ignore-case t)
           cdef
           (prompt (concat prompt
                           (or (and (car org-refile-history)
                                    (concat " (default " (car org-refile-history) ")"))
                               (and (assoc cbnex tbl) (setq cdef cbnex)
                                    (concat " (default " cbnex ")"))) ": "))
           pa answ parent-target child parent old-hist)
      (setq old-hist org-refile-history)
      ;; Use Helm's sources instead
      (setq answ (my/helm-org-refile-read-location tbl))
      (cond
       ((and (stringp answ)
             (setq pa (org-refile--get-location answ tbl)))
        (org-refile-check-position pa)
        (when (or (not org-refile-history)
                  (not (eq old-hist org-refile-history))
                  (not (equal (car pa) (car org-refile-history))))
          (setq org-refile-history
                (cons (car pa) (if (assoc (car org-refile-history) tbl)
                                   org-refile-history
                                 (cdr org-refile-history))))
          (if (equal (car org-refile-history) (nth 1 org-refile-history))
              (pop org-refile-history)))
        (setq my/org-refile-last-location pa)
        pa)
       ((and (stringp answ) (string-match "\\`\\(.*\\)/\\([^/]+\\)\\'" answ))
        (setq parent (match-string 1 answ)
              child (match-string 2 answ))
        (setq parent-target (org-refile--get-location parent tbl))
        (when (and parent-target
                   (or (eq new-nodes t)
                       (and (eq new-nodes 'confirm)
                            (y-or-n-p (format "Create new node \"%s\"? "
                                              child)))))
          (org-refile-new-child parent-target child)))
       ((listp answ) answ) ;; Sacha: Helm returned a refile location
       ((not (equal answ t))
        (user-error "Invalid target location")))))

  (fset 'org-refile-get-location 'my/org-refile-get-location)

(use-package smart-mode-line)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(defadvice color-theme-alist (around sacha activate)
  (if (ad-get-arg 0)
      ad-do-it
    nil))
(use-package color-theme)
(use-package color-theme-solarized)
(defun my/setup-color-theme ()
  (interactive)
  (color-theme-solarized-dark)
  (set-face-foreground 'secondary-selection "darkblue")
  (set-face-background 'secondary-selection "lightblue")
  (set-face-background 'font-lock-doc-face "black")
  (set-face-foreground 'font-lock-doc-face "wheat")
  (set-face-background 'font-lock-string-face "black")
  (set-face-foreground 'org-todo "green")
  (set-face-background 'org-todo "black"))

(eval-after-load 'color-theme (my/setup-color-theme))

(when window-system
  (custom-set-faces
   '(erc-input-face ((t (:foreground "antique white"))))
   '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
   '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))) t)
   '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
   '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
   '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
   '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))))))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (progn
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (guide-key-mode 1)))  ; Enable guide-key-mode

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(defun my/def-rep-command (alist)
    "Return a lambda that calls the first function of ALIST.
It sets the transient map to all functions of ALIST,
allowing you to repeat those functions as needed."
    (let ((keymap (make-sparse-keymap))
                  (func (cdar alist)))
      (mapc (lambda (x)
              (when x
                (define-key keymap (kbd (car x)) (cdr x))))
            alist)
      (lambda (arg)
        (interactive "p")
        (when func
          (funcall func arg))
        (set-transient-map keymap t))))

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(use-package helm-swoop
 :bind
 (("C-S-s" . helm-swoop)
  ("M-i" . helm-swoop)
  ("M-s s" . helm-swoop)
  ("M-s M-s" . helm-swoop)
  ("M-I" . helm-swoop-back-to-last-point)
  ("C-c M-i" . helm-multi-swoop)
  ("C-x M-i" . helm-multi-swoop-all)
  )
 :config
 (progn
   (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
   (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
)

(use-package windmove
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))

(use-package switch-window
  :bind (("C-x o" . switch-window)))

(defvar my/refile-map (make-sparse-keymap))

(defmacro my/defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (define-key my/refile-map
       (char-to-string ,key)
       (lambda (prefix)
         (interactive "p")
         (let ((org-refile-targets '(((,file) :maxlevel . 6)))
               (current-prefix-arg (or current-prefix-arg '(4))))
           (call-interactively 'org-refile))))))


  (define-key my/refile-map "," 'my/org-refile-to-previous-in-file)

(my/defshortcut ?i "~/.emacs.d/Sacha.org")
(my/defshortcut ?o "~/personal/organizer.org")
(my/defshortcut ?s "~/personal/sewing.org")
(my/defshortcut ?b "~/personal/business.org")
(my/defshortcut ?p "~/personal/google-inbox.org")
(my/defshortcut ?P "~/personal/google-ideas.org")
(my/defshortcut ?B "~/Dropbox/books")
(my/defshortcut ?e "~/code/emacs-notes/tasks.org")
(my/defshortcut ?w "~/Dropbox/public/sharing/index.org")
(my/defshortcut ?W "~/Dropbox/public/sharing/blog.org")
(my/defshortcut ?j "~/personal/journal.org")
(my/defshortcut ?I "~/Dropbox/Inbox")
(my/defshortcut ?g "~/sachac.github.io/evil-plans/index.org")
(my/defshortcut ?c "~/code/dev/elisp-course.org")
(my/defshortcut ?C "~/personal/calendar.org")
(my/defshortcut ?l "~/dropbox/public/sharing/learning.org")
(my/defshortcut ?q "~/personal/questions.org")

(defun my/key-chord-define (keymap keys command)
  "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

MODIFICATION: Do not define the transposed key chord.
"
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (define-key keymap (vector 'key-chord key1 key2) command)))
(fset 'key-chord-define 'my/key-chord-define)

(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my/org-check-agenda ()
  "Peek at agenda."
  (interactive)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if (window-parent) (delete-window) (bury-buffer)))
   ((get-buffer "*Org Agenda*")
    (switch-to-buffer-other-window "*Org Agenda*"))
   (t (org-agenda nil "a"))))

(defun my/goto-random-char ()
  (interactive)
  (goto-char (random (point-max))))

(use-package hydra
  :config
  (defhydra my/goto-random-char-hydra ()
    ("r" my/goto-random-char))

  (defhydra my/window-movement ()
    ("<left>" windmove-left)
    ("<right>" windmove-right)
    ("<down>" windmove-down)
    ("<up>" windmove-up)
    ("y" other-window "other")
    ("h" switch-window "switch-window")
    ("f" find-file "file")
    ("F" find-file-other-window "other file")
    ("v" (progn (split-window-right) (windmove-right)))
    ("o" delete-other-windows :color blue)
    ("a" ace-window)
    ("s" ace-swap-window)
    ("d" delete-window "delete")
    ("D" ace-delete-window "ace delete")
    ("i" ace-maximize-window "maximize")
    ("b" helm-buffers-list)
    ("q" nil))
  (defhydra join-lines ()
    ("<up>" join-line)
    ("<down>" (join-line 1))
    ("t" join-line)
    ("n" (join-line 1)))
  (defhydra my/quantified-hydra (:color blue)
    "Quick tracking of Quantified Awesome stuff"
    ("c" (my/org-clock-in-and-track-by-name "Childcare") "Childcare")
    ("f" (my/org-clock-in-and-track-by-name "Family") "Family")
    ("F" (my/org-clock-in-and-track-by-name "Read fiction") "Fiction")
    ("k" (my/org-clock-in-and-track-by-name "Clean the kitchen") "Kitchen")
    ("D" (my/org-clock-in-and-track-by-name "Draw") "Draw")
    ("w" (my/org-clock-in-and-track-by-name "Walk for 30+ minutes") "Walk")
    ("W" (my/org-clock-in-and-track-by-name "Write") "Write")
    ("r" (my/org-clock-in-and-track-by-name "Personal routines") "Routines")
    ("R" (my/org-clock-in-and-track-by-name "Relax") "Relax")
    ("t" (my/org-clock-in-and-track-by-name "Tidy") "Tidy")
    ("b" (my/org-clock-in-and-track-by-name "Play Borderlands 2") "Borderlands 2")
    ("l" (my/org-clock-in-and-track-by-name "Eat lunch") "Lunch")
    ("L" (my/org-clock-in-and-track-by-name "Do laundry") "Laundry")
    ("d" (my/org-clock-in-and-track-by-name "Eat dinner") "Dinner")
    ("e" (my/org-clock-in-and-track-by-name "Process my inbox") "E-mail")
    )
  (defhydra my/org (:color blue)
    "Convenient Org stuff."
    ("p" my/org-show-active-projects "Active projects")
    ("a" (org-agenda nil "a") "Agenda"))
  (defhydra my/key-chord-commands ()
    "Main"
    ("k" kill-sexp)
    ("h" my/org-jump :color blue)
    ("x" my/org-finish-previous-task-and-clock-in-new-one "Finish and clock in" :color blue)
    ("i" my/org-quick-clock-in-task "Clock in" :color blue)
    ("b" helm-buffers-list :color blue)
    ("f" find-file :color blue)
    ("a" my/org-check-agenda :color blue)
    ("c" (call-interactively 'org-capture) "capture" :color blue)
    ("t" (org-capture nil "T") "Capture task")
    ("." repeat)
    ("C-t" transpose-chars)
    ("o" my/org-off-my-computer :color blue)
    ("w" my/engine-mode-hydra/body "web" :exit t)
    ("m" imenu :color blue)
    ("q" quantified-track :color blue)
    ("r" my/describe-random-interactive-function)
    ("l" org-insert-last-stored-link)
    ("L" my/org-insert-link)
    ("+" text-scale-increase)
    ("-" text-scale-decrease))
  (defhydra my/engine-mode-hydra (:color blue)
    "Engine mode"
    ("b" engine/search-my-blog "blog")
    ("f" engine/search-my-photos "flickr")
    ("m" engine/search-mail "mail")
    ("g" engine/search-google "google")
    ("e" engine/search-emacswiki "emacswiki"))
  )


(defun my/org-insert-link ()
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (goto-char (match-end 0))
    (insert "\n"))
  (call-interactively 'org-insert-link))

(use-package key-chord
  :init
  (progn
    (fset 'key-chord-define 'my/key-chord-define)
    (setq key-chord-one-key-delay 0.16)
    (key-chord-mode 1)
    ;; k can be bound too
    (key-chord-define-global "uu"     'undo)
    (key-chord-define-global "jr"     'my/goto-random-char-hydra/my/goto-random-char)
    (key-chord-define-global "kk"     'my/org/body)
    (key-chord-define-global "jj"     'avy-goto-word-1)
    (key-chord-define-global "yy"    'my/window-movement/body)
    (key-chord-define-global "jw"     'switch-window)
    (key-chord-define-global "jl"     'avy-goto-line)
    (key-chord-define-global "j."     'join-lines/body)
    ;(key-chord-define-global "jZ"     'avy-zap-to-char)
    (key-chord-define-global "FF"     'find-file)
    (key-chord-define-global "qq"     'my/quantified-hydra/body)
    (key-chord-define-global "hh"     'my/key-chord-commands/body)
    (key-chord-define-global "xx"     'er/expand-region)
    (key-chord-define-global "  "     'my/insert-space-or-expand)
    (key-chord-define-global "JJ"     'my/switch-to-previous-buffer)))

(bind-key "C-t" 'my/key-chord-commands/body)

(use-package smartscan
  :defer t
  :config (global-smartscan-mode t))

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(defun my/save-photo (name)
  (interactive "MName: ")
  (let* ((file (dired-get-filename))
         new-name)
    (cond
    ((string-match "CameraZOOM-\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)" file)
      (setq new-name
            (format "%s-%s-%s %s.%s.%s.%s %s.jpg"
                    (match-string 1 file)
                    (match-string 2 file)
                    (match-string 3 file)
                    (match-string 4 file)
                    (match-string 5 file)
                    (match-string 6 file)
                    (match-string 7 file)
                    name)))
    ((string-match "\\([0-9][0-9][0-9][0-9]\\)[\\.-]\\([0-9][0-9]\\)[\\.-]\\([0-9][0-9]\\)[\\.- ]\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)" file)
      (setq new-name
            (format "%s-%s-%s %s.%s.%s %s.jpg"
                    (match-string 1 file)
                    (match-string 2 file)
                    (match-string 3 file)
                    (match-string 4 file)
                    (match-string 5 file)
                    (match-string 6 file)
                    name)))
    (t (setq new-name (concat (file-name-sans-extension (file-name-nondirectory file)) " " name ".jpg"))))
    (when (string-match "A-" name)
      (copy-file file (expand-file-name new-name my/kid-photo-directory)))
    (rename-file file (expand-file-name new-name "~/archives/2016/photos/selected/"))))
(defun my/backup-media ()
  (interactive)
  (mapcar (lambda (file)
            (rename-file
             file
             (expand-file-name
              (file-name-nondirectory file)
              (cond
               ((string-match "mp4" file) "~/archives/2016/videos/")
               ((string-match "mp3\\|wav" file) "~/archives/2016/audio/")
               (t "~/archives/2016/photos/backup/")))))
          (dired-get-marked-files)))
(bind-key "b" 'my/save-photo dired-mode-map)
(bind-key "r" 'my/backup-media dired-mode-map)

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(defun my/shuffle-lines-in-region (beg end)
  (interactive "r")
  (let ((list (split-string (buffer-substring beg end) "[\r\n]+")))
    (delete-region beg end)
    (insert (mapconcat 'identity (shuffle-list list) "\n"))))

(use-package artbollocks-mode
  :defer t
  :load-path  "~/elisp/artbollocks-mode"
  :config
  (progn
    (setq artbollocks-weasel-words-regex
          (concat "\\b" (regexp-opt
                         '("one of the"
                           "should"
                           "just"
                           "sort of"
                           "a lot"
                           "probably"
                           "maybe"
                           "perhaps"
                           "I think"
                           "really"
                           "pretty"
                           "nice"
                           "action"
                           "utilize"
                           "leverage") t) "\\b"))
    ;; Don't show the art critic words, or at least until I figure
    ;; out my own jargon
    (setq artbollocks-jargon nil)))

(defun my/unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list t)))
    (let ((fill-column (point-max)))
      (fill-paragraph nil region)))
(bind-key "M-Q" 'my/unfill-paragraph)

(defun my/fill-or-unfill-paragraph (&optional unfill region)
    "Fill paragraph (or REGION).
  With the prefix argument UNFILL, unfill it instead."
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list (if current-prefix-arg 'unfill) t)))
    (let ((fill-column (if unfill (point-max) fill-column)))
      (fill-paragraph nil region)))
(bind-key "M-q" 'my/fill-or-unfill-paragraph)

(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(defmacro my/insert-unicode (unicode-name)
  `(lambda () (interactive)
     (insert-char (cdr (assoc-string ,unicode-name (ucs-names))))))
(bind-key "C-x 8 s" (my/insert-unicode "ZERO WIDTH SPACE"))
(bind-key "C-x 8 S" (my/insert-unicode "SNOWMAN"))

(bind-key "M-SPC" 'cycle-spacing)

(bind-key "M-/" 'hippie-expand)

(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(let ((data (quote (("bc" "because") ("wo" "without") ("wi" "with") ("qm" "sacha@sachachua.com") ("qe" "http://sachachua.com/dotemacs") ("qw" "http://sachachua.com/") ("qb" "http://sachachua.com/blog/") ("qc" "http://sachachua.com/blog/emacs-chat/")))))
(mapc (lambda (x) (define-global-abbrev (car x) (cadr x))) (cddr data))
)

(add-hook 'text-mode-hook 'abbrev-mode)
(diminish 'abbrev-mode " A")

(setq org-modules '(org-bbdb
                      org-gnus
                      org-drill
                      org-info
                      org-jsinfo
                      org-habit
                      org-irc
                      org-mouse
                      org-protocol
                      org-annotate-file
                      org-eval
                      org-expiry
                      org-interactive-query
                      org-man
                      org-collector
                      org-panel
                      org-screen
                      org-toc))
(eval-after-load 'org
 '(org-load-modules-maybe t))
;; Prepare stuff for org-export-backends
(setq org-export-backends '(org latex icalendar html ascii))

(bind-key "C-c r" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c O" 'org-open-at-point-global)
(bind-key "<f9> <f9>" 'org-agenda-list)
(bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))

(with-eval-after-load 'org
  (bind-key "C-M-w" 'append-next-kill org-mode-map)
  (bind-key "C-TAB" 'org-cycle org-mode-map)
  (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
  (bind-key "C-c C-r" 'org-refile org-mode-map)
  (bind-key "C-c R" 'org-reveal org-mode-map)
  (bind-key "C-c o" 'my/org-follow-entry-link org-mode-map)
  (bind-key "C-c d" 'my/org-move-line-to-destination org-mode-map)
  (bind-key "C-c f" 'my/org-file-blog-index-entries org-mode-map)
  (bind-key "C-c t s"  'my/split-sentence-and-capitalize org-mode-map)
  (bind-key "C-c t -"  'my/split-sentence-delete-word-and-capitalize org-mode-map)
  (bind-key "C-c t d"  'my/delete-word-and-capitalize org-mode-map)

  (bind-key "C-c C-p C-p" 'my/org-publish-maybe org-mode-map)
  (bind-key "C-c C-r" 'my/org-refile-and-jump org-mode-map))

(with-eval-after-load 'org-agenda
  (bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))

(setq org-use-effective-time t)

   (defun my/org-use-speed-commands-for-headings-and-lists ()
     "Activate speed commands on list items too."
     (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
         (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))
   (setq org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists)

(with-eval-after-load 'org
   (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
   (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
   (add-to-list 'org-speed-commands-user '("!" my/org-clock-in-and-track))
   (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
   (add-to-list 'org-speed-commands-user '("d" my/org-move-line-to-destination))
   (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
   (add-to-list 'org-speed-commands-user '("P" call-interactively 'org2blog/wp-post-subtree))
   (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
   (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
   (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map))

(setq org-goto-interface 'outline
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)

(defun my/org-follow-entry-link ()
  "Follow the defined link for this entry."
  (interactive)
  (if (org-entry-get (point) "LINK")
      (org-open-link-from-string (org-entry-get (point) "LINK"))
    (org-open-at-point)))

(defun my/org-link-projects (location)
  "Add link properties between the current subtree and the one specified by LOCATION."
  (interactive
   (list (let ((org-refile-use-cache nil))
     (org-refile-get-location "Location"))))
  (let ((link1 (org-store-link nil)) link2)
    (save-window-excursion
      (org-refile 4 nil location)
      (setq link2 (org-store-link nil))
      (org-set-property "LINK" link1))
    (org-set-property "LINK" link2)))

(with-eval-after-load 'org
     (bind-key "C-c k" 'org-cut-subtree org-mode-map)
     (setq org-yank-adjusted-subtrees t))

(setq org-directory "~/personal")
(setq org-default-notes-file "~/personal/organizer.org")

(defun my/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'my/yank-more)

(defun my/org-insert-heading-for-next-day ()
	"Insert a same-level heading for the following day."
	(interactive)
	(let ((new-date
				 (seconds-to-time
					(+ 86400.0
						 (float-time
							(org-read-date nil 'to-time (elt (org-heading-components) 4)))))))
		(org-insert-heading-after-current)
		(insert (format-time-string "%Y-%m-%d\n\n" new-date))))

(defun my/org-contacts-template-email (&optional return-value)
    "Try to return the contact email for a template.
  If not found return RETURN-VALUE or something that would ask the user."
    (or (cadr (if (gnus-alive-p)
                  (gnus-with-article-headers
                    (mail-extract-address-components
                     (or (mail-fetch-field "Reply-To") (mail-fetch-field "From") "")))))
        return-value
        (concat "%^{" org-contacts-email-property "}p")))


  (defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")
  (setq org-capture-templates
        `(("t" "Tasks" entry
           (file+headline "~/personal/organizer.org" "Inbox")
           ,my/org-basic-task-template)
          ("T" "Quick task" entry
           (file+headline "~/personal/organizer.org" "Inbox")
           "* TODO %^{Task}\nSCHEDULED: %t\n"
           :immediate-finish t)
          ("i" "Interrupting task" entry
           (file+headline "~/personal/organizer.org" "Inbox")
           "* STARTED %^{Task}"
           :clock-in :clock-resume)
          ("e" "Emacs idea" entry
           (file+headline "~/code/emacs-notes/tasks.org" "Emacs")
           "* TODO %^{Task}"
           :immediate-finish t)
          ("E" "Energy" table-line
           (file+headline "~/personal/organizer.org" "Track energy")
           "| %U | %^{Energy 5-awesome 3-fuzzy 1-zzz} | %^{Note} |"
           :immediate-finish t
           )
          ("b" "Business task" entry
           (file+headline "~/personal/business.org" "Tasks")
           ,my/org-basic-task-template)
          ("p" "People task" entry
           (file+headline "~/personal/people.org" "Tasks")
           ,my/org-basic-task-template)
          ("j" "Journal entry" plain
           (file+datetree "~/personal/journal.org")
           "%K - %a\n%i\n%?\n"
           :unnarrowed t)
          ("J" "Journal entry with date" plain
           (file+datetree+prompt "~/personal/journal.org")
           "%K - %a\n%i\n%?\n"
           :unnarrowed t)
          ("s" "Journal entry with date, scheduled" entry
           (file+datetree+prompt "~/personal/journal.org")
           "* \n%K - %a\n%t\t%i\n%?\n"
           :unnarrowed t)
          ("c" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox")
           "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")
          ("db" "Done - Business" entry
           (file+headline "~/personal/business.org" "Tasks")
           "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
          ("dp" "Done - People" entry
           (file+headline "~/personal/people.org" "Tasks")
           "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
          ("dt" "Done - Task" entry
           (file+headline "~/personal/organizer.org" "Inbox")
           "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
          ("q" "Quick note" item
           (file+headline "~/personal/organizer.org" "Quick notes"))
          ("l" "Ledger entries")
          ("lm" "MBNA" plain
           (file "~/personal/ledger")
           "%(org-read-date) %^{Payee}
    Liabilities:MBNA
    Expenses:%^{Account}  $%^{Amount}
  " :immediate-finish t)
          ("ln" "No Frills" plain
           (file "~/personal/ledger")
           "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
    Liabilities:MBNA
    Assets:Wayne:Groceries  $%^{Amount}
  " :immediate-finish t)
          ("lc" "Cash" plain
           (file "~/personal/ledger")
           "%(org-read-date) * %^{Payee}
    Expenses:Cash
    Expenses:%^{Account}  %^{Amount}
  ")
          ("B" "Book" entry
           (file+datetree "~/personal/books.org" "Inbox")
           "* %^{Title}  %^g
  %i
  *Author(s):* %^{Author} \\\\
  *ISBN:* %^{ISBN}

  %?

  *Review on:* %^t \\
  %a
  %U"
           :clock-in :clock-resume)
           ("C" "Contact" entry (file "~/personal/contacts.org")
            "* %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(my/org-contacts-template-email)
  :END:")
           ("n" "Daily note" table-line (file+olp "~/personal/organizer.org" "Inbox")
            "| %u | %^{Note} |"
            :immediate-finish t)
           ("r" "Notes" entry
            (file+datetree "~/personal/organizer.org")
            "* %?\n\n%i\n%U\n"
            )))
  (bind-key "C-M-r" 'org-capture)

(defun my/org-refile-and-jump ()
  (interactive)
  (if (derived-mode-p 'org-capture-mode)
      (org-capture-refile)
    (call-interactively 'org-refile))
  (org-refile-goto-last-stored))
(eval-after-load 'org-capture
 '(bind-key "C-c C-r" 'my/org-refile-and-jump org-capture-mode-map))

(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
(setq org-blank-before-new-entry nil)

;; Example: (org-refile 4 nil (my/org-refile-get-location-by-substring "Other Emacs"))
(defun my/org-refile-get-location-by-substring (regexp &optional file)
  "Return the refile location identified by REGEXP."
  (let ((org-refile-targets org-refile-targets) tbl)
    (setq org-refile-target-table (org-refile-get-targets)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (cl-find regexp org-refile-target-table
           :test
           (lambda (a b)
             (and
              (string-match a (car b))
              (or (null file)
                  (string-match file (elt b 1)))))))
(defun my/org-refile-subtree-to (name)
  (org-refile nil nil (my/org-refile-get-location-exact name)))

 (defun my/org-refile-get-location-exact (name &optional file)
  "Return the refile location identified by NAME."
  (let ((org-refile-targets org-refile-targets) tbl)
    (setq org-refile-target-table (org-refile-get-targets)))
  (unless org-refile-target-table
    (user-error "No refile targets"))
  (cl-find name org-refile-target-table
           :test (lambda (a b)
                 (and (string-equal a (car b))
              (or (null file)
                  (string-match file (elt b 1)))))))
;; Example: (my/org-clock-in-refile "Off my computer")
 (defun my/org-clock-in-refile (location &optional file)
  "Clocks into LOCATION.
LOCATION and FILE can also be regular expressions for `my/org-refile-get-location-by-substring'."
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (save-excursion
      (if (stringp location) (setq location (my/org-refile-get-location-by-substring location file)))
      (org-refile 4 nil location)
      (org-clock-in))))

 (defun my/org-finish-previous-task-and-clock-in-new-one (location &optional file)
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (org-clock-goto)
    (org-todo 'done))
  (my/org-clock-in-and-track-by-name location file))

(defun my/org-clock-in-and-track-by-name (location &optional file)
  (interactive (list (my/org-refile-get-location)))
  (save-window-excursion
    (save-excursion
      (if (stringp location) (setq location (my/org-refile-get-location-exact location file)))
      (org-refile 4 nil location)
      (my/org-clock-in-and-track))))
(defun my/org-off-my-computer (category)
  (interactive "MCategory: ")
  (my/org-clock-in-refile "Off my computer")
  (quantified-track category))

(defun my/org-jump ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-refile)))

(require 'org-clock)
(defun my/org-entry-wpm ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let* ((words (count-words-region (point-min) (point-max)))
	     (minutes (org-clock-sum-current-item))
	     (wpm (/ words minutes)))
	(message "WPM: %d (words: %d, minutes: %d)" wpm words minutes)
	(kill-new (number-to-string wpm))))))

(setq org-todo-keywords
 '((sequence
    "TODO(t)"  ; next action
    "TOBLOG(b)"  ; next action
    "STARTED(s)"
    "WAITING(w@/!)"
    "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
   (sequence "LEARN" "TRY" "TEACH" "|" "COMPLETE(x)")
   (sequence "TOSKETCH" "SKETCHED" "|" "POSTED")
   (sequence "TOBUY" "TOSHRINK" "TOCUT"  "TOSEW" "|" "DONE(x)")
   (sequence "TODELEGATE(-)" "DELEGATED(d)" "|" "COMPLETE(x)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "cyan" :weight bold))
        ("WAITING" . (:foreground "red" :weight bold))
        ("SOMEDAY" . (:foreground "gray" :weight bold))))

(setq org-log-done 'time)

(setq org-tags-exclude-from-inheritance '("project"))

(add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
(add-to-list 'org-speed-commands-user '("W" widen))

(defun my/org-agenda-for-subtree ()
  (interactive)
  (when (derived-mode-p 'org-agenda-mode) (org-agenda-switch-to))
  (my/org-with-current-task
   (let ((org-agenda-view-columns-initially t))
     (org-agenda nil "t" 'subtree))))
(add-to-list 'org-speed-commands-user '("T" my/org-agenda-for-subtree))

(add-to-list 'org-speed-commands-user '("S" call-interactively 'org-sort))

(setq org-tag-alist '(("@work" . ?b)
                      ("@home" . ?h)
                      ("@writing" . ?w)
                      ("@errands" . ?e)
                      ("@drawing" . ?d)
                      ("@coding" . ?c)
                      ("kaizen" . ?k)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@computer" . ?l)
                      ("quantified" . ?q)
                      ("fuzzy" . ?0)
                      ("highenergy" . ?1)))

(add-to-list 'org-global-properties
      '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

;; COPIED
(use-package org
 :init
 (progn
  (setq org-expiry-inactive-timestamps t)
  (setq org-clock-idle-time nil)
  (setq org-log-done 'time)
  (setq org-clock-continuously nil)
  (setq org-clock-persist t)
  (setq org-clock-in-switch-to-state "STARTED")
  (setq org-clock-in-resume nil)
  (setq org-show-notification-handler 'message)
  (setq org-clock-report-include-clocking-task t))
 :config
  (org-clock-persistence-insinuate))

(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)

(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)

(add-hook 'org-clock-in-prepare-hook
          'my/org-mode-ask-effort)

(defun my/org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

(defvar my/org-agenda-limit-items nil "Number of items to show in agenda to-do views; nil if unlimited.")
(eval-after-load 'org
'(defadvice org-agenda-finalize-entries (around sacha activate)
  (if my/org-agenda-limit-items
      (progn
        (setq list (mapcar 'org-agenda-highlight-todo list))
        (setq ad-return-value
              (subseq list 0 my/org-agenda-limit-items))
        (when org-agenda-before-sorting-filter-function
          (setq list (delq nil (mapcar org-agenda-before-sorting-filter-function list))))
        (setq ad-return-value
              (mapconcat 'identity
                         (delq nil
                               (subseq
                                (sort list 'org-entries-lessp)
                                0
                                my/org-agenda-limit-items))
                         "\n")))
    ad-do-it)))

;; Get this from https://raw.github.com/chenfengyuan/elisp/master/next-spec-day.el
(load "~/elisp/next-spec-day.el" t)

(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)

(setq org-structure-template-alist
      '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
        ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
        ("c" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
        ("p" "#+BEGIN_PRACTICE\n?\n#+END_PRACTICE")
        ("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
        ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
        ("H" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n?\n#+end_ascii")
        ("A" "#+ascii: ")
        ("i" "#+index: ?" "#+index: ?")
        ("I" "#+include %file ?" "<include file=%file markup=\"?\">")))

(defun my/org-html-quote2 (block backend info)
  (when (org-export-derived-backend-p backend 'html)
  (when (string-match "\\`<div class=\"quote2\">" block)
  (setq block (replace-match "<blockquote>" t nil block))
  (string-match "</div>\n\\'" block)
  (setq block (replace-match "</blockquote>\n" t nil block))
  block)))
(eval-after-load 'ox
'(add-to-list 'org-export-filter-special-block-functions 'my/org-html-quote2))

(defun my/org-link-youtube-time (url beg end)
  "Link times of the form h:mm to YouTube video at URL.
Works on region defined by BEG and END."
  (interactive (list (read-string "URL: " (org-entry-get-with-inheritance "YOUTUBE")) (point) (mark)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((char (if (string-match "\\?" url) "&" "?")))
        (while (re-search-forward "\\(\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\) ::" nil t)
          (replace-match
           (format "[[%s%st=%sh%sm%ss][%s]] "
                   url
                   char
                   (match-string 2)
                   (match-string 3)
                   (or (match-string 5) "0")
                   (match-string 1)) nil t))))))

(defun my/clean-up-google-hangout-chat ()
  (interactive)
  (save-excursion
    (while (re-search-forward "<hr.*?div class=\"Kc-Ma-m\".*?>" nil t)
      (replace-match "\n| ")))
  (save-excursion
    (while (re-search-forward "</div><div class=\"Kc-yi-m\">" nil t)
      (replace-match " | ")))
  (save-excursion
    (while (re-search-forward "</div></div><div class=\"Kc-ib\">" nil t)
      (replace-match " | ")))
  (save-excursion
    (while (re-search-forward "<a rel=\"nofollow\" target=\"_blank\" href=\"\\(.*?\\)\">\\(.*?\\)</a>" nil t)
      (replace-match "[[\\1][\\2]]")))
  (save-excursion
    (while (re-search-forward "</div></div></div></div>" nil t)
      (replace-match " |")))
  (save-excursion
    (while (re-search-forward "&nbsp;" nil t)
      (replace-match " ")))
  (save-excursion
    (while (re-search-forward "</div><div class=\"Kc-ib\">" nil t)
      (replace-match " ")))
  (save-excursion
    (while (re-search-forward "<img.*?>" nil t)
      (replace-match "")))
  (save-excursion
    (while (re-search-forward "<wbr>" nil t)
      (replace-match "")))
  )

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (and (file-exists-p x) x))
                    `("~/personal/organizer.org"
                      "~/personal/sewing.org"
                      "~/personal/people.org"
                      "~/Dropbox/wsmef/trip.txt"
                      ,my/kid-org-file
                      "~/personal/business.org"
                      "~/personal/calendar.org"
                      "~/Dropbox/tasker/summary.txt"
                      "~/Dropbox/public/sharing/index.org"
                      "~/dropbox/public/sharing/learning.org"
                      "~/code/emacs-notes/tasks.org"
                      "~/sachac.github.io/evil-plans/index.org"
                      "~/personal/cooking.org"
                      "~/personal/routines.org"))))
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

(setq org-agenda-span 2)
(setq org-agenda-tags-column -100) ; take advantage of the screen width
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-time-grid
      '((daily today require-timed)
       "----------------"
       (800 1000 1200 1400 1600 1800)))
(setq org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")

(bind-key "Y" 'org-agenda-todo-yesterday org-agenda-mode-map)

(setq org-agenda-start-on-weekday 6)

(defun my/org-agenda-project-agenda ()
  "Return the project headline and up to `my/org-agenda-limit-items' tasks."
  (save-excursion
    (let* ((marker (org-agenda-new-marker))
           (heading
            (org-agenda-format-item "" (org-get-heading) (org-get-category) nil))
           (org-agenda-restrict t)
           (org-agenda-restrict-begin (point))
           (org-agenda-restrict-end (org-end-of-subtree 'invisible))
           ;; Find the TODO items in this subtree
           (list (org-agenda-get-day-entries (buffer-file-name) (calendar-current-date) :todo)))
      (org-add-props heading
          (list 'face 'defaults
                'done-face 'org-agenda-done
                'undone-face 'default
                'mouse-face 'highlight
                'org-not-done-regexp org-not-done-regexp
                'org-todo-regexp org-todo-regexp
                'org-complex-heading-regexp org-complex-heading-regexp
                'help-echo
                (format "mouse-2 or RET jump to org file %s"
                        (abbreviate-file-name
                         (or (buffer-file-name (buffer-base-buffer))
                             (buffer-name (buffer-base-buffer))))))
        'org-marker marker
        'org-hd-marker marker
        'org-category (org-get-category)
        'type "tagsmatch")
      (concat heading "\n"
              (org-agenda-finalize-entries list)))))

(defun my/org-agenda-projects-and-tasks (match)
  "Show TODOs for all `org-agenda-files' headlines matching MATCH."
  (interactive "MString: ")
  (let ((todo-only nil))
    (if org-agenda-overriding-arguments
        (setq todo-only (car org-agenda-overriding-arguments)
              match (nth 1 org-agenda-overriding-arguments)))
    (let* ((org-tags-match-list-sublevels
            org-tags-match-list-sublevels)
           (completion-ignore-case t)
           rtn rtnall files file pos matcher
           buffer)
      (when (and (stringp match) (not (string-match "\\S-" match)))
        (setq match nil))
      (when match
        (setq matcher (org-make-tags-matcher match)
              match (car matcher) matcher (cdr matcher)))
      (catch 'exit
        (if org-agenda-sticky
            (setq org-agenda-buffer-name
                  (if (stringp match)
                      (format "*Org Agenda(%s:%s)*"
                              (or org-keys (or (and todo-only "M") "m")) match)
                    (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
        (org-agenda-prepare (concat "TAGS " match))
        (org-compile-prefix-format 'tags)
        (org-set-sorting-strategy 'tags)
        (setq org-agenda-query-string match)
        (setq org-agenda-redo-command
              (list 'org-tags-view `(quote ,todo-only)
                    (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
        (setq files (org-agenda-files nil 'ifmode)
              rtnall nil)
        (while (setq file (pop files))
          (catch 'nextfile
            (org-check-agenda-file file)
            (setq buffer (if (file-exists-p file)
                             (org-get-agenda-file-buffer file)
                           (error "No such file %s" file)))
            (if (not buffer)
                ;; If file does not exist, error message to agenda
                (setq rtn (list
                           (format "ORG-AGENDA-ERROR: No such org-file %s" file))
                      rtnall (append rtnall rtn))
              (with-current-buffer buffer
                (unless (derived-mode-p 'org-mode)
                  (error "Agenda file %s is not in `org-mode'" file))
                (save-excursion
                  (save-restriction
                    (if org-agenda-restrict
                        (narrow-to-region org-agenda-restrict-begin
                                          org-agenda-restrict-end)
                      (widen))
                    (setq rtn (org-scan-tags 'my/org-agenda-project-agenda matcher todo-only))
                    (setq rtnall (append rtnall rtn))))))))
        (if org-agenda-overriding-header
            (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                        nil 'face 'org-agenda-structure) "\n")
          (insert "Headlines with TAGS match: ")
          (add-text-properties (point-min) (1- (point))
                               (list 'face 'org-agenda-structure
                                     'short-heading
                                     (concat "Match: " match)))
          (setq pos (point))
          (insert match "\n")
          (add-text-properties pos (1- (point)) (list 'face 'org-warning))
          (setq pos (point))
          (unless org-agenda-multi
            (insert "Press `C-u r' to search again with new search string\n"))
          (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
        (org-agenda-mark-header-line (point-min))
        (when rtnall
          (insert (mapconcat 'identity rtnall "\n") ""))
        (goto-char (point-min))
        (or org-agenda-multi (org-agenda-fit-window-to-buffer))
        (add-text-properties (point-min) (point-max)
                             `(org-agenda-type tags
                                               org-last-args (,todo-only ,match)
                                               org-redo-cmd ,org-agenda-redo-command
                                               org-series-cmd ,org-cmd))
        (org-agenda-finalize)
        (setq buffer-read-only t)))))

(defvar my/org-agenda-contexts
  '((tags-todo "+@phone")
    (tags-todo "+@work")
    (tags-todo "+@drawing")
    (tags-todo "+@coding")
    (tags-todo "+@writing")
    (tags-todo "+@computer")
    (tags-todo "+@home")
    (tags-todo "+@errands"))
  "Usual list of contexts.")
(defun my/org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))
(setq org-agenda-custom-commands
      `(("t" tags-todo "-cooking"
         ((org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))
        ("T" tags-todo "TODO=\"TODO\"-goal-routine-cooking-SCHEDULED={.+}" nil "~/Dropbox/agenda/nonroutine.html")
        ("f" tags-todo "fuzzy-TODO=\"DONE\"-TODO=\"CANCELLED\"")
        ("F" tags-todo "highenergy-TODO=\"DONE\"-TODO=\"CANCELLED\"")
        ("b" todo ""
         ((org-agenda-files '("~/personal/business.org"))))
        ("B" todo ""
         ((org-agenda-files '("~/Dropbox/books"))))
        ("o" todo ""
         ((org-agenda-files '("~/personal/organizer.org"))))
        ("c" todo ""
         ((org-agenda-prefix-format "")
          (org-agenda-cmp-user-defined 'my/org-sort-agenda-items-todo)
          (org-agenda-view-columns-initially t)
          ))
        ;; Weekly review
        ("w" "Weekly review" agenda ""
         ((org-agenda-span 7)
          (org-agenda-log-mode 1)) "~/Dropbox/agenda/this-week.html")
        ("W" "Weekly review sans routines" agenda ""
         ((org-agenda-span 7)
          (org-agenda-log-mode 1)
          (org-agenda-tag-filter-preset '("-routine"))) "~/Dropbox/agenda/this-week-nonroutine.html")
        ("2" "Bi-weekly review" agenda "" ((org-agenda-span 14) (org-agenda-log-mode 1)))
        ("5" "Quick tasks" tags-todo "EFFORT>=\"0:05\"&EFFORT<=\"0:15\"")
        ("0" "Unestimated tasks" tags-todo "EFFORT=\"\"")
        ("gb" "Business" todo ""
         ((org-agenda-files '("~/personal/business.org"))
          (org-agenda-view-columns-initially t)))
        ("gc" "Coding" tags-todo "@coding"
         ((org-agenda-view-columns-initially t)))
        ("gw" "Writing" tags-todo "@writing"
         ((org-agenda-view-columns-initially t)))
        ("gp" "Phone" tags-todo "@phone"
         ((org-agenda-view-columns-initially t)))
        ("gd" "Drawing" tags-todo "@drawing"
         ((org-agenda-view-columns-initially t)))
        ("gh" "Home" tags-todo "@home"
         ((org-agenda-view-columns-initially t)))
        ("gk" "Kaizen" tags-todo "kaizen"
         ((org-agenda-view-columns-initially t))
         ("~/Dropbox/agenda/errands.html"))
        ("ge" "Errands" tags-todo "@errands"
         ((org-agenda-view-columns-initially t))
         ("~/Dropbox/agenda/errands.html"))
        ("0" "Top 3 by context"
         ,my/org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-up effort-down))
          (my/org-agenda-limit-items 3)))
        (")" "All by context"
         ,my/org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-down effort-down))
          (my/org-agenda-limit-items nil)))
        ("9" "Unscheduled top 3 by context"
         ,my/org-agenda-contexts
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-agenda-sorting-strategy '(priority-down effort-down))
          (my/org-agenda-limit-items 3)))
        ("(" "All unscheduled by context"
         ,my/org-agenda-contexts
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-agenda-sorting-strategy '(priority-down effort-down))
          ))
        ("d" "Timeline for today" ((agenda "" ))
         ((org-agenda-ndays 1)
          (org-agenda-show-log t)
          (org-agenda-log-mode-items '(clock closed))
          (org-agenda-clockreport-mode t)
          (org-agenda-entry-types '())))
        ("." "Waiting for" todo "WAITING")
        ("u" "Unscheduled tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project"
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-agenda-view-columns-initially t)
          (org-tags-exclude-from-inheritance '("project"))
          (org-agenda-overriding-header "Unscheduled TODO entries: ")
          (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
        ("U" "Unscheduled tasks outside projects" tags-todo "-project"
         ((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
          (org-tags-exclude-from-inheritance nil)
          (org-agenda-view-columns-initially t)
          (org-agenda-overriding-header "Unscheduled TODO entries outside projects: ")
          (org-agenda-sorting-strategy '(todo-state-up priority-down tag-up category-keep effort-down))))
        ("P" "By priority"
         ((tags-todo "+PRIORITY=\"A\"")
          (tags-todo "+PRIORITY=\"B\"")
          (tags-todo "+PRIORITY=\"\"")
          (tags-todo "+PRIORITY=\"C\""))
         ((org-agenda-prefix-format "%-10c %-10T %e ")
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
        ("pp" tags "+project-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\"-inactive"
         ((org-tags-exclude-from-inheritance '("project"))
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
        ("p." tags "+project-TODO=\"DONE\""
         ((org-tags-exclude-from-inheritance '("project"))
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
        ("S" tags-todo "TODO=\"STARTED\"")
        ("C" "Cooking"
         ((tags "vegetables")
          (tags "chicken")
          (tags "beef")
          (tags "pork")
          (tags "other"))
         ((org-agenda-files '("~/personal/cooking.org"))
          (org-agenda-view-columns-initially t)
          (org-agenda-sorting-strategy '(scheduled-up time-down todo-state-up)))
         )
        ("2" "List projects with tasks" my/org-agenda-projects-and-tasks
         "+PROJECT"
         ((my/org-agenda-limit-items 3)))))
(bind-key "<apps> a" 'org-agenda)

(defun my/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'my/org-agenda-done)

(defun my/org-agenda-mark-done-and-add-followup ()
    "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
    (interactive)
    (org-agenda-todo "DONE")
    (org-agenda-switch-to)
    (org-capture 0 "t"))
;; Override the key definition
(define-key org-agenda-mode-map "X" 'my/org-agenda-mark-done-and-add-followup)

(defun my/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
;; New key assignment
(define-key org-agenda-mode-map "N" 'my/org-agenda-new)

(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down tag-up category-keep effort-up)
        ;; (todo user-defined-up todo-state-up priority-down effort-up)
        (todo todo-state-up priority-down effort-up)
        (tags user-defined-up)
        (search category-keep)))
(setq org-agenda-cmp-user-defined 'my/org-sort-agenda-items-user-defined)
(require 'cl)
(defun my/org-get-context (txt)
  "Find the context."
  (car (member-if
        (lambda (item) (string-match "@" item))
        (get-text-property 1 'tags txt))))

(defun my/org-compare-dates (a b)
  "Return 1 if A should go after B, -1 if B should go after A, or 0 if a = b."
  (cond
   ((and (= a 0) (= b 0)) nil)
   ((= a 0) 1)
   ((= b 0) -1)
   ((> a b) 1)
   ((< a b) -1)
   (t nil)))

(defun my/org-complete-cmp (a b)
  (let* ((state-a (or (get-text-property 1 'todo-state a) ""))
         (state-b (or (get-text-property 1 'todo-state b) "")))
    (or
     (if (member state-a org-done-keywords-for-agenda) 1)
     (if (member state-b org-done-keywords-for-agenda) -1))))

(defun my/org-date-cmp (a b)
  (let* ((sched-a (or (get-text-property 1 'org-scheduled a) 0))
         (sched-b (or (get-text-property 1 'org-scheduled b) 0))
         (deadline-a (or (get-text-property 1 'org-deadline a) 0))
         (deadline-b (or (get-text-property 1 'org-deadline b) 0)))
    (or
     (my/org-compare-dates
      (my/org-min-date sched-a deadline-a)
      (my/org-min-date sched-b deadline-b)))))

(defun my/org-min-date (a b)
  "Return the smaller of A or B, except for 0."
  (funcall (if (and (> a 0) (> b 0)) 'min 'max) a b))

(defun my/org-sort-agenda-items-user-defined (a b)
  ;; compare by deadline, then scheduled date; done tasks are listed at the very bottom
  (or
   (my/org-complete-cmp a b)
   (my/org-date-cmp a b)))

(defun my/org-context-cmp (a b)
  "Compare CONTEXT-A and CONTEXT-B."
  (let ((context-a (my/org-get-context a))
        (context-b (my/org-get-context b)))
    (cond
     ((null context-a) +1)
     ((null context-b) -1)
     ((string< context-a context-b) -1)
     ((string< context-b context-a) +1)
     (t nil))))

(defun my/org-sort-agenda-items-todo (a b)
  (or
   (org-cmp-time a b)
   (my/org-complete-cmp a b)
   (my/org-context-cmp a b)
   (my/org-date-cmp a b)
   (org-cmp-todo-state a b)
   (org-cmp-priority a b)
   (org-cmp-effort a b)))

(defun my/org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
	 (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))

(defun my/org-gcal-notify (title mes)
  (message "%s - %s" title mes))
(use-package org-gcal
  :load-path "~/elisp/org-gcal.el"
  :init (fset 'org-gcal-notify 'my/org-gcal-notify))

(defun my/org-show-active-projects ()
  "Show my current projects."
  (interactive)
  (org-tags-view nil "project-inactive-someday"))

(defvar my/weekly-review-line-regexp
  "^  \\([^:]+\\): +\\(Sched[^:]+: +\\)?TODO \\(.*?\\)\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include.")
(defvar my/weekly-done-line-regexp
  "^  \\([^:]+\\): +.*?\\(?:Clocked\\|Closed\\):.*?\\(TODO\\|DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$"
  "Regular expression matching lines to include as completed tasks.")

(defun my/quantified-get-hours (category time-summary)
  "Return the number of hours based on the time summary."
  (if (stringp category)
      (if (assoc category time-summary) (/ (cdr (assoc category time-summary)) 3600.0) 0)
    (apply '+ (mapcar (lambda (x) (my/quantified-get-hours x time-summary)) category))))

(defun _my/extract-tasks-from-agenda (string matchers prefix line-re)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward line-re nil t)
      (let ((temp-list matchers))
        (while temp-list
          (if (save-match-data
                (string-match (car (car temp-list)) (match-string 1)))
              (progn
                (add-to-list (cdr (car temp-list)) (concat prefix (match-string 3)) t)
                (setq temp-list nil)))
          (setq temp-list (cdr temp-list)))))))

(ert-deftest _my/extract-tasks-from-agenda ()
  (let (list-a list-b (line-re "\\([^:]+\\):\\( \\)\\(.*\\)"))
    (_my/extract-tasks-from-agenda
     "listA: Task 1\nother: Task 2\nlistA: Task 3"
     '(("listA" . list-a)
       ("." . list-b))
     "- [ ] "
     line-re)
    (should (equal list-a '("- [ ] Task 1" "- [ ] Task 3")))
    (should (equal list-b '("- [ ] Task 2")))))

(defun _my/get-upcoming-tasks ()
  (save-window-excursion
      (org-agenda nil "W")
      (_my/extract-tasks-from-agenda (buffer-string)
                                        '(("routines" . ignore)
                                          ("business" . business-next)
                                          ("people" . relationships-next)
                                          ("tasks" . emacs-next)
                                          ("." . life-next))
                                        "  - [ ] "
                                        my/weekly-review-line-regexp)))
(defun _my/get-previous-tasks ()
  (let (string)
    (save-window-excursion
      (org-agenda nil "W")
      (org-agenda-later -1)
      (org-agenda-log-mode 16)
      (setq string (buffer-string))
      ;; Get any completed tasks from the current week as well
      (org-agenda-later 1)
      (org-agenda-log-mode 16)
      (setq string (concat string "\n" (buffer-string)))
      (_my/extract-tasks-from-agenda string
                                        '(("routines" . ignore)
                                          ("business" . business)
                                          ("people" . relationships)
                                          ("tasks" . emacs)
                                          ("." . life))
                                        "  - [X] "
                                        my/weekly-done-line-regexp))))

(defun my/org-summarize-focus-areas (date)
  "Summarize previous and upcoming tasks as a list."
  (interactive (list (org-read-date-analyze (if current-prefix-arg (org-read-date) "-fri") nil '(0 0 0))))
  (let (business relationships life business-next relationships-next life-next string emacs emacs-next
                 start end time-summary biz-time ignore base-date)
    (setq base-date (apply 'encode-time date))
    (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
    (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
    (setq time-summary (quantified-summarize-time start end))
    (setq biz-time (my/quantified-get-hours "Business" time-summary))
    (_my/get-upcoming-tasks)
    (_my/get-previous-tasks)
    (setq string
          (concat
           (format "- *Business* (%.1fh - %d%%)\n" biz-time (/ biz-time 1.68))
           (mapconcat 'identity business "\n") "\n"
           (mapconcat 'identity business-next "\n")
           "\n"
           (format "  - *Earn* (%.1fh - %d%% of Business)\n"
                   (my/quantified-get-hours "Business - Earn" time-summary)
                   (/ (my/quantified-get-hours "Business - Earn" time-summary) (* 0.01 biz-time)))
           (format "  - *Build* (%.1fh - %d%% of Business)\n"
                   (my/quantified-get-hours "Business - Build" time-summary)
                   (/ (my/quantified-get-hours "Business - Build" time-summary) (* 0.01 biz-time)))
           (format "  - *Connect* (%.1fh - %d%% of Business)\n"
                   (my/quantified-get-hours "Business - Connect" time-summary)
                   (/ (my/quantified-get-hours "Business - Connect" time-summary) (* 0.01 biz-time)))
           (format "- *Relationships* (%.1fh - %d%%)\n"
                   (my/quantified-get-hours '("Discretionary - Social"
                                                 "Discretionary - Family") time-summary)
                   (/ (my/quantified-get-hours '("Discretionary - Social"
                                                    "Discretionary - Family") time-summary) 1.68))
           (mapconcat 'identity relationships "\n") "\n"
           (mapconcat 'identity relationships-next "\n") "\n"
           "\n"
           (format "- *Discretionary - Productive* (%.1fh - %d%%)\n"
                   (my/quantified-get-hours "Discretionary - Productive" time-summary)
                   (/ (my/quantified-get-hours "Discretionary - Productive" time-summary) 1.68))
           (format "  - *Drawing* (%.1fh)\n"
                   (my/quantified-get-hours '("Discretionary - Productive - Drawing")  time-summary))
           (format "  - *Emacs* (%.1fh)\n"
                   (my/quantified-get-hours "Discretionary - Productive - Emacs" time-summary))
           (mapconcat 'identity emacs "\n") "\n"
           (mapconcat 'identity emacs-next "\n") "\n"
           (format "  - *Coding* (%.1fh)\n"
                   (my/quantified-get-hours "Discretionary - Productive - Coding" time-summary))
           (mapconcat 'identity life "\n") "\n"
           (mapconcat 'identity life-next "\n") "\n"
           (format "  - *Sewing* (%.1fh)\n"
                   (my/quantified-get-hours "Discretionary - Productive - Sewing" time-summary))
           (format "  - *Writing* (%.1fh)\n"
                   (my/quantified-get-hours "Discretionary - Productive - Writing" time-summary))
           (format "- *Discretionary - Play* (%.1fh - %d%%)\n"
                   (my/quantified-get-hours "Discretionary - Play" time-summary)
                   (/ (my/quantified-get-hours "Discretionary - Play" time-summary) 1.68))
           (format "- *Personal routines* (%.1fh - %d%%)\n"
                   (my/quantified-get-hours "Personal" time-summary)
                   (/ (my/quantified-get-hours "Personal" time-summary) 1.68))
           (format "- *Unpaid work* (%.1fh - %d%%)\n"
                   (my/quantified-get-hours "Unpaid work" time-summary)
                   (/ (my/quantified-get-hours "Unpaid work" time-summary) 1.68))
           (format "- *A- (Childcare)* (%.1fh - %d%% of total)\n"
                   (my/quantified-get-hours '("A-") time-summary)
                   (/ (my/quantified-get-hours '("A-") time-summary) 1.68))
           (format "- *Sleep* (%.1fh - %d%% - average of %.1f per day)\n"
                   (my/quantified-get-hours "Sleep" time-summary)
                   (/ (my/quantified-get-hours "Sleep" time-summary) 1.68)
                   (/ (my/quantified-get-hours "Sleep" time-summary) 7)
                   )))
    (if (called-interactively-p 'any)
        (insert string)
      string)))

(defun my/org-add-line-item-task (task)
  (interactive "MTask: ")
  (org-insert-heading)
  (insert "[ ] " task)
  (let ((org-capture-entry '("t" "Tasks" entry
                             (file+headline "~/personal/organizer.org" "Tasks")
                             "")))
    (org-capture nil "t")
    (insert "TODO " task "\nSCHEDULED: <" (org-read-date) ">")))
;(define-key org-mode-map (kbd "C-c t") 'my/org-add-line-item-task)

(defun my/org-prepare-weekly-review (&optional date skip-urls)
  "Prepare weekly review template."
  (interactive (list (org-read-date-analyze (if current-prefix-arg (org-read-date) "-fri") nil '(0 0 0))))
  (let ((base-date (apply 'encode-time date))
        start end links)
    (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
    (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
    (outline-next-heading)
      (insert
       "*** Weekly review: Week ending " (format-time-string "%B %e, %Y" base-date) "  :weekly:\n"
       "*Blog posts*\n\n"
       "*Sketches*\n\n"
       (my/sketches-export-and-extract start end) "\n"
       "\n\n*Focus areas and time review*\n\n"
       (my/org-summarize-focus-areas date)
       "\n")))

(defun _my/clean-up-flickr-list (list)
    (setq list
          (replace-regexp-in-string "\\[\"" "[" list))
    (setq list
          (replace-regexp-in-string "<a href=\"\"\\([^\"]+\\).*?>.*?</a>"
                                    "[[\\1][\\2]]" list))
    (setq list
          (replace-regexp-in-string "\"
" "" (replace-regexp-in-string "\"\\]" "]" list))))

  (defun _my/format-flickr-link-for-org (x)
    (let ((title (assoc-default "FileName" x)))
      (format
       "- [[%s][%s]] %s"
       (assoc-default "URL" x)
       title
       (if (string= (assoc-default "Description" x) "")
           ""
         (concat "- "
                 (replace-regexp-in-string
                  "<a href=\"\"\\(.*?\\)\"\".*?>\\(.*?\\)</a>"
                  "[[\\1][\\2]]"
                  (assoc-default "Description" x)))))))


  (defun _my/parse-and-filter-flickr-csv-buffer (start end)
    (sort
     (delq nil
           (mapcar (lambda (x)
                     (if (and (string< (assoc-default "FileName" x) end)
                              (org-string<= start (assoc-default "FileName" x)))
                         x))
                   (csv-parse-buffer t)))
     (lambda (a b)
       (string< (assoc-default "FileName" a)
                (assoc-default "FileName" b)))))


  (defun my/sketches-export-and-extract (start end &optional do-insert update-db filter)
    "Create a list of links to sketches."
    (interactive (list (org-read-date) (org-read-date) t current-prefix-arg (read-string "Filter: ")))
    (let ((value
           (cond
            ((eq system-type 'windows-nt)
             (when update-db (shell-command "c:/sacha/dropbox/bin/flickr.bat"))
             (my/flickr-extract-links-for-review "c:/sacha/dropbox/bin/flickr_metadata.csv" start end))
            ((eq system-type 'gnu/linux)  ; Flickr
             (shell-command-to-string
             (format "cd /home/sacha/code/node; nodejs flickr-list.js -b \"%s\" -e \"%s\" -f \"%s\""
              (or start "") (or end "") (or filter ""))))
            ;; below method not used at the moment, but useful if flickr is being weird
            ((and t (eq system-type 'gnu/linux)) ;; Create links to sketches.sachachua.com/URL-encoded; not used at the moment
             ;; because Org does weird things with escaped # links
             (mapconcat
              (lambda (filename)
                (format "- [[http://sketches.sachachua.com/%s][%s]]\n"
                        (browse-url-url-encode-chars (file-name-nondirectory filename) "[ #]")
                        (file-name-base filename)))
              (let ((my/sketch-directories '("~/sketches"))) (my/get-index-card-filenames-between-dates start end))
              ""))
            )
           ))
      (if do-insert
          (insert value)
        value)))

;; (my/sketches-export-and-extract "2015-11-01" "2015-12-01")

  (defun my/flickr-extract-links-for-review (filename start end)
  "Extract Flickr titles and URLs from FILENAME from START to END.
  The file should be a CSV downloaded by the Flickr metadata downloader.
         Start date and end date should be strings in the form yyyy-mm-dd."
    (require 'csv)
    (let (list)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (setq list
              (mapconcat
               '_my/format-flickr-link-for-org
               (_my/parse-and-filter-flickr-csv-buffer start end)
               "\n"))
        (setq list (_my/clean-up-flickr-list list))
        (if (called-interactively-p 'any)
            (insert list)
          list))))

(defun kensanata/resolve-redirect (url)
  "Resolve shortened URL by launching `curl --head' and parsing the result."
  (let* ((curl (shell-command-to-string
                (format "curl --silent --head %s" url)))
         (location (when (and (string-match "^HTTP/1\.1 301" curl)
                              (string-match "^Location: \\(.*\\)" curl))
                     (match-string 1 curl))))
    (or location url)))

(defun my/resolve-urls-in-region (beg end)
  "Expand URLs between BEG and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
        (replace-match (save-match-data (kensanata/resolve-redirect
                                         (match-string 1))) t t nil 1))
      (goto-char (point-min))
      (while (re-search-forward org-link-re-with-space nil t)
        (replace-match (save-match-data (kensanata/resolve-redirect
                                         (match-string 0))) t t nil)))))

(defun my/open-urls-in-region (beg end)
  "Open URLs between BEG and END.
TODO: Get better at detecting and opening all URLs"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward org-any-link-re nil t)
        (save-excursion
          (backward-char)
          (org-open-at-point))))))

(defun my/evernote-export-and-extract (start-date end-date)
  "Extract notes created on or after START-DATE and before END-DATE."
  (let ((filename "c:/sacha/tmp/Evernote.enex"))
    (call-process
     "c:/Program Files (x86)/Evernote/Evernote/enscript.exe"
     nil t t
     "exportNotes"
     "/q" (concat
           " tag:roundup"
           " created:" (replace-regexp-in-string "-" "" start-date)
           " -created:" (replace-regexp-in-string "-" "" end-date))
     "/f" filename)
    (my/evernote-extract-links-for-review filename)))

(defun my/evernote-extract-links-for-review (filename)
  "Extract note names and URLs from FILENAME.
     The file should be an ENEX export."
  (interactive (list (read-file-name "File: ")
                     (org-read-date)
                     (org-read-date)))
  (let (list)
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (re-search-forward "<title>\\(.+?\\)</title>\\(.*?\n\\)*?via Diigo.*?href=\"\\(.*?\\)\"" nil t)
        (setq list
              (cons
               (cons
                (match-string-no-properties 1)
                (match-string-no-properties 3)) list))))
    (setq list
          (mapconcat (lambda (x)
                       (concat "- [["
                               (kensanata/resolve-redirect (cdr x))
                               "][" (car x) "]]: ")) list "\n"))
          (if (called-interactively-p 'any)
              (insert list)
            list)))

(defun my/evernote-export-and-extract-journal ()
    "Extract and file journal entries."
    (interactive)
    (let ((filename "c:\\sacha\\tmp\\journal.enex")
          (journal-file "~/personal/journal.org"))
      (call-process
       "c:/Program Files (x86)/Evernote/Evernote/enscript.exe"
       nil t t
       "exportNotes"
       "/q" (concat
             " notebook:!Inbox"
             " intitle:Journal")
       "/f" filename)
      (my/evernote-process-journal-entries filename journal-file)))

  (defun my/evernote-process-journal-entries (filename journal-file)
    "Insert all the journal entries if they do not yet exist."
    (let ((data (car (xml-parse-file filename))))
      (mapc (lambda (x)
              (if (and  (listp x) (equal (car x) 'note))
                  (my/evernote-create-journal-note x journal-file)))
            data)))

  (defun my/evernote-get-creation-date (note)
    "Return NOTE's created date as (month day year)."
    (let ((created (cadr (assoc-default 'created note))))
      (list (string-to-number (substring created 4 6)) ; month
            (string-to-number (substring created 6 8)) ; day
            (string-to-number (substring created 0 4))))) ; year

(defun my/evernote-create-journal-note (note journal-file)
  "Save the given NOTE to the JOURNAL-FILE."
  (with-current-buffer (find-file journal-file)
    (org-datetree-find-date-create (my/evernote-get-creation-date note))
    (forward-line 1)
    (when (org-at-heading-p) (save-excursion (insert "\n")))
    (let ((content (my/evernote-convert-content-to-org note)))
      (unless (save-excursion
                (re-search-forward (regexp-quote content)
                (max (point) (save-excursion (org-end-of-subtree t))) t))
        (insert content)))))

(defun my/evernote-convert-content-to-org (note)
  "Convert Evernote content for NOTE to HTML"
  (with-temp-buffer
    (insert (cadr (assoc-default 'content note)))
    (goto-char (point-min))
    (while (re-search-forward "div>" nil t)
      (replace-match "p>"))
    (shell-command-on-region (point-min) (point-max) "pandoc -f html -t org" nil t)
    (goto-char (point-min))
    (while (re-search-forward "^\\\\+" nil t)
     (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\\\\+$" nil t)
     (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
     (replace-match "\n\n"))
    (s-trim (buffer-string))))

(defun my/org-review-month (start-date)
  "Review the month's clocked tasks and time."
  (interactive (list (org-read-date)))
  ;; Set to the beginning of the month
  (setq start-date (concat (substring start-date 0 8) "01"))
  (let ((org-agenda-show-log t)
        (org-agenda-start-with-log-mode t)
        (org-agenda-start-with-clockreport-mode t)
        (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3)))
    (org-agenda-list nil start-date 'month)))

(defun _my/extract-posts-from-webpage (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "<pre>")
    (buffer-substring
     (point)
     (progn (re-search-forward "</pre>") (match-beginning 0)))))

(defun my/org-prepare-monthly-review ()
  (interactive)
  (let* ((date (decode-time))
         (month (elt date 4))
         (year (elt date 5))
         start-date
         end-date
         previous-date
         posts
         sketches
         org-date
         time)
    (calendar-increment-month month year -1)
    (setq start-date (format "%4d-%02d-01" year month)
          end-date (format "%4d-%02d-01" (elt date 5) (elt date 4))
          posts (_my/extract-posts-from-webpage
                 (format "http://sachachua.com/blog/%4d/%d?org=1"
                         year month))
          sketches (my/sketches-export-and-extract start-date nil nil t))
    (calendar-increment-month month year -1)
    (setq previous-date (format "%4d-%02d-01" year month))
    (setq time (my/quantified-compare previous-date start-date start-date end-date '("Business - Build" "Discretionary - Play" "Unpaid work" "A-" "Discretionary - Social" "Discretionary - Family" "Sleep" "Business - Connect" "Business - Earn" "Discretionary - Productive" "Personal") "Previous month %" "This month %"))
    (goto-char (line-end-position))
    (insert
     "\n\n** Monthly review: "
     (format-time-string "%B %Y" (encode-time 0 0 0 1 month year))
     "  :monthly:review:\n"
     "*Blog posts*\n"
     posts "\n\n"
     "*Sketches*\n\n"
     sketches
     "*Time*\n\n"
     (orgtbl-to-orgtbl time nil))))

(defun my/org-move-line-to-destination ()
      "Moves the current list item to <<destination>> in the current buffer.
If no <<destination>> is found, move it to the end of the list
and indent it one level."
      (interactive)
      (save-window-excursion
        (save-excursion
          (let ((string
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))
                found)
            (delete-region (line-beginning-position) (1+ (line-end-position)))
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "<<destination>>" nil t)
                (insert "\n" (make-string (- (match-beginning 0) (line-beginning-position)) ?\ ) (s-trim string))
                (setq found t)))
            (unless found
              (org-end-of-item-list)
              (insert string "\n"))))))

(defun my/org-move-line-to-end-of-list ()
  "Move the current list item to the end of the list."
  (interactive)
  (save-excursion
    (let ((string (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
      (delete-region (line-beginning-position) (1+ (line-end-position)))
      (org-end-of-item-list)
      (insert string))))

(defun my/org-file-blog-index-entries (beg end location)
  "Copy entries into blog.org."
  (interactive
   (list
    (if (region-active-p) (point) (line-beginning-position))
    (if (region-active-p) (mark) (1+ (line-end-position)))
    (let ((org-refile-targets
           '(("~/Dropbox/Public/sharing/blog.org" . (:maxlevel . 3)))))
      (save-excursion (org-refile-get-location "Location")))))
  (let ((s
         (replace-regexp-in-string
          "^[ \t]*- \\(\\[X\\] \\)?"
          "- [X] "
          (buffer-substring-no-properties beg end))))
    ;; if we're already in blog.org, delete the previous entry
    (if (string= buffer-file-name (expand-file-name "~/Dropbox/Public/sharing/blog.org"))
        (delete-region beg end))
    (save-window-excursion
      (save-excursion
        (find-file (nth 1 location))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (nth 3 location))
            (re-search-forward org-list-full-item-re nil t)
            (goto-char (line-beginning-position))
            (insert s)
            (org-update-statistics-cookies nil)))))))

(defvar my/org-last-refile-marker nil "Marker for last refile")
(defun my/org-refile-in-file (&optional prefix)
  "Refile to a target within the current file."
  (interactive)
  (let ((helm-org-headings-actions
         '(("Refile to this heading" . helm-org-heading-refile))))
    (save-excursion
      (helm-org-in-buffer-headings)
      (org-end-of-subtree t)
      (setq my/org-last-refile-marker (point-marker)))))

(defun my/org-refile-to-previous ()
  "Refile subtree to last position from `my/org-refile-in-file'."
  (interactive)
  (save-selected-window
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-switch-to))
    (org-cut-subtree)
    (save-excursion
      (let* ((marker my/org-last-refile-marker)
             (target-level
              (with-current-buffer (marker-buffer marker)
                (goto-char (marker-position marker))
                (org-current-level))))
        (helm-org-goto-marker marker)
        (org-end-of-subtree t t)
        (org-paste-subtree target-level)))))

(add-to-list 'org-speed-commands-user '("w" call-interactively 'my/org-refile-in-file))
(add-to-list 'org-speed-commands-user '("." call-interactively 'my/org-refile-to-previous))

(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-toc nil)
(setq org-html-toplevel-hlevel 2)
(setq org-export-htmlize-output-type 'css)

(setq org-ascii-text-width 10000)

(if (string= system-name "webdev")
       (setq my/emacs-notes-directory "~/code/dev/emacs-notes")
     (setq my/emacs-notes-directory "c:/sacha/code/dev/emacs-notes"))
    (setq org-publish-project-alist
          '(("public"
             :base-directory "c:/sacha/Dropbox/public"
             :publishing-directory "c:/sacha/Dropbox/public"
             :publishing-function my/org-html-publish-to-html-trustingly
             )
            ("sharing"
             :base-directory "c:/sacha/Dropbox/public/sharing"
             :publishing-directory "c:/sacha/Dropbox/public/sharing"
             :publishing-function my/org-html-publish-to-html-trustingly
             )
            ("emacs-config"
             :base-directory "~/.emacs.d"
             :publishing-directory "~/.emacs.d"
             :publishing-function my/org-html-publish-to-html-trustingly
             )
            ("book-notes"
             :base-directory "c:/sacha/Dropbox/books"
             :publishing-directory "c:/sacha/Dropbox/books/html"
             :publishing-function my/org-html-publish-to-html-trustingly
             :makeindex t)))
(load "~/code/dev/emacs-chats/build-site.el" t)
(load "~/code/dev/emacs-notes/build-site.el" t)

(defun my/org-publish-maybe ()
  (require 'ox-publish)
  (interactive)
  (save-excursion
    (if (org-publish-get-project-from-filename
           (buffer-file-name (buffer-base-buffer)) 'up)
           (org-publish-current-file t)
           (my/org-html-export-trustingly))))

(defun my/org-publish-and-browse ()
  (interactive)
  (save-buffer)
  (my/org-publish-maybe)
  (browse-url (org-export-output-file-name ".html" nil default-directory)))
(bind-key "<apps> b" 'my/org-publish-and-browse)

(use-package org2blog
  :load-path "~/code/org2blog"
  :commands 'org2blog/wp-post-subtree
  :config
   (progn
    (setq org2blog/wp-track-posts nil)
    (setq org2blog/wp-use-tags-as-categories t)
    (defadvice org2blog/wp-post-buffer (around sacha activate)
    (let ((org-confirm-babel-evaluate nil)
          (org-html-toplevel-hlevel 3))
      ad-do-it))))

(defun my/org2blog-subtree ()
  "Post to my blog and get files ready."
  (interactive)
  (org2blog/wp-post-subtree)
  (my/org-stage-image-files-in-subtree)
  (shell-command "start c:\\sacha\\dropbox\\inbox\\selection")
  (browse-url "http://sachachua.com/blog/wp-admin/edit.php?page=cal"))

(use-package htmlize)

(defun my/org-html-export-trustingly ()
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-export-to-html)))

(defun my/org-html-publish-to-html-trustingly (plist filename pub-dir)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-publish-to-html plist filename pub-dir)))

(setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\"
href=\"http://sachachua.com/blog/wp-content/themes/sacha-v3/foundation/css/foundation.min.css\"></link>
<link rel=\"stylesheet\" type=\"text/css\" href=\"http://sachachua.com/org-export.css\"></link>
<link rel=\"stylesheet\" type=\"text/css\" href=\"http://sachachua.com/blog/wp-content/themes/sacha-v3/style.css\"></link>
<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js\"></script>")
(setq org-html-htmlize-output-type 'css)
(setq org-src-fontify-natively t)

(setq org-html-preamble "<a name=\"top\" id=\"top\"></a>")
(setq org-html-postamble "
<style type=\"text/css\">
.back-to-top {
    position: fixed;
    bottom: 2em;
    right: 0px;
    text-decoration: none;
    color: #000000;
    background-color: rgba(235, 235, 235, 0.80);
    font-size: 12px;
    padding: 1em;
    display: none;
}

.back-to-top:hover {
    background-color: rgba(135, 135, 135, 0.50);
}
</style>

<div class=\"back-to-top\">
<a href=\"#top\">Back to top</a> | <a href=\"mailto:sacha@sachachua.com\">E-mail me</a>
</div>

<script type=\"text/javascript\">
    var offset = 220;
    var duration = 500;
    jQuery(window).scroll(function() {
        if (jQuery(this).scrollTop() > offset) {
            jQuery('.back-to-top').fadeIn(duration);
        } else {
            jQuery('.back-to-top').fadeOut(duration);
        }
    });
</script>")

(defun my/org-copy-region-as-html (beg end &optional level)
  "Make it easier to copy code for Wordpress posts and other things."
  (interactive "r\np")
  (let ((org-export-html-preamble nil)
        (org-html-toplevel-hlevel (or level 3)))
    (kill-new
     (org-export-string-as (buffer-substring beg end) 'html t))))

(defun my/org-copy-subtree-as-html ()
  (interactive)
  (my/org-copy-region-as-html
   (org-back-to-heading)
   (org-end-of-subtree)))

(setq org-html-checkbox-type 'unicode)
(setq org-html-checkbox-types
 '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
            (off . "<span class=\"task-todo\">&#x2610;</span>")
            (trans . "<span class=\"task-in-progress\">[-]</span>"))))

(defun my/org-share-emacs ()
  "Share my Emacs configuration."
  (interactive)
  (let* ((destination-dir "~/Dropbox/Public/")
         (destination-filename "sacha-emacs.org"))
    (my/save-new-packages)
    (with-current-buffer (find-file "~/.emacs.d/Sacha.org")
      (save-restriction
        (save-excursion
          (widen)
          (write-region (point-min) (point-max)
                        (expand-file-name destination-filename destination-dir))
          (with-current-buffer (find-file-noselect (expand-file-name
                                                    destination-filename destination-dir))
            (org-babel-tangle-file buffer-file-name
                                   (expand-file-name
                                    "sacha-emacs.el" destination-dir) "emacs-lisp")
            (org-html-export-to-html)))))))

(setq org-link-abbrev-alist
    '(("google" . "http://www.google.com/search?q=")
("gmap" . "http://maps.google.com/maps?q=%s")
("blog" . "http://sachachua.com/blog/p/")))

(defun my/org-protocol-insert-link (info)
  "Store and insert the link at point based on INFO."
  (org-protocol-store-link info)
  (with-current-buffer (window-buffer (selected-window))
    (insert "- ")
    (org-insert-last-stored-link 1)
    (insert "\n")))
(eval-after-load 'org-protocol
 '(add-to-list 'org-protocol-protocol-alist
    '("insert-link" :protocol "insert-link" :function my/org-protocol-insert-link)))

(defun my/org-get-links-in-region (beg end)
  (save-excursion
    (let (results)
      (goto-char (min beg end))
      (while (re-search-forward org-any-link-re (max beg end) t)
        (add-to-list 'results (org-element-context)))
      results)))

(defun my/org-dired-file-links-in-region (beg end)
  "Display a Dired buffer for the file links in the selected region."
  (interactive "r")
  (let ((files
         (-map
          (lambda (x)
            (expand-file-name (org-link-unescape (plist-get (cadr x) :path))))
          (-filter
           (lambda (x)
             (string= (plist-get (cadr x) :type) "file"))
           (my/org-get-links-in-region beg end)))))
    (with-current-buffer (get-buffer-create "*Files*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply 'call-process "ls" nil t nil "-lR" files))
      (dired-virtual "/")
      (switch-to-buffer (current-buffer)))))

(setq org-attach-store-link-p 'attached)
(setq org-attach-auto-tag nil)

(use-package ob-http)

(setq org-ditaa-jar-path "c:/sacha/Dropbox/bin/ditaa.jar")
(setq org-startup-with-inline-images t)
(use-package org
 :config
 (progn
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (ditaa . t)
   (sh . t)
   (sqlite . t)
   (http . t)
   (ledger . t)
   (shell . t)
   (R . t)))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))))

(defun my/org-summarize-task-status ()
  "Count number of tasks by status.
Probably should make this a dblock someday."
  (interactive)
  (let (result)
    (org-map-entries
     (lambda ()
       (let ((todo (elt (org-heading-components) 2)))
         (if todo
             (if (assoc todo result)
                 (setcdr (assoc todo result)
                         (1+ (cdr (assoc todo result))))
               (setq result (cons (cons todo 1) result)))))))
    (message "%s" (mapconcat (lambda (x) (format "%s: %d" (car x) (cdr x)))
                             result "\n"))))

(defun my/org-days-between (start end)
  "Number of days between START and END (exclusive).
This includes START but not END."
  (- (calendar-absolute-from-gregorian (org-date-to-gregorian end))
     (calendar-absolute-from-gregorian (org-date-to-gregorian start))))

(setq org-src-window-setup 'current-window)

(defun my/copy-code-as-org-block-and-gist (beg end)
  (interactive "r")
  (let ((filename (file-name-base))
        (mode (symbol-name major-mode))
        (contents
         (if (use-region-p) (buffer-substring beg end) (buffer-string)))
        (gist (if (use-region-p) (gist-region beg end) (gist-buffer))))
    (kill-new
     (format "\n[[%s][Gist: %s]]\n#+begin_src %s\n%s\n#+end_src\n"
             (oref (oref gist :data) :html-url) filename
             (replace-regexp-in-string "-mode$" "" mode)
             contents))))

(setq calendar-week-start-day 6) ;; My weeks start on Saturday

(defun my/org-get-invoice-range-based-on-date (date)
  (let* ((invoice-date (org-date-to-gregorian date))
         (start (list (1- (car invoice-date)) 1 (elt invoice-date 2)))
         (end (list (car invoice-date) 1 (elt invoice-date 2))))
    (mapcar (lambda (date)
              (format-time-string "%F" (encode-time 0 0 0 1 (elt date 0) (elt date 2))))
            (list start end))))

(defun my/org-quantified-get-hours-based-on-range (category start end)
  "Return the number of hours for the specified category."
  (/ (assoc-default category
                    (quantified-summarize-time start end)) 3600.0))

;; TODO: paginate
(defun my/org-quantified-get-detailed-hours-based-on-range (category start end)
  "Return a list of (date week-ending-date dow seconds) for CATEGORY from START to END."
  (let ((entries
         (gethash "entries"
                  (quantified-parse-json
                   (quantified-request (format "records.json?start=%s&end=%s&filter_string=%s&per_page=1000&split=split" start end (url-encode-url category))
                                       nil "GET")))))
    (mapcar
     (lambda (entry)
       (let ((time (date-to-time (gethash "timestamp" entry))))
         (list
          (format-time-string "%F" time)
          (format-time-string "%F" (my/get-week-end-for-time time))
          (format-time-string "%a" time)
          (gethash "duration" entry))))
     entries)))

(defun my/get-week-end-for-time (time &optional week-ends-on-day)
  "WEEK-ENDS-ON-DAY: 0 is Sunday"
  (let* ((decoded (decode-time time))
         (dow (elt decoded 6))
         (end-week (or week-ends-on-day (% (+ 6 calendar-week-start-day) 7))))
    (encode-time
     (elt decoded 0)
     (elt decoded 1)
     (elt decoded 2)
     (+ (elt decoded 3)
        (% (+ 7 (- end-week dow)) 7))
     (elt decoded 4)
     (elt decoded 5))))

(ert-deftest my/org-get-week-ending-date ()
  (let ((calendar-week-start-day 6)
        (tests '(
                 ("2015-09-03" . "2015-09-04")
                 ("2015-12-01" . "2015-12-04")
                 ("2015-12-03" . "2015-12-04")
                 ("2015-12-04" . "2015-12-04")
                 ("2015-12-05" . "2015-12-11"))))
    (dolist (test tests)
      (should (string=
               (format-time-string
                "%F"
                (my/get-week-end-for-time (org-time-string-to-time (car test))))
               (cdr test)))
      (should (string=
               (format-time-string
                "%F"
                (my/get-week-end-for-time (org-time-string-to-time (car test)) 5))
               (cdr test))))))



(defun my/org-quantified-format-detailed-hours-as-table (list)
  "Return a table with rows for LIST.
  | Week ending ____ | Sat | Sun | Mon | Tue | Wed | Thu | Fri | Total |
  LIST elements should be in the form (date week-end-date dow seconds).
  See `my/org-quantified-get-detailed-hours-based-on-range'."
  ;; Group by week ending date
  (let ((days '("Sat" "Sun" "Mon" "Tue" "Wed" "Thu" "Fri")))
    (append
     (list (append '("Week ending") days '("Total")))
     (mapcar
      (lambda (row)
        (let ((day-values (-group-by (lambda (x) (elt x 2)) (cdr row)))
              (week-total 0))
          (append
           (list (format "Week ending %s" (format-time-string "%b %-e" (org-time-string-to-time (car row)))))
           (mapcar (lambda (day)
                     (if (assoc-default day day-values)
                         (format "%.1f"
                                 (apply '+
                                        (mapcar
                                         (lambda (day-val) (/ (elt day-val 3) 3600.0))
                                         (assoc-default day day-values))))
                       ""))
                   days)
           (list (format "%.1f"
                         (apply '+ (mapcar (lambda (day-val) (/ (elt day-val 3) 3600.0)) (cdr row)))))
           ))
        )
      (-sort (lambda (a b) (string< (car a) (car b))) (-group-by (lambda (x) (elt x 1)) list))))))


(defun my/org-quantified-hours-table ()
  (my/org-quantified-format-detailed-hours-as-table
   (apply 'my/org-quantified-get-detailed-hours-based-on-range
          (org-entry-get-with-inheritance "QUANTIFIED_CATEGORY")
          (my/org-get-invoice-range-based-on-date (org-entry-get-with-inheritance "INVOICE_DATE")))))

(ert-deftest my/org-get-invoice-range-based-on-date ()
  "Check if invoice range is sane."
  (should (equal (my/org-get-invoice-range-based-on-date "2015-12-05")
                 '("2015-11-01" "2015-12-01"))))

(defun my/org-archive-done-tasks ()
  "Archive finished or cancelled tasks."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "TODO=\"DONE\"|TODO=\"CANCELLED\"" (if (org-before-first-heading-p) 'file 'tree)))

(use-package org-fstree
  :commands (org-fstree-apply-maybe org-fstree-show-entry-maybe)
  :config
  (progn (add-hook 'org-ctrl-c-ctrl-c-hook 'org-fstree-apply-maybe)
         (add-hook 'org-pre-cycle-hook 'org-fstree-show-entry-maybe)))

(use-package ox-reveal :disabled t)

(defun my/org-add-dashes-to-tag-regexps ()
  (setq org-complex-heading-regexp
        (concat "^\\(\\*+\\)"
                "\\(?: +" org-todo-regexp "\\)?"
                "\\(?: +\\(\\[#.\\]\\)\\)?"
                "\\(?: +\\(.*?\\)\\)??"
                (org-re "\\(?:[ \t]+\\(:[-[:alnum:]_@#%:]+:\\)\\)?")
                "[ \t]*$")
        org-complex-heading-regexp-format
        (concat "^\\(\\*+\\)"
                "\\(?: +" org-todo-regexp "\\)?"
                "\\(?: +\\(\\[#.\\]\\)\\)?"
                "\\(?: +"
                ;; Stats cookies can be stuck to body.
                "\\(?:\\[[0-9%%/]+\\] *\\)*"
                "\\(%s\\)"
                "\\(?: *\\[[0-9%%/]+\\]\\)*"
                "\\)"
                (org-re "\\(?:[ \t]+\\(:[-[:alnum:]_@#%%:]+:\\)\\)?")
                "[ \t]*$")
        org-todo-line-tags-regexp
        (concat "^\\(\\*+\\)"
                "\\(?: +" org-todo-regexp "\\)?"
                "\\(?: +\\(.*?\\)\\)??"
                (org-re "\\(?:[ \t]+\\(:[-[:alnum:]:_@#%]+:\\)\\)?")
                "[ \t]*$")))
(add-hook 'org-mode-hook 'my/org-add-dashes-to-tag-regexps)

(defun my/read-phone-entries ()
  "Copy phone data to a summary Org file."
  (interactive)
  (mapc
   (lambda (filename)
     (let ((base (file-name-base filename)) contents timestamp category encoded-time date)
       (when (string-match "^[^ ]+ [^ ]+ \\([^ ]+\\) - \\(.*\\)" base)
         (setq time (seconds-to-time (/ (string-to-number (match-string 1 base)) 1000))
               encoded-time (decode-time time)
               date (list (elt encoded-time 4) (elt encoded-time 3) (elt encoded-time 5))
               category (match-string 2 base))
         (with-temp-buffer
           (insert-file-contents filename)
           (setq contents (s-trim (buffer-string))))
         (with-current-buffer
             (find-file "~/dropbox/tasker/summary.txt")
           (org-datetree-find-date-create date)
           (unless (save-excursion (re-search-forward (regexp-quote base) nil t))
             (goto-char (line-end-position))
             (insert "\n")
             (insert "**** " contents "  :" category ":\n" base "\n")
             (insert (format-time-string "[%Y-%m-%d %a %H:%M]\n" time))

             (if (member category '("Think" "Do"))
                 (save-excursion
                   (org-back-to-heading t)
                   (if (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
                   (unless (looking-at org-todo-regexp)
                     (org-todo "TODO"))))
             (if (string-match "^Energy \\([0-9]\\)" contents)
                 (org-set-property "ENERGY" (match-string 1 contents)))))
         (delete-file filename))))
   (directory-files "~/dropbox/tasker/data" t "\\.txt$")))

(defun my/org-package-open (package-name)
  (interactive "MPackage name: ")
  (describe-package (intern package-name)))

(ert-deftest my/org-package-export ()
  (should
   (string=
    (my/org-package-export "transcribe" "transcribe" 'html)
    "<a target=\"_blank\" href=\"https://elpa.gnu.org/packages/transcribe.html\">transcribe</a>"
    ))
  (should
   (string=
    (my/org-package-export "fireplace" "fireplace" 'html)
    "<a target=\"_blank\" href=\"http://melpa.org/#/fireplace\">fireplace</a>"
    )))
(defun my/org-package-export (link description format)
  (let* ((package-info (car (assoc-default (intern link) package-archive-contents)))
         (package-source (package-desc-archive package-info))
         (path (format
                (cond
                 ((string= package-source "gnu") "https://elpa.gnu.org/packages/%s.html")
                 ((string= package-source "melpa") "http://melpa.org/#/%s")
                 (t (throw 'unknown-source)))
                link))
         (desc (or description link)))
    (cond
     ((eq format 'html) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
     ((eq format 'wp) (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
     ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
     ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
     ((eq format 'ascii) (format "%s <%s>" desc path))
     (t path))))

(org-link-set-parameters "package" :follow 'my/org-package-open :export 'my/org-package-export)

(setq org-ascii-links-to-notes nil)

(defun my/org-list-from-rss (url date)
      "Convert URL to an Org list"
      (with-current-buffer (url-retrieve-synchronously url)
        (re-search-forward "<\\?xml")
        (goto-char (match-beginning 0))
        (let ((feed (xml-parse-region (point) (point-max))))
          (mapconcat (lambda (entry)
            (format "- [[%s][%s]]"
             (xml-get-attribute (car (xml-get-children entry 'link)) 'href)
             (elt (car (xml-get-children entry 'title)) 2)))
            (-filter (lambda (entry)
              (org-string<= date (elt (car (xml-get-children entry 'updated)) 2)))
                     (xml-get-children (car feed) 'entry)) "\n"))))

(defun my/describe-packages (list)
  "Return an Org list of package descriptions for LIST."
  (mapconcat
   (lambda (symbol)
     (let ((package-desc (assoc symbol package-archive-contents)))
       (if package-desc
         (format "- [[package:%s][%s]]: %s"
                 (symbol-name symbol)
                 (symbol-name symbol)
                 (package-desc-summary (cadr package-desc)))
         "")))
   list
   "\n"))

  (defun my/list-new-packages ()
;     (package-refresh-contents)
     (with-temp-buffer
       (insert-file-contents "~/.emacs.d/.package-list")
       (goto-char (point-min))
       (my/describe-packages
        (-difference (mapcar 'car package-archive-contents)
                     (read (current-buffer))))))
  (defun my/save-new-packages ()
   (interactive)
   (when package-archive-contents
   (with-temp-buffer
     (find-file "~/.emacs.d/.package-list")
     (erase-buffer)
     (prin1 (mapcar 'car package-archive-contents) (current-buffer))
     (save-buffer)
     nil)))

(defun my/share-emacs-news ()
    "Prepare current subtree for yanking into post."
    (interactive)
    ;; Draft Gnus article
    (save-restriction
      (org-narrow-to-subtree)
      (let ((org-export-html-preamble nil)
            (org-html-toplevel-hlevel 3)
            output)
        (setq output
              (apply
               'format
               "<#multipart type=alternative>
<#part type=\"text/plain\" disposition=inline>
%s
<#/part>
<#part type=\"text/html\" disposition=inline>
%s
<#/part>
<#/multipart>
<#part type=\"text/x-org\" disposition=attachment name=\"emacs-news.org\">
%s
<#/part>
"
               (mapcar
                (lambda (format)
                  (org-export-string-as (buffer-substring (point-min) (point-max)) format t))
                '(ascii html org))))
        (kill-new output)))
  (gnus))

(require 'widget)
     (defun my/check-widget-value (widget &rest ignore)
       "Provide visual feedback for WIDGET."
       (cond
        ((string= (widget-value widget) "?")
         ;; Asking for hint
         (message "%s" (widget-get widget :correct))
         (widget-value-set widget ""))
        ;; Use string-match to obey case-fold-search
        ((string-match
          (concat "^"
                  (regexp-quote (widget-get widget :correct))
                  "$")
          (widget-value widget))
         (message "Correct")
         (goto-char (widget-field-start widget))
         (goto-char (line-end-position))
         (insert "✓")
         (widget-forward 1)
         )))

   (defun my/make-fill-in-quiz (&optional quiz-table)
     "Create an fill-in quiz for the Org table at point.
The Org table's first column should have the questions and the second column
should have the answers."
     (interactive (list (org-babel-read-table)))
     (with-current-buffer (get-buffer-create "*Quiz*")
       (kill-all-local-variables)
       (let ((inhibit-read-only t))
         (erase-buffer))
       (remove-overlays)
       (mapc (lambda (row)
               (widget-insert (car row))
               (widget-insert "\t")
               (widget-create 'editable-field
                              :size 15
                              :correct (cadr row)
                              :notify 'my/check-widget-value)
               (widget-insert "\n"))
             quiz-table)
       (widget-create 'push-button
                      :table quiz-table
                      :notify (lambda (widget &rest ignore)
                                (my/make-fill-in-quiz (widget-get widget :table)))
                      "Reset")
       (use-local-map widget-keymap)
       (widget-setup)
       (goto-char (point-min))
       (widget-forward 1)
       (switch-to-buffer (current-buffer))))

;; from FAQ at http://web-mode.org/ for smartparens
(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
          )))

(setq-default tab-width 2)

(global-set-key (kbd "RET") 'newline-and-indent)

(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)

(when (eq system-type 'windows-nt)
	(setenv "CYGWIN" "nodosfilewarning")
  (setq shell-file-name "C:/emacs/libexec/emacs/24.4/i686-pc-mingw32/cmdproxy.exe")
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t))

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region)
  ("C-<prior>" . er/expand-region)
  ("C-<next>" . er/contract-region))

(eval-after-load 'python-mode
  '(bind-key "C-c C-c" 'compile python-mode-map))

(use-package "eldoc"
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;; C-c C-v l : elint current buffer in clean environment.
;; C-c C-v L : elint current buffer by multiple emacs binaries.
;;             See `erefactor-lint-emacsen'
;; C-c C-v r : Rename symbol in current buffer.
;;             Resolve `let' binding as long as i can.
;; C-c C-v R : Rename symbol in requiring modules and current buffer.
;; C-c C-v h : Highlight current symbol in this buffer
;;             and suppress `erefacthr-highlight-mode'.
;; C-c C-v d : Dehighlight all by above command.
;; C-c C-v c : Switch prefix bunch of symbols.
;;             ex: '(hoge-var hoge-func) -> '(foo-var foo-func)
;; C-c C-v ? : Display flymake elint warnings/errors

  (use-package erefactor
    :config
    (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map))

  (use-package paredit)
  (use-package redshank
    :disabled t
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook 'redshank-mode))

(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
(bind-key "C-c f" 'find-function)

(defun my/sort-sexps-in-region (beg end)
    "Can be handy for sorting out duplicates.
Sorts the sexps from BEG to END. Leaves the point at where it
couldn't figure things out (ex: syntax errors)."
    (interactive "r")
    (let ((input (buffer-substring beg end))
          list last-point form result)
      (save-restriction
        (save-excursion
          (narrow-to-region beg end)
          (goto-char (point-min))
          (setq last-point (point-min))
          (setq form t)
          (while (and form (not (eobp)))
            (setq form (ignore-errors (read (current-buffer))))
            (when form
              (add-to-list
               'list
               (cons
                (prin1-to-string form)
                (buffer-substring last-point (point))))
              (setq last-point (point))))
          (setq list (sort list (lambda (a b) (string< (car a) (car b)))))
          (delete-region (point-min) (point))
          (insert (mapconcat 'cdr list "\n"))))))

(bind-key "M-:" 'pp-eval-expression)

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)

(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (progn
    (yas-global-mode)
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (setq yas-key-syntaxes '("w_" "w_." "^ "))
    (setq yas-installed-snippets-dir "~/elisp/yasnippet-snippets")
    (setq yas-expand-only-for-last-commands nil)
    (yas-global-mode 1)
    (bind-key "\t" 'hippie-expand yas-minor-mode-map)
    (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt)))
;;        (global-set-key (kbd "C-c y") (lambda () (interactive)
;;                                         (yas/load-directory "~/elisp/snippets")))

(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))

(setq default-cursor-color "gray")
(setq yasnippet-can-fire-cursor-color "purple")

;; It will test whether it can expand, if yes, cursor color -> green.
(defun yasnippet-can-fire-p (&optional field)
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
                                                      (yas--field-end field))
                                    (yas--templates-for-key-at-point))
                                (yas--templates-for-key-at-point))))
    (and templates-and-pos (first templates-and-pos))))

(defun my/change-cursor-color-when-can-expand (&optional field)
  (interactive)
  (when (eq last-command 'self-insert-command)
    (set-cursor-color (if (my/can-expand)
                          yasnippet-can-fire-cursor-color
                        default-cursor-color))))

(defun my/can-expand ()
  "Return true if right after an expandable thing."
  (or (abbrev--before-point) (yasnippet-can-fire-p)))

                                        ; As pointed out by Dmitri, this will make sure it will update color when needed.
(remove-hook 'post-command-hook 'my/change-cursor-color-when-can-expand)

(defun my/insert-space-or-expand ()
  "For binding to the SPC SPC keychord."
  (interactive)
  (condition-case nil (or (my/hippie-expand-maybe nil) (insert "  "))))

(defun my/hippie-expand-maybe (arg)
  "Try to expand text before point, using multiple methods.
The expansion functions in `hippie-expand-try-functions-list' are
tried in order, until a possible expansion is found.  Repeated
application of `hippie-expand' inserts successively possible
expansions.
With a positive numeric argument, jumps directly to the ARG next
function in this list.  With a negative argument or just \\[universal-argument],
undoes the expansion."
  (interactive "P")
  (require 'hippie-exp)
  (if (or (not arg)
          (and (integerp arg) (> arg 0)))
      (let ((first (or (= he-num -1)
                       (not (equal this-command last-command)))))
        (if first
            (progn
              (setq he-num -1)
              (setq he-tried-table nil)))
        (if arg
            (if (not first) (he-reset-string))
          (setq arg 0))
        (let ((i (max (+ he-num arg) 0)))
          (while (not (or (>= i (length hippie-expand-try-functions-list))
                          (apply (nth i hippie-expand-try-functions-list)
                                 (list (= he-num i)))))
            (setq i (1+ i)))
          (setq he-num i))
        (if (>= he-num (length hippie-expand-try-functions-list))
            (progn (setq he-num -1) nil)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Using %s"
                       (nth he-num hippie-expand-try-functions-list)))))
    (if (and (>= he-num 0)
             (eq (marker-buffer he-string-beg) (current-buffer)))
        (progn
          (setq he-num -1)
          (he-reset-string)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Undoing expansions"))))))

(column-number-mode 1)

(setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-git-diff-switches nil)

(add-to-list 'auto-mode-alist '("\\.js\\'\\|\\.json\\'" . js2-mode))

(use-package js2-mode
  :config
  (bind-key "C-c C-c" 'compile js2-mode-map)
  (add-hook 'js2-mode-hook 'jasminejs-mode))

(use-package coffee-mode
  :config
  (bind-key "C-c C-c" 'compile coffee-mode-map)
  )

(use-package jasminejs-mode
  :config
  (add-hook 'jasminejs-mode-hook 'jasminejs-add-snippets-to-yas-snippet-dirs))

(defvar my/javascript-test-regexp (concat (regexp-quote "/** Testing **/") "\\(.*\n\\)*")
	"Regular expression matching testing-related code to remove.
See `my/copy-javascript-region-or-buffer'.")

(defun my/copy-javascript-region-or-buffer (beg end)
	"Copy the active region or the buffer, wrapping it in script tags.
Add a comment with the current filename and skip test-related
code. See `my/javascript-test-regexp' to change the way
test-related code is detected."
	(interactive "r")
	(unless (region-active-p)
		(setq beg (point-min) end (point-max)))
	(kill-new
	 (concat
		"<script type=\"text/javascript\">\n"
		(if (buffer-file-name) (concat "// " (file-name-nondirectory (buffer-file-name)) "\n") "")
		(replace-regexp-in-string
		 my/javascript-test-regexp
		 ""
		 (buffer-substring (point-min) (point-max))
		 nil)
		"\n</script>")))

(defvar my/debug-counter 1)
(defun my/insert-or-flush-debug (&optional reset beg end)
  (interactive "pr")
  (cond
   ((= reset 4)
    (save-excursion
      (flush-lines "console.log('DEBUG: [0-9]+" (point-min) (point-max))
      (setq my/debug-counter 1)))
   ((region-active-p)
    (save-excursion
      (goto-char end)
      (insert ");\n")
      (goto-char beg)
      (insert (format "console.log('DEBUG: %d', " my/debug-counter))
      (setq my/debug-counter (1+ my/debug-counter))
      (js2-indent-line)))
   (t
    ;; Wrap the region in the debug
    (insert (format "console.log('DEBUG: %d');\n" my/debug-counter))
    (setq my/debug-counter (1+ my/debug-counter))
    (backward-char 3)
    (js2-indent-line))))

(use-package js2-mode
  :commands js2-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    (setq-default js2-basic-offset 2)
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode)))
  :config
  (progn
    (js2-imenu-extras-setup)
    (bind-key "C-x C-e" 'js-send-last-sexp js2-mode-map)
    (bind-key "C-M-x" 'js-send-last-sexp-and-go js2-mode-map)
    (bind-key "C-c b" 'js-send-buffer js2-mode-map)
    (bind-key "C-c d" 'my/insert-or-flush-debug js2-mode-map)
    (bind-key "C-c C-b" 'js-send-buffer-and-go js2-mode-map)
    (bind-key "C-c w" 'my/copy-javascript-region-or-buffer js2-mode-map)))

(use-package coffee-mode
:config (setq-default coffee-js-mode 'js2-mode coffee-tab-width 2))

(defun my/clean-up-spans-in-region (beg end)
  (interactive "r")
  (save-excursion
    (let ((changed t))
      (while changed
        (setq changed nil)
        (goto-char beg)
        (while (re-search-forward "<span>\\([^<]*\\)</span>" end t)
          (replace-match "\\1")
          (setq changed t)))
      (setq changed t)
      (while changed
        (setq changed nil)
        (goto-char beg)
        (while (re-search-forward "<span>*\\(<a[^<]+>[^<]*</a>\\)</span>" end t)
          (replace-match "\\1")
          (setq changed t))))))

(defun my/clean-up-spans-in-string (string)
  (with-temp-buffer
    (insert string)
    (my/clean-up-spans-in-region (point-min) (point-max))
    (buffer-string)))

(ert-deftest my/clean-up-spans-in-string ()
  (should (string= (my/clean-up-spans-in-string "<span><span>Hello world</span></span>")
          "Hello world"))
  (should (string= (my/clean-up-spans-in-string "<span><span><a href=\"http://example.com\">Hello another world</a></span></span>")
                   "<a href=\"http://example.com\">Hello another world</a>"))
  (should (string= (my/clean-up-spans-in-string "<span><h1>Leave alone</h1></span>") "<span><h1>Leave alone</h1></span>"))
  (should (string= (my/clean-up-spans-in-string "<span><a href=\"http://example.com\">Leave</a> alone</span>")
                   "<span><a href=\"http://example.com\">Leave</a> alone</span>")))

;; (ert "my/clean-up-spans-in-string")

(defun my/magit-commit-all ()
      "Publish the current file and commit all the current changes."
      (interactive)
      (my/org-publish-maybe)
      (magit-status default-directory)
      (magit-stage-all)
      (call-interactively 'magit-log-edit))

    (use-package magit
      :load-path "~/elisp/magit"
      :config
      (progn
        (when (equal system-type 'windows-nt)
          (setq magit-git-executable "c:/program files (x86)/git/bin/git.exe"))
        (setq magit-diff-options '("-b")) ; ignore whitespace
        (define-key magit-mode-map "#gg" 'endless/load-gh-pulls-mode)
        (defvar my/magit-limit-to-directory nil "Limit magit status to a specific directory.")
        (defun my/magit-status-in-directory (directory)
          "Displays magit status limited to DIRECTORY.
  Uses the current `default-directory', or prompts for a directory
  if called with a prefix argument. Sets `my/magit-limit-to-directory'
  so that it's still active even after you stage a change. Very experimental."
          (interactive (list (expand-file-name
                              (if current-prefix-arg
                                  (read-directory-name "Directory: ")
                                default-directory))))
          (setq my/magit-limit-to-directory directory)
          (magit-status directory))

        (defadvice magit-insert-untracked-files (around sacha activate)
          (if my/magit-limit-to-directory
              (magit-with-section (section untracked 'untracked "Untracked files:" t)
                (let ((files (cl-mapcan
                              (lambda (f)
                                (when (eq (aref f 0) ??) (list f)))
                              (magit-git-lines
                               "status" "--porcelain" "--" my/magit-limit-to-directory))))
                  (if (not files)
                      (setq section nil)
                    (dolist (file files)
                      (setq file (magit-decode-git-path (substring file 3)))
                      (magit-with-section (section file file)
                        (insert "\t" file "\n")))
                    (insert "\n"))))
            ad-do-it))

        (defadvice magit-insert-unstaged-changes (around sacha activate)
          (if my/magit-limit-to-directory
              (let ((magit-current-diff-range (cons 'index 'working))
                    (magit-diff-options (copy-sequence magit-diff-options)))
                (magit-git-insert-section (unstaged "Unstaged changes:")
                    #'magit-wash-raw-diffs
                  "diff-files"
                  "--" my/magit-limit-to-directory
                  ))
            ad-do-it))

        (defadvice magit-insert-staged-changes (around sacha activate)
          "Limit to `my/magit-limit-to-directory' if specified."
          (if my/magit-limit-to-directory
              (let ((no-commit (not (magit-git-success "log" "-1" "HEAD"))))
                (when (or no-commit (magit-anything-staged-p))
                  (let ((magit-current-diff-range (cons "HEAD" 'index))
                        (base (if no-commit
                                  (magit-git-string "mktree")
                                "HEAD"))
                        (magit-diff-options (append '("--cached") magit-diff-options)))
                    (magit-git-insert-section (staged "Staged changes:")
                        (apply-partially #'magit-wash-raw-diffs t)
                      "diff-index" "--cached" base "--" my/magit-limit-to-directory))))
            ad-do-it)))
      :bind (("C-x v d" . magit-status)
             ("C-x v C-d" . my/magit-status-in-directory)
             ("C-x v p" . magit-push)
             ("C-x v c" . my/magit-commit-all)))

;; From http://endlessparentheses.com/merging-github-pull-requests-from-emacs.html
(defun endless/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1)
  (magit-gh-pulls-reload))

(use-package magit-gh-pulls)

(use-package git-messenger
  :bind (("C-x v m" . git-messenger:popup-message)))

(defun my/recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or its parents."
  (setq directory (or directory (file-name-directory (buffer-file-name)) (pwd)))
  (if (file-exists-p (expand-file-name file directory))
      (expand-file-name file directory)
    (unless (string= directory "/")
      (my/recursive-find-file file (expand-file-name ".." directory)))))

(defun my/find-tags ()
  "Set the TAGS file."
  (set (make-variable-buffer-local 'tags-table-list) nil)
  (set (make-variable-buffer-local 'tags-file-name)
       (my/recursive-find-file "TAGS")))

(eval-after-load 'drupal-mode
  '(progn
     (add-hook 'drupal-mode-hook 'my/find-tags)))

(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node-modules"))
  :config
  (projectile-global-mode))
(use-package helm-projectile)



(use-package rinari)
(use-package bundler)
    (use-package robe
      :init
		  (progn (add-hook 'ruby-mode-hook 'robe-mode)
             (add-hook 'robe-mode-hook 'ac-robe-setup)
             (add-hook 'ruby-mode-hook 'auto-complete-mode)))

(defun my/rspec-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (rspec-run-single-file
   (concat
     (rspec-spec-file-for (buffer-file-name))
     ":"
     (save-restriction
               (widen)
               (number-to-string (line-number-at-pos))))
   (rspec-core-options)))

(use-package rspec-mode
  :config
  (progn
    (setq rspec-command-options "--fail-fast --format documentation")
    (bind-key "C-c , ," 'rspec-rerun rspec-mode-map)
    (fset 'rspec-verify-single 'my/rspec-verify-single)))

(add-hook 'sass-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(setq-default indent-tabs-mode nil)

(use-package skewer-mode
  :config (skewer-setup))

(use-package company
  :config (add-hook 'prog-mode-hook 'company-mode))

(use-package tern
  :config
  (bind-key "C-c C-c" 'compile tern-mode-keymap)
  (when (eq system-type 'windows-nt) (setq tern-command '("cmd" "/c" "tern")))
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package company-tern
:init (add-to-list 'company-backends 'company-tern))

(use-package erc
  :config
  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  (setq erc-autojoin-channels-alist '(("freenode.net"
   			"#org-mode"
   			"#hacklabto"
   			"#emacs"
         "#emacs-beginners"
         "#emacs-ops"))
 erc-server "irc.freenode.net"
 erc-nick "sachac")
  (defun erc-cmd-OPME ()
    "Request chanserv to op me."
    (erc-message "PRIVMSG"
                 (format "chanserv op %s %s"
                         (erc-default-target)
                         (erc-current-nick)) nil))

  (defun erc-cmd-DEOPME ()
    "Deop myself from current channel."
    (erc-cmd-DEOP (format "%s" (erc-current-nick))))
  )

(defmacro my/org-with-current-task (&rest body)
  "Execute BODY with the point at the subtree of the current task."
  `(if (derived-mode-p 'org-agenda-mode)
       (save-window-excursion
         (org-agenda-switch-to)
         ,@body)
     ,@body))

(defun my/org-clock-in-and-track ()
  "Start the clock running. Clock into Quantified Awesome."
  (interactive)
  (my/org-with-current-task
   (org-clock-in)
   (call-interactively 'my/org-quantified-track)
   (when (org-entry-get (point) "AUTO")
     (org-open-link-from-string (org-entry-get (point) "AUTO")))))
(bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)

(defmacro my/with-org-task (&rest body)
  "Run BODY within the current agenda task, clocked task, or cursor task."
  `(cond
    ((derived-mode-p 'org-agenda-mode)
     (let* ((marker (org-get-at-bol 'org-marker))
            (buffer (marker-buffer marker))
            (pos (marker-position marker)))
       (with-current-buffer buffer
         (save-excursion
           (save-restriction
             (widen)
             (goto-char pos)
             ,@body)))))
    ((and (derived-mode-p 'org-mode) (org-at-heading-p)) (save-excursion ,@body))
    ((org-clocking-p) (save-excursion (org-clock-goto) ,@body))
    ((derived-mode-p 'org-mode) ,@body)))

(defun my/org-quantified-track (&optional category note)
  "Create a tracking record using CATEGORY and NOTE.
Default to the current task in the agenda, the currently-clocked
entry, or the current subtree in Org."
  (interactive (list nil nil))
  (unless (and category note)
    (my/with-org-task
     (setq category (or category
                        (org-entry-get-with-inheritance "QUANTIFIED")))
     (cond
      ((null category)
       (setq category (read-string "Category: "))
       (org-set-property "QUANTIFIED" category))
      ((string= category "ask")
       (setq category (read-string "Category: "))))
     (setq note
           (concat
            (if (string= (or (org-entry-get-with-inheritance "QUANTIFIEDQUIET") "") "t")
                "!private "
              "")
            (or note (elt (org-heading-components) 4) (read-string "Note: "))))))
  (quantified-track (concat category " | " note)))

(defun my/org-quick-clock-in-task (location jump)
  "Track and clock in on the specified task.
If JUMP is non-nil or the function is called with the prefix argument, jump to that location afterwards."
  (interactive (list (save-excursion (my/org-refile-get-location "Location")) current-prefix-arg))
  (when location
    (if jump
        (progn (org-refile 4 nil location) (my/org-clock-in-and-track))
      (save-window-excursion
        (org-refile 4 nil location)
        (my/org-clock-in-and-track)))))
(bind-key "C-c q" 'my/org-quick-clock-in-task)

(require 'quantified)

(defun my/compare-times (clocked estimated)
  (if (and (> (length clocked) 0) estimated)
      (format "%.2f"
            (/ (* 1.0 (org-hh:mm-string-to-minutes clocked))
               (org-hh:mm-string-to-minutes estimated)))
    ""))

(defvar my/workrave-file (expand-file-name ".\\Workrave\\historystats" (getenv "AppData")))

(defun my/workrave-transform-statistics (&optional file)
  (interactive (list my/workrave-file))
  (with-current-buffer (find-file-noselect file)
  ;; D day month-1 year hour min day month-1 year hour min
    (let ((result "Date\tStart\tEnd\tClicks\tKeystrokes\n"))
      (goto-char (point-min))
      (while (re-search-forward "^D \\(.*\\)" nil t)
  (let ((dates (split-string (match-string 1))))
    (if (re-search-forward "^m \\(.*\\)" nil t)
        (let ((info (split-string (match-string 1))))
    (setq result
          (concat result
            (format "%d-%d-%s\t%s:%02d\t%s:%02d\t%s\t%s\n"
              (+ 1900 (string-to-number (elt dates 2))) ; year
              (1+ (string-to-number (elt dates 1))) ; month
              (elt dates 0) ; day
              (elt dates 3) ; start hour
              (string-to-number (elt dates 4)) ; start min
              (elt dates 8) ; end hour
              (string-to-number (elt dates 9)) ; end min
              (elt info 5) ; clicks
              (elt info 6) ; keystrokes
              )))))))
      (if (interactive-p)
    (kill-new result)
  result))))

(defun my/strip-blog-share ()
  (interactive)
  (let (base)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "<div class=\"sharedaddy sd-sharing-enabled\">.*?<div class=\"sharing-clear\"></div></div></div></div>" nil t)
        (replace-match "")))))

(defun my/artrage-export-png (directory &optional prefix)
          "Change an Artrage script file (arscript) to export images to DIRECTORY.
    If PREFIX is specified, use that instead of image-."
          (interactive "MPath: ")
          (unless (file-directory-p directory)
            (make-directory directory t))
          (while (re-search-forward "[0-9\\.]+s" nil t)
            (replace-match "0.000s"))
          (goto-char (point-min))
          (while (search-forward "<StrokeEvent>" nil t)
            (replace-match (concat
                            "EvType: Command    CommandID: ExportLayer    Idx: -1    Channels: NO    Path: \""
                            directory
                            "/" (or prefix "image-")
                            ".png\"
<StrokeEvent>") t t)))

(setq gnutls-trustfiles '("c:/sacha/cacert.pem.txt"))

(defadvice face-attribute (around sacha activate)
  (if (symbolp (ad-get-arg 0))
      ad-do-it))

(defadvice ido-sort-mtime (around sacha activate)
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (let ((ta (or (nth 5 (file-attributes (concat ido-current-directory a))) '(0 0)))
                      (tb (or (nth 5 (file-attributes (concat ido-current-directory b))) '(0 0))))
                  (if (= (nth 0 ta) (nth 0 tb))
                      (> (nth 1 ta) (nth 1 tb))
                    (> (nth 0 ta) (nth 0 tb)))))))
  (setq ad-return-value
        (ido-to-end  ;; move . files to end (again)
         (delq nil (mapcar
                    (lambda (x) (if (string-equal (substring x 0 1) ".") x))
                    ido-temp-list)))))

;(setq eimp-mogrify-program "c:/Program Files/ImageMagick-6.8.3-Q16/mogrify.exe")

(defun my/ssh-refresh ()
  "Reset the environment variable SSH_AUTH_SOCK"
  (interactive)
  (let (ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))
    (setenv "SSH_AUTH_SOCK"
            (car (split-string
                  (shell-command-to-string
                   "ls -t $(find /tmp/ssh-* -user $USER -name 'agent.*' 2> /dev/null)"))))
    (message
     (format "SSH_AUTH_SOCK %s --> %s"
             ssh-auth-sock-old (getenv "SSH_AUTH_SOCK")))))
(my/ssh-refresh)

(defun sanityinc/adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(setq browse-url-firefox-program
      "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")

(setq browse-url-generic-program "google-chrome")
(setq browse-url-browser-function 'browse-url-generic)

(use-package clipmon
  :disabled t
  :init (progn (setq clipmon-action 'kill-new clipmon-timeout nil clipmon-sound nil clipmon-cursor-color nil clipmon-suffix nil) (clipmon-mode)))

(use-package engine-mode
  :config
  (progn
    (defengine my-blog "https://www.google.ca/search?q=site:sachachua.com+%s" :keybinding "b")
    (defengine my-photos "http://www.flickr.com/search/?w=65214961@N00&q=%s" :keybinding "f")
    (defengine mail "https://mail.google.com/mail/u/0/#search/%s" :keybinding "m")
    (defengine google "http://google.com/search?q=%s" :keybinding "g")
    (defengine emacswiki "http://google.com/search?q=site:emacswiki.org+%s" :keybinding "e")
    (bind-key* "C-c /" 'my/engine-mode-hydra/body)
    (engine-mode)))

(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '((nntp "news.gmane.org")
        ;; (nnmaildir "mail"
        ;;            (directory "~/Maildir")
        ;;            (directory-files nnheader-directory-files-safe)
        ;;            (get-new-mail nil))
        ;; (nnimap "imap.gmail.com"
        ;;         (nnimap-address "imap.gmail.com")
        ;;         (nnimap-server-port 993)
        ;;         (nnimap-stream ssl)
        ;;         (nnimap-authenticator login))
        (nnimap "localhost"
          (nnimap-address "localhost")
          (nnimap-stream network)
          (nnimap-user "sacha")
          (nnimap-authenticator login)
          (nnimap-authinfo-file "~/.authinfo.gpg"))))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("localhost" 25 "sacha@local.sachachua.com" nil))
      smtpmail-auth-credentials '(("localhost" 25 "sacha@local.sachachua.com" nil))
      smtpmail-default-smtp-server "localhost"
      smtpmail-smtp-server "localhost"
      smtpmail-smtp-service 25
      smtpmail-local-domain "local.sachachua.com")
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "127.0.0.1")
(setq smtpmail-smtp-service 25)
(setq user-mail-address "sacha@sachachua.com")

(use-package gnus
:config
(require 'mm-decode)
(setq mm-discouraged-alternatives
      '("text/html" "text/richtext")
      mm-automatic-display
      (-difference mm-automatic-display '("text/html" "text/enriched" "text/richtext"))))

(setq gnus-treat-hide-citation t)

(setq gnus-use-adaptive-scoring t)
(setq gnus-default-adaptive-score-alist
     '((gnus-unread-mark)
			 (gnus-ticked-mark (subject 10))
       (gnus-killed-mark (subject -5))
       (gnus-catchup-mark (subject -1))))

(setq notmuch-message-headers '("Subject" "To" "Cc" "Date" "Reply-To"))

(use-package ledger-mode
:load-path "~/vendor/ledger/lisp"
:mode "\\.ledger$"
)

(defun my/ledger-go-to-beginning-of-entry ()
  "Move to the beginning of the current entry."
  (while (and (not (bobp))
              (eq (ledger-context-line-type (ledger-context-at-point))
                  'acct-transaction))
    (forward-line -1)))

(defun my/ledger-entry-date ()
  "Returns the date of the entry containing point or nil."
  (save-excursion
    (my/ledger-go-to-beginning-of-entry)
    (let ((context-info (ledger-context-other-line 0)))
      (when (eq (ledger-context-line-type context-info) 'entry)
        (goto-char (line-beginning-position))
        (if (looking-at "\\([-0-9\\./]+\\)")
            (match-string-no-properties 1))))))

(defun my/ledger-guess-mbna ()
  "Adds a sub-account for the dates for my credit card transactions."
  (interactive)
  (save-excursion
    (my/ledger-go-to-beginning-of-entry)
    (forward-line 1)
    (let ((amount 0) (date (my/ledger-entry-date)) month)
      (if (string-match "[0-9]+[-\\.]\\([0-9]+\\)[-\\.]\\([0-9]+\\)" date)
          (setq month (string-to-number (match-string 1 date))))
      ;; Is this a payment or a charge?
      (save-excursion
        (while (and (eq (ledger-context-line-type (ledger-context-at-point))
                        'acct-transaction)
                    (not (eobp)))
          (let ((context (ledger-context-at-point)))
            (if (ledger-context-field-value context 'amount)
                (if (string-match "MBNA" (ledger-context-field-value context 'account))
                    (setq amount (string-to-number (ledger-context-field-value context 'amount)))
                  (setq amount (- (string-to-number (ledger-context-field-value context 'amount)))))))
          (forward-line 1)))
      (save-excursion
        (while (and (eq (ledger-context-line-type (ledger-context-at-point))
                        'acct-transaction)
                    (not (eobp)))
          (let ((context (ledger-context-at-point)))
            (if (string-match "MBNA" (ledger-context-field-value context 'account))
                (if (re-search-forward "\\(MBNA\\)[ \t]*[-$\.0-9]*[ \t]*$" (line-end-position) t)
                    (replace-match
                     (concat "MBNA:"
                             (elt
                              '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
                              (% (+ (if (> amount 0) 10 11) month) 12)))
                             t t nil 1))))
          (forward-line 1))))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (my/setup-color-theme)))

(define-key-after global-map [menu-bar my-menu] (cons "Shortcuts" (make-sparse-keymap "Custom shortcuts")) 'tools)
(define-key global-map [menu-bar my-menu journal] '("Show journal entries" . my/show-missing-journal-entries))
(define-key global-map [menu-bar my-menu agenda] '("Org agenda" . (lambda () (interactive) (org-agenda nil "a"))))
(define-key global-map [menu-bar my-menu audio] '("Process audio" . (lambda () (interactive) (shell-command "~/bin/process-audio &"))))
(define-key global-map [menu-bar my-menu new-index-card] '("New index card" . (lambda () (interactive)
                                     (my/open-image (my/prepare-index-card-template)))))

(use-package multiple-cursors
  :bind
   (("C-c m t" . mc/mark-all-like-this)
    ("C-c m m" . mc/mark-all-like-this-dwim)
    ("C-c m l" . mc/edit-lines)
    ("C-c m e" . mc/edit-ends-of-lines)
    ("C-c m a" . mc/edit-beginnings-of-lines)
    ("C-c m n" . mc/mark-next-like-this)
    ("C-c m p" . mc/mark-previous-like-this)
    ("C-c m s" . mc/mark-sgml-tag-pair)
    ("C-c m d" . mc/mark-all-like-this-in-defun)))
(use-package phi-search)
(use-package phi-search-mc :config (phi-search-mc/setup-keys))
(use-package mc-extras :config (define-key mc/keymap (kbd "C-. =") 'mc/compare-chars))

(use-package edit-list :commands edit-list)

(use-package avy)
  ;; I use the jj key-chord for this; see the definitions for key-chord
(use-package avy-zap
  :bind
  (("M-z" . avy-zap-up-to-char-dwim)
   ("M-Z" . avy-zap-to-char-dwim)))

(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char; let's call it zap-to-isearch. The deleted region
does not include the isearch word. This is meant to be bound only
in isearch mode.  The point of this function is that oftentimes
you want to delete some portion of text, one end of which happens
to be an active isearch word. The observation to make is that if
you use isearch a lot to move the cursor around (as you should,
it is much more efficient than using the arrows), it happens a
lot that you could just delete the active region between the mark
and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plink")
  (setq tramp-auto-save-directory "c:\\sacha\\tmp"))

(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
    (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
    ;; keybinding management

    (define-key sp-keymap (kbd "C-c s r n") 'sp-narrow-to-sexp)
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

    (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

    (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

    (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

    (define-key sp-keymap (kbd "C-c s t") 'sp-prefix-tag-object)
    (define-key sp-keymap (kbd "C-c s p") 'sp-prefix-pair-object)
    (define-key sp-keymap (kbd "C-c s c") 'sp-convolute-sexp)
    (define-key sp-keymap (kbd "C-c s a") 'sp-absorb-sexp)
    (define-key sp-keymap (kbd "C-c s e") 'sp-emit-sexp)
    (define-key sp-keymap (kbd "C-c s p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "C-c s n") 'sp-add-to-next-sexp)
    (define-key sp-keymap (kbd "C-c s j") 'sp-join-sexp)
    (define-key sp-keymap (kbd "C-c s s") 'sp-split-sexp)

;;;;;;;;;;;;;;;;;;
    ;; pair management

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "<" nil :when '(my/sp-web-mode-is-code-context))

;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;<" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;>"))

;;; html-mode
    (sp-with-modes '(html-mode sgml-mode web-mode)
      (sp-local-pair "<" ">"))

;;; lisp modes
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "C-("))))

(use-package forecast
  :config
  (setq forecast-city "Toronto"
        forecast-latitude 43.6486
        forecast-longitude -79.3853
        )
  )

(setq epa-file-encrypt-to '("sacha@sachachua.com"))

(setq yaoddmuse-wikis
  '(("EmacsWiki" "https://www.emacswiki.org/emacs" utf-8 "uihnscuskc=1;")))

(defun my/describe-random-interactive-function ()
  (interactive)
  "Show the documentation for a random interactive function.
Consider only documented, non-obsolete functions."
  (let (result)
    (mapatoms
     (lambda (s)
       (when (and (commandp s)
                  (documentation s t)
                  (null (get s 'byte-obsolete-info)))
         (setq result (cons s result)))))
    (describe-function (elt result (random (length result))))))

(require 'dash)
(require 'dash-functional)
(defmacro my/convert-shell-scripts-to-interactive-commands (directory)
  "Make the shell scripts in DIRECTORY available as interactive commands."
  (cons 'progn
          (-map
           (lambda (filename)
             (let ((function-name (intern (concat "my/shell/" (file-name-nondirectory filename)))))
               `(defun ,function-name (&rest args)
                  (interactive)
                  (cond
                   ((not (called-interactively-p 'any))
                    (shell-command-to-string (mapconcat 'shell-quote-argument (cons ,filename args) " ")))
                   ((region-active-p)
                    (apply 'call-process-region (point) (mark) ,filename nil (if current-prefix-arg t nil) t args))
                   (t
                    (apply 'call-process ,filename nil (if current-prefix-arg t nil) nil args))))))
           (-filter (-not #'file-directory-p)
                    (-filter #'file-executable-p (directory-files directory t))))))

(my/convert-shell-scripts-to-interactive-commands "~/bin")

(defun my/search-irc-logs (string)
  (interactive "MSearch for: ")
  (grep (concat "grep -nH -r -P " (shell-quote-argument string) " ~/backups/server/home/.znc/users/sacha/moddata/log/freenode")))

(defun widget-button-click (event)
  "Invoke the button that the mouse is pointing at."
  (interactive "e")
  (if (widget-event-point event)
      (let* ((oevent event)
	     (mouse-1 (memq (event-basic-type event) '(mouse-1 down-mouse-1)))
	     (pos (widget-event-point event))
	     (start (event-start event))
	     (button (get-char-property
		      pos 'button (and (windowp (posn-window start))
				       (window-buffer (posn-window start)))))
	     newpoint)
	(when (or (null button)
		  (catch 'button-press-cancelled
	      ;; Mouse click on a widget button.  Do the following
	      ;; in a save-excursion so that the click on the button
	      ;; doesn't change point.
	      (save-selected-window
		(select-window (posn-window (event-start event)))
		(save-excursion
		  (goto-char (posn-point (event-start event)))
		  (let* ((overlay (widget-get button :button-overlay))
			 (pressed-face (or (widget-get button :pressed-face)
					   widget-button-pressed-face))
			 (face (overlay-get overlay 'face))
			 (mouse-face (overlay-get overlay 'mouse-face)))
		    (unwind-protect
			;; Read events, including mouse-movement
			;; events, waiting for a release event.  If we
			;; began with a mouse-1 event and receive a
			;; movement event, that means the user wants
			;; to perform drag-selection, so cancel the
			;; button press and do the default mouse-1
			;; action.  For mouse-2, just highlight/
			;; unhighlight the button the mouse was
			;; initially on when we move over it.
			(save-excursion
			  (when face	; avoid changing around image
			    (overlay-put overlay 'face pressed-face)
			    (overlay-put overlay 'mouse-face pressed-face))
			  (unless (widget-apply button :mouse-down-action event)
			    (let ((track-mouse t))
			      (while (not (widget-button-release-event-p event))
              (setq event (read-event))

              ;; Sacha: Commented this section out so that my stylus
              ;; clicks don't get reported as mouse movement

				;; (when (and mouse-1 (mouse-movement-p event))
				;;   (push event unread-command-events)
				;;   (setq event oevent)
				;;   (throw 'button-press-cancelled t))
				(unless (or (integerp event)
					    (memq (car event) '(switch-frame select-window))
					    (eq (car event) 'scroll-bar-movement))
				  (setq pos (widget-event-point event))
				  (if (and pos
					   (eq (get-char-property pos 'button)
					       button))
				      (when face
					(overlay-put overlay 'face pressed-face)
					(overlay-put overlay 'mouse-face pressed-face))
				    (overlay-put overlay 'face face)
				    (overlay-put overlay 'mouse-face mouse-face))))))

			  ;; When mouse is released over the button, run
			  ;; its action function.
			  (when (and pos (eq (get-char-property pos 'button) button))
			    (goto-char pos)
			    (widget-apply-action button event)
			    (if widget-button-click-moves-point
				(setq newpoint (point)))))
		      (overlay-put overlay 'face face)
		      (overlay-put overlay 'mouse-face mouse-face))))

		(if newpoint (goto-char newpoint))
		;; This loses if the widget action switches windows. -- cyd
		;; (unless (pos-visible-in-window-p (widget-event-point event))
		;;   (mouse-set-point event)
		;;   (beginning-of-line)
		;;   (recenter))
		)
	      nil))
	  (let ((up t) command)
	    ;; Mouse click not on a widget button.  Find the global
	    ;; command to run, and check whether it is bound to an
	    ;; up event.
	    (if mouse-1
		(cond ((setq command	;down event
			     (lookup-key widget-global-map [down-mouse-1]))
		       (setq up nil))
		      ((setq command	;up event
			     (lookup-key widget-global-map [mouse-1]))))
	      (cond ((setq command	;down event
			   (lookup-key widget-global-map [down-mouse-2]))
		     (setq up nil))
		    ((setq command	;up event
			   (lookup-key widget-global-map [mouse-2])))))
	    (when up
	      ;; Don't execute up events twice.
	      (while (not (widget-button-release-event-p event))
		(setq event (read-event))))
	    (when command
	      (call-interactively command)))))
    (message "You clicked somewhere weird.")))

(use-package emms
  :config
  (progn
    (require 'emms-player-simple)
    (require 'emms-source-file)
    (require 'emms-source-playlist)
    (require 'emms-player-mplayer)
    (setq emms-player-list '(emms-player-mplayer))
    )
  :bind
  (("C-c e SPC" . emms-pause)
   ("C-c e e" . emms-pause)
   ("C-c e +" . emms-seek-forward)
   ("C-c e -" . emms-seek-backward)
   ("C-c e s" . emms-seek)
   ("C-c e [" . my/emms-player-mplayer-slow-down)
   ("C-c e ]" . my/emms-player-mplayer-speed-up)))

(defun my/split-sentence-and-capitalize ()
  (interactive)
  (delete-char 1)
  (insert ".")
  (capitalize-word 1))
(defun my/split-sentence-delete-word-and-capitalize ()
  (interactive)
  (delete-char 1)
  (insert ".")
  (kill-word 1)
  (capitalize-word 1))
(defun my/delete-word-and-capitalize ()
  (interactive)
  (skip-syntax-backward "w")
  (kill-word 1)
  (capitalize-word 1))

(defun my/emms-player-mplayer-set-speed (speed)
  "Depends on mplayer's -slave mode"
  (interactive "MSpeed: ")
  (process-send-string emms-player-simple-process-name
     (format "speed_set %s\n" speed)))

(defvar my/emms-player-mplayer-speed-increment 0.1)

(defun my/emms-player-mplayer-speed-up ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name
     (format "speed_incr %f\n" my/emms-player-mplayer-speed-increment)))
(defun my/emms-player-mplayer-slow-down ()
  "Depends on mplayer's -slave mode"
  (interactive)
  (process-send-string emms-player-simple-process-name
     (format "speed_incr %f\n" (- 0 my/emms-player-mplayer-speed-increment))))

(use-package rainbow-delimiters :disabled t)

(defvar my/org-quantified-categories
    '(("Business"
       ("Earn" . "Business - Earn")
       ("E1" . "Business - Earn - Consulting - E1")
       ("Connect" . "Business - Connect")
       ("Build" . "Business - Build"))
      ("Discretionary"
       ("Social" . "Discretionary - Social")
       ("Productive" . "Discretionary - Productive")
       ("Sewing" . "Discretionary - Productive - Sewing")
       ("Writing" . "Discretionary - Productive - Writing")
       ("Emacs" . "Discretionary - Productive - Emacs")
       ("Play" . "Discretionary - Play"))
      ("Personal" ;("Biking" . "Personal - Bike")
       ("Routines" . "Personal - Routines"))
      ("Sleep" nil)
      ("Unpaid work"
       ("Commuting" . "Unpaid work - Subway")
       ("Cook" . "Unpaid work - Cook")
       ("Tidy" . "Unpaid work - Tidy up")))
    "Categories for time summary.")

  (defun my/org-summarize-time-use (&optional start end)
    (require 'quantified)
    (interactive (list (org-read-date) (org-read-date)))
    (let ((time-summary (quantified-summarize-time start end))
          (categories my/org-quantified-categories)
          result)
      (setq result
            (mapconcat
             (lambda (a)
               (if (assoc (car a) time-summary)
                   (concat
                    (format "- %s: %.1f hours" (car a) (/ (cdr (assoc (car a) time-summary)) 3600.0))
                    (if (cdr a)
                        (let ((detail
                               (delq nil
                                     (mapcar (lambda (b)
                                               (if (assoc (cdr b) time-summary)
                                                   (format "%s: %.1f"
                                                           (car b)
                                                           (/ (cdr (assoc (cdr b) time-summary)) 3600.0))
                                                 nil))
                                             (cdr a)))))
                          (if detail
                              (concat " (" (mapconcat 'identity detail ", ") ")")
                            ""))
                      "")
                    (if (string-equal (car a) "Sleep")
                        (format " - average of %.1f hours per day" (/ (cdr (assoc (car a) time-summary)) 3600.0 7.0))
                      "")
                    "\n")))
       categories ""))
(if (called-interactively-p 'any)
    (insert result)
  result)))

(defun my/org-summarize-upcoming-week ()
  "Summarize upcoming tasks as a list."
  (interactive)
  (org-agenda nil "w")
  (let ((string (buffer-string))
        business relationships life)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward my/weekly-review-line-regexp nil t)
        (cond
         ((string= (match-string 1) "routines") nil) ; skip routine tasks
         ((string= (match-string 1) "business")
          (add-to-list 'business (concat "  - [ ] " (match-string 3))))
         ((string= (match-string 1) "people")
          (add-to-list 'relationships (concat "  - [ ] " (match-string 3))))
         (t (add-to-list 'life (concat "  - [ ] " (match-string 3)))))))
    (setq string
          (concat
      "*Plans for next week*\n"
      "- Business\n"
      (mapconcat 'identity business "\n")
      "\n- Relationships\n"
      (mapconcat 'identity relationships "\n")
      "\n- Life\n"
      (mapconcat 'identity life "\n")))
    (if (called-interactively-p)
        (kill-new string)
      string)))

(defun my/org-summarize-previous-week ()
  "Summarize previously-completed tasks as a list."
  (interactive)
  (save-window-excursion
    (org-agenda nil "w")
    (org-agenda-later -1)
    (org-agenda-log-mode 16)
    (let ((string (buffer-string))
          business relationships life)
      (with-temp-buffer
        (insert string)
      (goto-char (point-min))
      (while (re-search-forward my/weekly-review-line-regexp nil t)
        (cond
         ((string= (match-string 1) "routines") nil) ; skip routine tasks
         ((string= (match-string 1) "business")
          (add-to-list 'business (concat "  - " (match-string 2))))
         ((string= (match-string 1) "people")
          (add-to-list 'relationships (concat "  - " (match-string 2))))
         (t (add-to-list 'life (concat "  - " (match-string 2)))))))
    (setq string
          (concat
           "*Accomplished this week*\n\n"
           "- Business\n"
           (mapconcat 'identity business "\n")
           "\n- Relationships\n"
           (mapconcat 'identity relationships "\n")
           "\n- Life\n"
           (mapconcat 'identity life "\n")))
    (if (called-interactively-p)
        (kill-new string)
      string))))

(defun my/quantified-compare (start1 end1 start2 end2 &optional categories label1 label2)
  "Return a table comparing the times for START1 - END1 and START2 - END2."
  (let* ((start2 (org-read-date nil nil (or start2 "-sat")))
         (end2 (org-read-date nil nil (or end2 "+1")))
         (start1 (org-read-date nil nil (or start1 "-4sat")))
         (end1 (org-read-date nil nil (or end1 "-sat")))
         (time2 (quantified-summarize-time start2 end2))
         (time1 (quantified-summarize-time start1 end1))
         (label1 (or label1 "Period 1 %"))
         (label2 (or label2 "Period 2 %"))
         (total2 (* 0.01 (- (org-time-string-to-seconds end2) (org-time-string-to-seconds start2))))
         (total1 (* 0.01 (- (org-time-string-to-seconds end1) (org-time-string-to-seconds start1))))
         (keys (or categories (-union (mapcar 'car time1) (mapcar 'car time2)))))
    ;; Build a list comparing the two
    (append
    `(("Category" ,label1 ,label2 "Diff" "h/wk" "Diff h/wk"))
    (mapcar (lambda (key)
              (list
               key
               (format "%.1f" (/ (or (assoc-default key time1) 0) total1))
               (format "%.1f" (/ (or (assoc-default key time2) 0) total2))
               (format "%.1f" (- (/ (or (assoc-default key time2) 0) total2)
                                 (/ (or (assoc-default key time1) 0) total1)))
               (format "%.1f" (* (/ (or (assoc-default key time2) 0) total1) 1.68))
               (format "%.1f"
                       (* (- (/ (or (assoc-default key time2) 0) total2)
                             (/ (or (assoc-default key time1) 0) total1)) 1.68))
               )) keys))))

(defun my/animate-emacs-chat ()
  (interactive)
  (text-scale-set 6)
  (erase-buffer)
  (sit-for 3)
  (let ((list '("Emacs Chat: Sacha Chua"
                "interviewed by Bastien Guerry"
                ""
                "July 24, 2013"
                "sachachua.com/emacs-chat"))
        (approx-width 41)
        (approx-height 16)
        row)
    (setq row (/ (- approx-height (length list)) 2))
    (mapcar
     (lambda (x)
       (animate-string x
                       row
                       (/ (- approx-width (length x)) 2))
       (setq row (1+ row)))
     list)))

(defun my/org-get-list-categories ()
  "Return a list of (category indent matching-regexp sample).
List categories are items that don't contain links."
  (let ((list (org-list-struct)) last-category results)
    (save-excursion
      (mapc
       (lambda (x)
         (goto-char (car x))
         (let ((current-item
                (buffer-substring-no-properties
                 (+ (point)
                    (elt x 1)
                    (length (elt x 2)))
                 (line-end-position))))
           (if (string-match
                org-bracket-link-regexp
                (buffer-substring-no-properties
                 (point)
                 (line-end-position)))
               ;; Link - update the last category
               (when last-category
                 (if (< (elt x 1) (elt last-category 1))
                     (setq results
                           (cons (append last-category
                                         (list
                                          (match-string-no-properties
                                           3
                                           (buffer-substring-no-properties
                                            (point)
                                            (line-end-position)))))
                                 (cdr results))))
                 (setq last-category nil))
             ;; Category
             (setq results
                     (cons
                      (setq last-category
                            (list
                             current-item
                             (elt x 1)
                             (concat "^"
                                     (make-string (elt x 1) ?\ )
                                     (regexp-quote
                                      (concat (elt x 2)
                                              current-item))
                                     "$")))
                      results)))))
       list))
    results))

(defvar my/helm-org-list-candidates nil)
(defun my/helm-org-list-categories-init-candidates ()
  "Return a list of categories from this list in a form ready for Helm."
  (setq my/helm-org-list-candidates
        (mapcar (lambda (x)
                  (cons (if (elt x 3)
                            (format "%s - %s" (car x) (elt x 3))
                          (car x))
                        x))
                (my/org-get-list-categories))))

(defun my/org-move-current-item-to-category (category)
  (when category
    (let* ((beg (line-beginning-position))
           (end (line-end-position))
           (string (buffer-substring-no-properties beg end)))
      (save-excursion
        (when (re-search-backward (elt category 2) nil t)
          (delete-region beg (min (1+ end) (point-max)))
          (forward-line 1)
          (insert (make-string (+ 2 (elt category 1)) ?\ )
                  string "\n")))) t))

(defun my/org-guess-list-category (&optional categories)
  (interactive)
  (require 'cl-lib)
  (unless categories
    (setq categories
          (my/helm-org-list-categories-init-candidates)))
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (string (buffer-substring-no-properties beg end))
         (found
          (cl-member string
                     categories
                     :test
                     (lambda (string cat-entry)
                       (string-match (regexp-quote (downcase (car cat-entry)))
                                     string)))))
    (when (car found)
      (my/org-move-current-item-to-category
       (cdr (car found)))
      t)))

(setq my/helm-org-list-category-source
      (helm-build-sync-source
          "Non-link categories in the current list"
        :init 'my/helm-org-list-categories-init-candidates
        :candidates 'my/helm-org-list-candidates
        :action 'my/org-move-current-item-to-category
        :fuzzy-match t))

(defun my/org-guess-uncategorized ()
  (interactive)
  (my/helm-org-list-categories-init-candidates)
  (let (done)
    (while (not done)
      (save-excursion
        (unless (my/org-guess-list-category my/helm-org-list-candidates)
          (unless
              (helm :sources
                    '(my/helm-org-list-category-source
                      my/helm-org-list-category-create-source))
            (setq done t))))
      (unless done
        (setq done (not (looking-at "^[-+] \\[")))))))

(defun my/org-sort-list-by-regexp (regexp)
  (interactive "MRegexp: ")
  (save-restriction
    (narrow-to-region (point)
                      (save-excursion (org-end-of-item-list)))
    (org-sort-list
     nil ?f
     (lambda ()
       (let ((line (buffer-substring-no-properties
                    (point)
                    (save-excursion
                      (org-end-of-item)))))
         (if (string-match regexp line)
             (if (string-match org-bracket-link-regexp line)
                 (match-string 3 line)
               "ZZZ")
           "ZZZZZ")))
     'string<)))

(defun my/refile-sketches-to-questions ()
  (interactive)
(while (looking-at "^  \\+ \\[\\[.*?\\]\\[\\(.*?\\) -- \\(.*?\\)\\]\\]\n")
  (let ((link (match-string 0))
        (title (match-string 1)))
    (save-excursion
      (if (save-match-data (search-forward (concat "* " title) nil t))
          (progn (forward-line) (insert (match-string 0)) (replace-match ""))
        (forward-line 1))))))

(setq my/helm-org-list-category-create-source
      (helm-build-dummy-source
          "Create category"
        :action (helm-make-actions
                 "Create category"
                 (lambda (candidate)
                   (save-excursion
                     (let* ((beg (line-beginning-position))
                            (end (line-end-position))
                            (string (buffer-substring beg end)))
                       (delete-region beg (min (1+ end) (point-max)))
                       (org-beginning-of-item-list)
                       (insert "- " candidate "\n  " string "\n")))
                   (my/helm-org-list-categories-init-candidates)))))

(defun my/set-up-sketch-buffer ()
      "Populate a widget buffer with a few handy buttons."
      (with-current-buffer (get-buffer-create "*Done*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (widget-create 'push-button
                         :notify (lambda (&rest ignore)
                                   (my/org-clock-in-and-track-by-name "Draw"))
                         "Track: Draw")
          (widget-create 'push-button
                         :notify (lambda (&rest ignore)
                                   (my/org-clock-in-and-track-by-name "Draw journal entries"))
                         "Track: Journal")
          (widget-create 'push-button
                         :notify (lambda (&rest ignore)
                                   (my/open-image (my/prepare-index-card-template)))
                         "New")
          (widget-create 'push-button
                         :notify (lambda (&rest ignore)
                                   (my/open-image (my/prepare-index-card-template nil (org-read-date))))
                         "Date")
          (insert "\n")
          (widget-create 'push-button
                         :notify (lambda (&rest ignore)
                                   (shell-command "~/bin/add-output-png"))
                         "Add output.png")
          (widget-create 'push-button
                         :notify (lambda (&rest ignore)
                                   (my/rotate-screen 0)
                                   (kill-buffer)
                                   (my/rename-scanned-cards))
                         "Process")
          (widget-create 'push-button
                         :notify (lambda (&rest ignore)
                                   (my/rotate-screen 0)
                                   (delete-window)
                                   (my/rename-scanned-cards))
                         "Rename")
          (widget-create 'push-button
                         :notify (lambda (&rest ignore)
                                   (my/rotate-screen 0)
                                   (delete-window)
                                     (my/convert-and-upload-cards))
                         "Upload")
          (widget-create 'push-button
                         :notify (lambda (&rest ignore)
                                   (my/rotate-screen 0)
                                   (org-clock-out)
                                   (kill-buffer))
                         "Quit")
          (text-scale-set 10)
          (widget-setup)
          (widget-minor-mode)
          (pop-to-buffer (current-buffer))
          (goto-char (point-min))
          (current-buffer))))

    (defvar my/sketch-executable "C:/Program Files/Autodesk/SketchBook Pro 7/SketchBookPro.exe")
    (defvar my/index-card-template-file "c:/data/drawing-templates/custom/0 - index.tif")
    (when (eq system-type 'gnu/linux)
      (setq my/sketch-executable "krita"
            my/index-card-template-file "~/Dropbox/drawings/templates/0 - index.psd"))
    (defun my/prepare-index-cards (n)
      (interactive (list (or current-prefix-arg 5)))
      (let ((counter 1)
            (directory "~/Dropbox/Inbox")
            (template my/index-card-template-file)
            (date (org-read-date nil nil "."))
            temp-file)
        (quantified-track "Drawing")
        (dotimes (i 5) (my/open-image (my/prepare-index-card-template)))
        (my/rotate-screen 180)
        (my/set-up-sketch-buffer)))

    (defvar my/index-card-file-name nil "Most recent index card file name.")
    (defun my/rotate-screen (degrees)
      (cond
       ((eq system-type 'windows-nt)
        (shell-command (format "c:/sacha/Dropbox/bin/orient /rotate:%d" degrees)))
       ((eq system-type 'gnu/linux)
        (shell-command (format "~/bin/rotate-screen %s"
                               (cond
                                ((= degrees 0) "normal")
                                ((= degrees 180) "inverted")
                                ((= degrees 90) "left")
                                ((= degrees 270) "right")))))))

    (defvar my/sketch-directories
      '("~/Dropbox/Inbox"
        "~/Dropbox/Inbox/To blog"
        "~/Dropbox/sketches/private"
        "~/sketches"
        "~/photosync/photostream"))

(defun my/get-index-card-filenames-between-dates (start end)
  "Returns index card filenames between START and END."
  (my/get-index-card-filenames
   (lambda (filename)
     (and (string> (file-name-nondirectory filename) start)
          (string> end (file-name-nondirectory filename))))))
(defun my/get-index-card-filenames (base &optional as-regexp)
  "Check several directories for files matching BASE.
Return the matching filenames, if any.
If AS-REGEXP is non-nil, treat BASE as a regular expression.
If BASE is a function, use that to filter."
  (sort (-flatten
         (delq nil
               (mapcar
                (lambda (dir)
                  (and (file-directory-p dir)
                       (if (functionp base)
                           (-filter base (directory-files dir t ".*\\.\\(png\\|psd\\|tiff\\|jpg\\)?$"))
                         (directory-files
                          dir t
                          (concat
                           "\\("
                           (if as-regexp base (regexp-quote base))
                           "\\)"
                           ".*\\.\\(png\\|psd\\|tiff\\|jpg\\)?$"
                           )))))
                my/sketch-directories)))
        'string<))

(defun my/get-index-card-filename (base &optional as-regexp)
  "Check several directories for files matching BASE.
Return the first matching filename, if any.
If AS-REGEXP is non-nil, treat BASE as a regular expression."
  (car (my/get-index-card-filenames base as-regexp)))

(defun my/list-sketches (regexp &optional full-filename directories)
  "Return a list of sketch filenames matching REGEXP."
  (interactive "MFilter: \np")
  (let ((my/sketch-directories (or directories '("~/sketches"))))
    (funcall (if (called-interactively-p 'interactive)
                 (lambda (x) (insert (mapconcat (lambda (y) (concat "- " y)) x "\n"))) 'identity)
             (sort (-uniq
                    (mapcar (if full-filename 'identity
                              'file-name-nondirectory)
                            (my/get-index-card-filenames regexp t)))
                   'string>))))

(defun my/show-sketches-as-slideshow (list &optional shuffle)
  "Display a quick slideshow of sketches in LIST.
If LIST is a string, look up those sketch filenames in my Flickr copy."
  (interactive "MFilter: \nP")
  (apply 'call-process "feh" nil nil nil "-D" "1" "-F" (if shuffle "-z" """")
         (-filter (lambda (x) (string-match "photostream" x))
                  (if (stringp list)
                      (my/list-sketches list t)
                    list))))

    (defun my/prepare-index-card-template (&optional name date)
      "Create the image file for NAME. Return the new filename."
      (let* ((directory "~/Dropbox/Inbox")
             (template my/index-card-template-file)
             (date (or date (org-read-date nil nil ".")))
             (counter ?a))
        (while (my/get-index-card-filename (concat date (char-to-string counter)))
          (setq counter (1+ counter)))
        (setq name (expand-file-name
                    (concat date (char-to-string counter)
                            (if name
                                (concat " "
                                        (my/convert-sketch-title-to-filename (or name "")))
                              "")
                              "." (file-name-extension my/index-card-template-file))
                    directory))
        (unless (file-exists-p name) (copy-file template name))
        name))

    (defun my/prepare-index-card (&optional name date)
      "Prepare the index card for NAME.
    Rotate the screen and show a button to un-rotate the screen."
      (interactive (list (read-string "Name: ")
                         (if current-prefix-arg (org-read-date) (org-read-date nil nil "."))))
      (setq my/index-card-file-name (my/prepare-index-card-template name date))
      (save-window-excursion
        (my/rotate-screen 180)
        (shell-command
         (concat (shell-quote-argument my/sketch-executable)
                 " " (shell-quote-argument my/index-card-file-name) " &")))
      (my/set-up-sketch-buffer))

    (defun my/prepare-index-card-for-subtree ()
      "Create an index card template for the current subtree."
      (interactive)
      (let* ((heading (elt (org-heading-components) 4)))
        (unless (org-entry-get (point) "Effort") (org-set-property "Effort" "0:15"))
        (if (derived-mode-p 'org-agenda-mode) (org-agenda-clock-in) (org-clock-in))
        (my/org-quantified-track "Drawing")
        (if (org-at-heading-p) (forward-line 1))
        (my/prepare-index-card heading)))

    (defun my/helm-org-prepare-index-card-for-subtree (candidate)
      (let ((location (org-refile--get-location candidate my/helm-org-refile-locations)))
        (save-window-excursion
          (save-excursion
            (org-refile 4 nil location)
            (my/prepare-index-card-for-subtree)) t)))

    (defun my/open-image (filename)
      "Open image in Autodesk Sketchbook Pro."
      (call-process
       my/sketch-executable
       nil 0 nil filename))

    (defvar my/org-index-card-source nil)
    (defun my/org-prompt-index-cards ()
      "Display a buffer for easy selection of questions to work on."
      (interactive)
      (find-file "~/personal/questions.org")
      (let ((questions
             (cl-sort (org-map-entries 'org-heading-components "TODO=\"DRAW\"")
                      '< :key (lambda (x) (or (elt x 3) 100)))))
        (setq my/org-index-card-source (current-buffer))
        (my/rotate-screen 180)
        (my/set-up-sketch-buffer)
        (mapc (lambda (q)
                (widget-create 'push-button
                               :notify (lambda (widget &rest ignore)
                                         (my/open-image
                                          (my/prepare-index-card-template
                                           (widget-value widget)))
                                         (with-current-buffer my/org-index-card-source
                                           (save-excursion
                                             (goto-char (org-find-exact-headline-in-buffer (widget-value widget) my/org-index-card-source t))
                                             (org-set-property "Effort" "0:15")
                                             (org-clock-in)
                                             (org-todo "LINK")))
                                         (widget-delete widget))
                               (elt q 4))
                (insert "\n"))
              questions)
        (text-scale-set 5)
        (widget-setup)
        (widget-minor-mode)
        (goto-char (point-min))
        (scroll-bar-mode)
        (switch-to-buffer (current-buffer))))

    (defun my/prepare-index-card-for-journal ()
      "Create an index card for my process journal."
      (interactive)
      (quantified-track "Drawing")
      (my/prepare-index-card "Journal"))

    (add-to-list 'org-speed-commands-user '("d" call-interactively 'my/prepare-index-card-for-subtree))

(defun my/follow-up-on-sketch (filename)
    "Prompt for FILENAME to follow up on.
Create an index card with it as a layer, and add the ref to the filename."
    (interactive (list (helm-read-file-name "Image: " :initial-input "~/sketches/")))
    ;; Allow the specification of a short identifier
    (unless (file-exists-p filename)
      (setq filename (car (directory-files "~/sketches" t (concat "^" filename)))))
    (let ((async-shell-command-buffer 'new-buffer)
          (index-card (my/prepare-index-card-template
                       (format "-- index card ref %s"
                               (and (string-match "^[^ \\.]+" (file-name-nondirectory filename))
                                    (match-string 0 (file-name-nondirectory filename)))))))
      (shell-command (format "convert %s %s -colorspace cmyk %s"
                             (shell-quote-argument (expand-file-name my/index-card-template-file))
                             (shell-quote-argument (expand-file-name filename))
                             (shell-quote-argument (expand-file-name index-card))))
      (shell-command (format "%s %s &"
                             (shell-quote-argument my/sketch-executable)
                             (shell-quote-argument (expand-file-name index-card))))
      (my/rotate-screen 180)
      (my/set-up-sketch-buffer)))

(defun my/extract-categories (item)
  "Return a list of potential categories for ITEM.
For now, this looks for tags."
  )

(ert-deftest my/extract-categories ()
  (expect (equal (my/extract-categories "- [[http://example.com][This is a test #programming #geek]]")
                 '("programming" "geek"))))

(defun my/draw-journal-entry (date)
  "Creates a blank journal entry for DATE and brings up the log."
  (interactive (list (org-read-date)))
  ;; Open the Quantified Awesome time log for that date
  (let ((filename (my/get-journal-entry date))
        (day (format-time-string "%A" (org-time-string-to-time date))))
    (if filename
        (my/open-image filename)
      (browse-url (format "http://quantifiedawesome.com/records?start=%s&end=%s"
                          date
                          (format-time-string
                           "%Y-%m-%d"
                           (seconds-to-time
                            (+ (org-time-string-to-seconds date) 86400)))))
      (setq filename
            (my/prepare-index-card-template (concat day " #daily #journal") date))
      (my/open-image filename))))

(defun my/get-journal-entry (date)
  "Returns the filename for the journal sketch for DATE."
  (car
   (-filter (lambda (x) (not (string-match "weekly" x)))
            (my/get-index-card-filenames
             (format "%s.* .*#daily" date)
             t))))

(defun my/get-missing-journal-dates (start-date end-date)
  "Return a list of dates missing journal entries.
Range is specified by START-DATE (inclusive) and END-DATE (exclusive)."
  (let* ((current-day (org-time-string-to-absolute end-date))
         (start-day (org-time-string-to-absolute start-date))
         current-date
         current-date-string
         missing-list)
    (while (>= current-day start-day)
      (setq current-date (calendar-gregorian-from-absolute current-day))
      (setq current-date-string (format "%04d-%02d-%02d" (elt current-date 2) (elt current-date 0) (elt current-date 1)))
      (unless (my/get-journal-entry current-date-string)
        (add-to-list 'missing-list current-date-string))
      (setq current-day (1- current-day)))
    missing-list))

(defun my/show-missing-journal-entries (since)
  (interactive (list (if current-prefix-arg (org-read-date) (org-read-date nil nil "-7"))))
  (let ((missing-dates (my/get-missing-journal-dates since (org-read-date nil nil "."))))
    (with-current-buffer (my/set-up-sketch-buffer)
      (mapc
       (lambda (date)
         (widget-create 'push-button
                        :date date
                        :notify (lambda (widget &rest ignore)
                                  (my/draw-journal-entry (plist-get (cdr widget) :date)))
                        date))
       missing-dates)
      (widget-setup)
      (widget-minor-mode))))

(use-package s)
(defun my/process-tiff (files)
  "Convert, display, rename, and upload FILES."
  (interactive (list (dired-get-marked-files)))
  (unless (listp files) (setq files (list files)))
  (save-window-excursion
    (apply 'call-process "mogrify" nil nil nil (append (list "-format" "png" "-quality" "1") files))
    (delete-other-windows)
    (setq files
          (mapcar
           (lambda (filename)
             (find-file (setq filename (s-append ".png" (s-chop-suffix ".tif" filename))))
             (let ((new-name
                    (read-string "New name: "
                                 (concat
                                  (if (string-match "/\\(\\([0-9]+-[0-9]+-[0-9]+\\)\\( ?.*\\)?\\)\\.png" filename)
                                      (match-string 1 filename)
                                    filename)
                                  " "))))
               (rename-file filename (concat new-name ".png"))
               (setq filename (expand-file-name (concat new-name ".png") (file-name-directory filename)))))
           files)))
  (find-file "~/Dropbox/Public/sharing/index.org")
  (goto-char (point-min))
  (when (re-search-forward (regexp-quote "#+ORGLST: sketchinbox"))
    (forward-line 1)
    (org-end-of-item-list)
    (apply 'call-process "up" nil t nil files)))

(defun my/convert-index-card-tiffs-to-pngs ()
  (interactive)
  (let ((pattern "^\\(IMG\\|[0-9]+-[0-9]+-[0-9]+\\).*.\\(tif\\|psd\\)$"))
    (when (directory-files "~/Dropbox/Inbox" t pattern)
      ;; Convert the TIFFs first
      (apply 'call-process "mogrify" nil nil nil "-flatten" "-format" "png" "-quality" "1"
             (directory-files "~/Dropbox/Inbox" t pattern))
      (mapc (lambda (x)
              (rename-file x "~/Dropbox/Inbox/backup" t))
            (directory-files "~/Dropbox/Inbox" t pattern)))))

(defun my/convert-and-upload-cards ()
  "Trust in existing filenames, upload without modification."
  (interactive)
  (my/convert-index-card-tiffs-to-pngs)
  (my/upload-scanned-cards))

(defun my/rename-scanned-card (filename)
  (find-file filename)
  (delete-other-windows)
  (let ((base (file-name-sans-extension filename))
        notes)
    (when (string-match "/IMG.*\\|\\(\\([0-9]+-[0-9]+-[0-9]+\\)\\( ?.*\\)?\\)" base)
      (let ((kill-buffer-query-functions nil)
            old-name
            (new-name (read-string "New name: "
                                   (if (match-string 1 base)
                                       (concat (match-string 1 base))
                                     ""))))
        (while (and (string-match "^[0-9]+-[0-9]+-[0-9]+[a-z]" new-name)
                    (setq old-name (my/get-index-card-filename (match-string 0 new-name)))
                    (and old-name
                         (not (string= old-name filename))
                         (not (string= (file-name-nondirectory old-name)
                                       (concat (s-trim new-name) "." (file-name-extension filename))))))
          (setq new-name
                (read-string (format "Already exists (%s) - new name: " old-name)
                             new-name)))
        (when (string-match new-name "^\\(.*?\\) *| *\\(.*\\)")
          (with-current-buffer (find-file "~/Dropbox/orgzly/Inbox.org")
            (goto-char (point-max))
            (insert "\n* " (match-string 1 new-name) "\n" (match-string 2 new-name))
            (save-buffer))
          (setq new-name (match-string 1 new-name)))
        (when (> (length new-name) 0)
          (revert-buffer t t)
          (rename-file filename (concat (s-trim new-name) "." (file-name-extension filename)) t)
          (kill-buffer))))))

(defun my/rename-scanned-cards ()
  "Display and rename the scanned or saved files."
  (interactive)
  (my/convert-index-card-tiffs-to-pngs)
  (mapc 'my/rename-scanned-card
        (directory-files "~/Dropbox/Inbox" t "^\\(IMG\\|[0-9]+-[0-9]+-[0-9]+\\).*.\\(png\\|jpg\\)"))
   (my/upload-scanned-cards))

(defun my/clean-index-card-directory ()
  "Remove files marked for deletion and move private files."
  (shell-command "mv ~/Dropbox/Inbox/*delete* ~/Dropbox/Inbox/backup")
  (shell-command "mv ~/Dropbox/Inbox/*private* ~/Dropbox/sketches/private"))

(defun my/upload-scanned-cards ()
  (interactive)
  (my/clean-index-card-directory)
  (with-current-buffer (get-buffer-create "*Files to be uploaded*")
    (erase-buffer)
    (insert (mapconcat 'identity (directory-files "~/Dropbox/Inbox" nil "^[0-9]+-[0-9]+-[0-9]+[^ ]? .*.\\(png\\|jpg\\)") "\n"))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (delete-other-windows))
  (when (yes-or-no-p "Upload files?")
    (cond
     ((eq system-type 'windows-nt)
      (apply 'call-process "up" nil nil nil (directory-files "~/Dropbox/Inbox" t "^[0-9]+-[0-9]+-[0-9]+[^ ]? .*.\\(png\\|jpg\\)")))
     ((eq system-type 'gnu/linux)
      (with-temp-buffer
       (cd "~/code/node")
       (apply 'call-process "node" nil nil nil (expand-file-name "flickr-upload.js")
              (directory-files "~/Dropbox/Inbox" t "^[0-9]+-[0-9]+-[0-9]+[^ ]? .*.\\(png\\|jpg\\)")))
      )))
  (shell-command "~/bin/copy-sketches")
  ;;(when (re-search-forward (regexp-quote "#+ORGLST: sketchinbox"))
  ;;  (forward-line 1)
  ;;  (org-end-of-item-list)
  ;;  (save-excursion (yank) (insert "\n"))
  ;;  (my/org-guess-uncategorized))
 )

(use-package image+
 :load-path "~/elisp/Emacs-imagex"
 :commands (imagex-global-sticky-mode imagex-auto-adjust-mode)
 :init (progn (imagex-global-sticky-mode) (imagex-auto-adjust-mode)))

(defun my/prepare-sketchnote-file ()
  (interactive)
  (let* ((base-name (org-entry-get-with-inheritance  "BASENAME"))
         (filename (expand-file-name (concat base-name ".tif") "~/dropbox/inbox/")))
    (unless base-name (error "Missing basename property"))
    (if (file-exists-p filename)
        (error "File already exists")
        (copy-file "g:/drawing-templates/custom/0 - base.tif" filename))
      (shell-command (concat (shell-quote-argument my/sketch-executable)
                             (shell-quote-argument filename) " &"))))

(defun my/org-stage-image-files-in-subtree ()
  "Move corresponding linked images to staging directory."
  (interactive)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-regexp nil t)
        (let ((filename (file-name-nondirectory (or (match-string 3) (match-string 1)))))
          (when (and (string-match "\\.png$" filename)
                     (file-exists-p (expand-file-name filename "~/Dropbox/Inbox/To blog")))
            (rename-file
             (expand-file-name filename "~/Dropbox/Inbox/To blog")
             "~/Dropbox/Inbox/Selection")))))))

(defun my/get-tile-dimensions (num-items orig-width orig-height target-aspect-ratio)
  (let ((rows 1) (cols 1)
        (current-aspect (/ orig-width (float orig-height)))
        add-col-aspect
        add-row-aspect)
    (while (< (* rows cols) num-items)
      (setq add-col-aspect (/ (* (1+ cols) (float orig-width))
                              (* rows orig-height))
            add-row-aspect (/ (* cols (float orig-width))
                              (* (1+ rows) orig-height)))
      (if (<  (abs (- add-col-aspect target-aspect-ratio))
              (abs (- add-row-aspect target-aspect-ratio)))
          (setq cols (1+ cols))
        (setq rows (1+ rows))))
    (cons cols rows)))
(ert-deftest my/get-tile-dimensions ()
  (should (equal (my/get-tile-dimensions 2 2 1 1) (cons 1 2)))
  (should (equal (my/get-tile-dimensions 4 2 1 0.5) (cons 1 4)))
  (should (equal (my/get-tile-dimensions 12 1 1 (/ 4.0 3.0)) (cons 4 3)))
  (should (equal (my/get-tile-dimensions 11 1 1 (/ 4.0 3.0)) (cons 4 3)))
  (should (equal (my/get-tile-dimensions 13 1 1 (/ 4.0 3.0)) (cons 4 4))))

(defun my/extract-image-filenames (beg end)
  "Return the filenames from the links in this region."
  (let (files)
    (save-excursion
      (goto-char (min beg end))
      (while (re-search-forward "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][a-z]" (max beg end) t)
        (add-to-list 'files (match-string-no-properties 0) t)))
    files))
(ert-deftest my/extract-image-filenames ()
  "Check if it extracts the filenames properly."
  (with-temp-buffer
    (insert "- [[https://www.flickr.com/photos/somephotoid][2016-01-01a test]]\n")
    (insert "- [[https://www.flickr.com/photos/somephotoid2][2016-02-01a again]]\n")
    (should (equal (my/extract-image-filenames (point-min) (point-max))
                   '("2016-01-01a" "2016-02-01a")))))

(defun my/create-sketch-montage (files &optional tiles)
  "Combine the sketches in the region."
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (dired-get-marked-files)
      (mapcar 'my/get-index-card-filename
              (my/extract-image-filenames (min (point) (mark)) (max (point) (mark)))))
    (if current-prefix-arg (read-string "Tiling: "))))
  ;; Extract the links
  (let ((output-file "~/Dropbox/Inbox/output.png"))
    (unless tiles
      (setq tiles
            (format "%dx"
              (car (my/get-tile-dimensions (length files) 1500 900 (/ 4.0 3))))))
    (with-temp-buffer
      (cd "~/Dropbox/Inbox/To blog")
      (apply 'call-process
             "montage" nil nil nil
             (append
              files
              (list
               "-geometry" "1500x900>+0+0"
               "-tile" tiles
               (expand-file-name output-file)))))
    (if (called-interactively-p 'any) (find-file output-file))))

(defun my/create-week-montage (beg end)
  (interactive "r")
  (let* ((date (org-read-date nil nil (unless current-prefix-arg "-fri")))
         (filename (format "Week ending %s #journal #weekly" date))
         (full-filename (my/get-index-card-filename filename)))
    (if full-filename
        (my/open-image full-filename)
      (my/create-index-card-montage
       (mapcar 'my/get-index-card-filename
               (my/extract-image-filenames (min (point) (mark)) (max (point) (mark))))
       "2x"
       (my/prepare-index-card-template filename)))))

(defun my/create-index-card-montage (files &optional tiling filename)
  "Prepare an index card with a montage of the selected sketches as a layer."
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (dired-get-marked-files)
      (mapcar 'my/get-index-card-filename
              (my/extract-image-filenames (min (point) (mark)) (max (point) (mark)))))))
  (let ((async-shell-command-buffer 'new-buffer)
        (index-card (or filename (my/prepare-index-card-template))))
    (my/create-sketch-montage files tiling)
    (shell-command
     (format "convert %s \\( %s -resize 1500x900 \\) -colorspace cmyk %s"
             (shell-quote-argument (expand-file-name my/index-card-template-file))
             (shell-quote-argument (expand-file-name "~/Dropbox/Inbox/output.png"))
             (shell-quote-argument (expand-file-name index-card))))
    (shell-command (format "%s %s &"
                           (shell-quote-argument my/sketch-executable)
                           (shell-quote-argument (expand-file-name index-card))))
    (my/rotate-screen 180)
    (my/set-up-sketch-buffer)))

(setq yas-indent-line 'fixed)
  (defun my/convert-sketch-title-to-filename (text)
    (setq text (replace-regexp-in-string "[?!]$" "" text))
    (setq text (replace-regexp-in-string "[?!:] " " - " text)))
  (ert-deftest my/convert-sketch-title-to-filename ()
    (should (string= (my/convert-sketch-title-to-filename "Test") "Test"))
    (should (string= (my/convert-sketch-title-to-filename "Another Test!") "Another Test"))
    (should (string= (my/convert-sketch-title-to-filename "Does this work? Yes") "Does this work - Yes"))
    (should (string= (my/convert-sketch-title-to-filename "Title: Subtitle") "Title - Subtitle"))
    )

(defun my/convert-sketched-book-to-png ()
  "Convert TIFF to PNG."
  (interactive)
  (let ((basename (org-entry-get-with-inheritance "BASENAME")))
    (shell-command (format "convert \"c:/sacha/dropbox/inbox/%s.tif\" \"c:/sacha/dropbox/inbox/%s.png\""
                           basename
                           basename))))

(defun my/index-sketched-book ()
  "Add entries to sketched books index."
  (interactive)
  (let* ((title (org-entry-get-with-inheritance "SHORT_TITLE"))
        (author (org-entry-get-with-inheritance "AUTHOR"))
        (basename (org-entry-get-with-inheritance "BASENAME"))
        (base-file (format "~/Dropbox/inbox/%s.png" basename)))
    (when (file-exists-p base-file)
      (copy-file base-file
                 (format "~/Dropbox/packaging/sketched-books/%s.png" basename) t t)
      (copy-file base-file
                 (format "g:/documents/photosync/Visual Book Reviews/%s.png" basename) t t))
    (find-file "~/Dropbox/packaging/sketched-books/index.org")
    (vc-git-register (list (format "%s.png" basename)))
    (goto-char (point-min))
    (re-search-forward "<<insert-point>>")
    (insert (format "\n- [[file:%s.png][%s - %s (sketched %s)]]\n  [[file:%s.png]]\n\n"
                    basename
                    title
                    author
                    (substring basename 0 10)
                    basename))
    (find-file "~/Dropbox/packaging/sketched-books/ebook.org")
    (goto-char (point-min))
    (re-search-forward "<<insert-point>>")
    (insert (format "\n* %s - %s (sketched %s)\n\n[[file:%s.png]]\n\n"
                    title
                    author
                    (substring basename 0 10)
                    basename))))

(defun my/package-sketched-book ()
  "Add the latest sketch and package the collection."
  (interactive)
  (shell-command
   (format "plink -A vagrant@127.0.0.1 -P 2222 \"cd ~/dropbox/Packaging/sketched-books; git add '%s.png'; git commit -m 'Added %s - %s' -a; git push; make all\" &"
           (org-entry-get-with-inheritance "BASENAME")
           (org-entry-get-with-inheritance "SHORT_TITLE")
           (org-entry-get-with-inheritance "AUTHOR"))))

(defvar my/espeak-command "c:/program files (x86)/espeak/command_line/espeak.exe")
(defun my/say (string &optional speed)
  (interactive "MString: ")
  (setq speed (or speed 175))
  (call-process my/espeak-command nil nil nil string "-s" speed))

(when (eq system-type 'windows-nt)
(setenv "PATH" (concat "\"c:/program files/postgresql/9.3/bin;\"" (getenv "PATH"))))
