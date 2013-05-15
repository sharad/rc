;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www.emacswiki.org/emacs/RememberMode#toc7
;; start

;; Setting up for OrgMode
;;Basically you should follow the instructions in the org-mode info
;;file. However this currently forgets that you also need something like
;;this in your .emacs:

(require 'cl)

(require 'remember)
(require 'org)
(require 'planner)
(require 'remember-planner)
(require 'read-file-name)
;; (require 'remember-blosxom)
;; (require 'remember-experimental) ;; will start mail at daemon startup time.
(require 'remember-autoloads)
(require 'remember-diary)
(require 'remember-planner)
(require 'remember-bbdb)
(require 'remember)
;; (require 'remember-bibl) ; - check it
;; (require 'macs-wiki-journal)


(deh-require-maybe (and remember
                        org
                        planner
                        remember-planner
                        read-file-name
                        ;; remember-blosxom
                        ;; remember-experimental ;; will start mail at daemon startup time.
                        remember-autoloads
                        remember-diary
                        remember-planner
                        remember-bbdb
                        remember
                        loadhist
                        ;; remember-bibl
                        ;; macs-wiki-journal
                        )


  (defvar sharad/remember-functions-alist nil "")

  (defvar remember-organizer 'planner "")

  (setq sharad/remember-functions-alist
        `((planner .
                   ((annotation . ,planner-annotation-functions)
                    (handler    . (remember-planner-append))
                    (hook)))
          (org .
               ((annotation . (org-remember-annotation))
                (handler    . (org-remember-handler))
                (hook       . (org-remember-apply-template))))))



  ;; (set-tree-node sharad/remember-functions-alist '(org-remember-annotation) 'org 'annotation)
  ;; (set-tree-node sharad/remember-functions-alist 'planner-annotation-functions 'planner 'annotation)
  ;; (set-tree-node sharad/remember-functions-alist '(org-remember-handler) 'org 'handler)
  ;; (set-tree-node sharad/remember-functions-alist '(remember-planner-append) 'planner 'handler)
  ;; (set-tree-node sharad/remember-functions-alist '(org-remember-apply-template) 'org 'hook)
  ;; (set-tree-node sharad/remember-functions-alist nil 'planner 'hook)

  (defun get-tree-node (tree &rest keys)
    (reduce (lambda (xtree k)
              ;; (message "tree %s k %s ret (cdr (assoc k xtree)) %s" xtree k (cdr (assoc k xtree)))
              (cdr (assoc k xtree)))
            keys
            :initial-value tree))

  (when nil

    (get-tree-node sharad/remember-functions-alist 'org)

    (defun set-tree-node (tree e &rest keys)
      (setcdr
       (reduce (lambda (xtree k)
                 (message "tree %s k %s" xtree k)
                 (unless (assoc k xtree)
                   (pushnew (list k) xtree))
                 (assoc k xtree))
               keys
               :initial-value tree)
       e))

    (defun set-tree-node (tree e &rest keys)
      (setcdr
       (reduce (lambda (xtree k)
                 (message "tree %s k %s" xtree k)
                 (assoc k (pushnew (list k) (cdr xtree) :key 'car)))
               keys :initial-value (cons nil tree))
       e))


    (defmacro set-tree-node (tree e &rest keys)
      (if keys
          `(assoc ,(car keys)
                  (pushnew
                   (list ,(car keys))
                   (cdr
                    (set-tree-node `(cdr ,tree) e ,@(cdr keys)))
                   :key 'car))
          tree)
      e)

    (defmacro set-tree-node (tree &rest keys)
      (if keys
          `(assoc ,(car keys)
                  (pushnew
                   (list ,(car keys))
                   (cdr
                    (set-tree-node (cdr ,tree) e ,@(cdr keys)))
                   :key 'car))
          tree))

    (macroexpand-all '(set-tree-node jt o 'n 'b 'c))



    (setq jt nil)

    (set-cdr (set-tree-node jt 'o n b c) 'o)

    (defmacro remacro (&rest keys)
      (if keys
          `(abc ,(car keys) (list ,@(cdr keys)))
          t))



    (defmacro remacro (keys)
      (if keys
          `(abc ,(car keys)
                (remacro ,(cdr keys)))
          ))


    (defmacro remacro (&rest keys)
      (if keys
          `(abc ,(car keys)
                (remacro ,@(cdr keys)))
          ))



    (remacro 'a 'b 'c)

    (macroexpand-all '(remacro a b c))

    (abc a (abc b (abc c nil)))

    (remacro '(a b c))

    (macroexpand-all '(remacro (a b c)))
    (abc a (abc b (abc c nil)))





  (defun set-tree-node (tree e &rest keys)
    `(progn
       ,@(maplist
          (lambda (k)
            `(pushnew (list ,(car k) (cdr tree) :key 'car))
            keys))))

  )

  ;; (defun depth (tree)
  ;;   ;; http://www.lispforum.com/viewtopic.php?p=5372&sid=e117daaa584b63c64864135d178ea654#p5372
  ;;   (if (atom tree)
  ;;       0
  ;;       (if (atom (cdr tree))
  ;;           (1+ (depth (or (cdr tree)
  ;;                          (list (car tree)))))
  ;;           (1+ (apply 'max (mapcar #'depth tree))))))


  (defun depth (tree)
    ;; http://www.lispforum.com/viewtopic.php?p=5372&sid=e117daaa584b63c64864135d178ea654#p5372
    (if (atom tree)
        0
        (if (cdr tree)
            (if (atom (cdr tree))
                1
                (1+ (apply 'max (mapcar #'depth tree))))
            (if (atom (car tree))
                1
                (1+ (apply 'max (mapcar #'depth tree)))))))


  (depth '(nil .  (a (b  ( c . d) ))))

  (depth '( (b) a ))

  (depth '())
  (depth nil)
  (depth 'nil)
  (depth '(nil))

  (defun* get-tree-leaves (tree &optional (depth 0))
    (let ((tdepth (depth tree)))
      (if (>= depth tdepth)
          (if (= depth tdepth)
              (list tree))
          (let (ret)
            (if (and (cdr tree)
                     (atom (cdr tree)))
                (and (cdr tree)
                     (list (cdr tree)))
                (dolist (tr (or (cdr tree)
                                (list (car tree))) ret)
                  ;; (dolist (tr tree ret)
                  (setq
                   ret
                   (append ret
                           (get-tree-leaves tr depth)))))))))


  (testing


  (get-tree-leaves '(a
                     (z)
                     (b (c . d) (e . d) i)
                     (o (n (p . q) (w . x)))) 1)

  (get-tree-leaves '((a
                      (z)
                      (b (c . d) (e . d) i)
                      (o (n (p . q) (w . x))))) 1)


  ;; (((a (z) (b (c . d) (e . d) i) (o (n (p . q) (w . x))))))


  (car tree)

  (depth '((a (b (c . d) (e . d))
            (o (n (p . q) (w . x))))))
  )

  (when nil


    (progn
      (get-tree-node '((a .((b ((c . d)))))) 'a 'b 'c)
      (get-tree-node '((a (b (c . d)))) 'a 'b 'c)
      (get-tree-node '((a)) 'a 'b 'c)
      (cdr (assoc 'a '((a ((b ((c . d)))))))))

    (progn
      (setq jt '((a (b (c . d)))))
      ;; (setq jt '((a . b)))
      (set-tree-node jt 'o 'n 'b 'c)
      jt)


    (progn
      (setq ol '((k p) (a b)))
      (assoc 'k (pushnew '(f . c) ol :key 'car)))



    )



  (when nil ;;get interactive


    (nth 4 (indirect-function 'find-file))

    (nth 2 (aref  (indirect-function 'remember) 5))


    (nth 3  (indirect-function 'remember))



    (defun help-function-interactive (def)
      ;; Handle symbols aliased to other symbols.
      (if (and (symbolp def) (fboundp def)) (setq def (indirect-function def)))
      ;; If definition is a macro, find the function inside it.
      (if (eq (car-safe def) 'macro) (setq def (cdr def)))
      (cond
        ((byte-code-function-p def) (append (nth 2 (aref def 5)) nil))
        ((eq (car-safe def) 'lambda) (nth 3 def))
        ((and (eq (car-safe def) 'autoload) (not (eq (nth 4 def) 'keymap)))
         "[Arg list not available until function definition is loaded.]")
        (t t)))

    (append  [ 1 2 ] ())

    (help-function-interactive 'find-file)
    (help-function-interactive 'remember)


    )



  ;; (reduce #'list '(1 2 3 4)
  ;;         :initial-value 'foo)

  ;; (defun run-list-until-success (flist)
  ;;   (some 'funcall flist))



  (defun remember-fun-set-orgnizer-advice (fun adname)
    (unless (ad-find-advice fun 'around adname)
      (eval
       `(defadvice ,fun (around ,adname ,(help-function-arglist fun) activate)
          ;; ,(help-function-interactive 'fun)
          (let ((remember-annotation-functions
                 (get-tree-node  sharad/remember-functions-alist remember-organizer 'annotation))
                (remember-handler-functions
                 (get-tree-node sharad/remember-functions-alist remember-organizer 'handler))
                (remember-mode-hook
                 (get-tree-node sharad/remember-functions-alist remember-organizer 'hook)))
            ad-do-it))))
    (ad-enable-advice fun 'around adname)
    (ad-activate fun)
    (ad-update fun))

  (defun remember-fun-unset-orgnizer-advice (fun adname)
    (when (ad-find-advice fun 'around adname)
      (ad-remove-advice fun 'around adname))
    (ad-activate fun)
    (ad-update fun))

  (defun remember-fun-disable-orgnizer-advice (fun adname)
    (when (ad-find-advice fun 'around adname)
      (ad-disable-advice fun 'around adname))
    (ad-activate fun)
    (ad-update fun))

  (defun remember-fun-enable-orgnizer-advice (fun adname)
    (when (ad-find-advice fun 'around adname)
      (ad-enable-advice fun 'around adname))
    (ad-activate fun)
    (ad-update fun))

  (defun remember-set-orgnizer-advice ()
    (interactive)
    (setq remember-annotation-functions nil
          remember-handler-functions nil
          remember-mode-hook nil)
    (dolist (fun (mapcar 'cdr
                         (remove-if-not
                          '(lambda (e)
                            (and (consp e)
                             (eq 'defun (car e))))
                          (feature-symbols 'remember))))
      (remember-fun-set-orgnizer-advice fun 'Ad-organizer)))

    (defun remember-manage-orgnizer-advice (mgrfn)
      (interactive
       (let*
           ((fnnames '("sharad/remember-fun-set-orgnizer"
                       "sharad/remember-fun-unset-orgnizer"
                       "sharad/remember-fun-enable-orgnizer"
                       "sharad/remember-fun-disable-orgnizer"))
            (fn (ido-completing-read "manager: " fnnames nil t)))
         (list (intern fn))))
      (setq remember-annotation-functions nil
            remember-handler-functions nil
            remember-mode-hook nil)
      (dolist (fun (mapcar 'cdr
                           (remove-if-not
                            '(lambda (e)
                              (and (consp e)
                               (eq 'defun (car e))))
                            (feature-symbols 'remember))))
        (funcall mgrfn fun 'Ad-organizer)))

  (remember-set-orgnizer-advice)
  ;; (sharad/remember-unset-orgnizer)

  ;; (unless (ad-find-advice 'ccm-first-start 'before 'reset-ccm-vpos)
  ;;   (defadvice ccm-first-start (before reset-ccm-vpos (animate) activate)
  ;;     (setq ccm-vpos nil) t))


  (defun remember-change-orgnizer (&optional orgnizer)
    (interactive
     (list (when current-prefix-arg
             (buffer-substring (point) (mark)))))
    (let ((old-remember-organizer remember-organizer)
          (organizer
           (or
            orgnizer
            (intern
             (ido-completing-read "Organizer: "
                                  (mapcar (lambda (e)
                                            (symbol-name (car e)))
                                          sharad/remember-functions-alist)
                                  nil t)))))
      (setq remember-organizer organizer)))


  (defun dontforgetme (&optional initial)
    (interactive
     (list (when current-prefix-arg
             (buffer-substring (point) (mark)))))
    (let ((old-remember-organizer remember-organizer)
          (organizer (remember-change-orgnizer)))
      (setq remember-organizer organizer)
      (remember initial)
      ;; will not work, think more or live with it.
      ;; (setq remember-organizer old-remember-organizer)
      ))

  (defun sharad/remember-org (&optional initial)
    (interactive
     (list (when current-prefix-arg
             (buffer-substring (point) (mark)))))
    (let ((organizer 'org))
      (setq remember-organizer organizer)
      (remember initial)))

  (defun sharad/remember-planner (&optional initial)
    (interactive
     (list (when current-prefix-arg
             (buffer-substring (point) (mark)))))
    (let ((organizer 'planner))
      (setq remember-organizer organizer)
      (remember initial)))




  )

(progn

  (defvar ad-remember-mode-after-hook nil "")

  (defadvice remember-buffer (after remember-mode-after-hook activate)
    (run-hooks  'ad-remember-mode-after-hook))



 )



(deh-section "Idle reminder"

  (defvar idle-reminder-register nil "Idle reminder register")
  (defvar idle-reminder-buffer nil "buffer")
  (defvar idle-reminder-interval (* 7 60) "Idle reminder register")
  (defvar idle-reminder-timer nil "buffer")


  (defvar idle-reminder-mode-map (make-sparse-keymap)
    "Keymap for org-remember-mode, a minor mode.
Use this map to set additional keybindings for when Org-mode is used
for a Remember buffer.")

  (defvar idle-reminder-mode-hook nil
    "Hook for the minor `idle-reminder-mode'.")

  (define-minor-mode idle-reminder-mode
      "Minor mode for special key bindings in a remember buffer."
    nil " Rem" idle-reminder-mode-map
    (run-hooks 'org-remember-mode-hook))


  (defun leave-show-reminder ()
    (interactive)
    (when (equal idle-reminder-buffer
                 (current-buffer))
      (with-current-buffer idle-reminder-buffer
        (reader-mode nil)
        (bury-buffer)))
    (if idle-reminder-register
        (jump-to-register idle-reminder-register)))

  (define-key idle-reminder-mode-map "q" 'leave-show-reminder)
  ;; (define-key idle-reminder-mode-map "\C-c\C-k" 'org-remember-kill)

  (defun show-reminder (fn &optional time-to-show)
    (window-configuration-to-register idle-reminder-register)
    (setq idle-reminder-buffer (funcall fn))
    ;; (view-mode 1)
    (when idle-reminder-buffer
      (with-current-buffer idle-reminder-buffer
        (reader-mode 1)
        (idle-reminder-mode 1))
      (switch-to-buffer idle-reminder-buffer)))


  (defun show-some-orgfile ()
    (let* ((file "~/.Organize/emacs/org/myself/emacs.org")
           (buf (or (find-buffer-visiting file)
                    (find-file-noselect file))))
      (switch-to-buffer buf)
      buf))

  (defun idle-reminder-start ()
    (interactive)
    (setq idle-reminder-timer
          (run-with-idle-timer idle-reminder-interval t 'show-reminder 'show-some-orgfile)))


  (defun idle-reminder-cancel ()
    (interactive)
    (when idle-reminder-timer
      (cancel-timer idle-reminder-timer)
      (when idle-reminder-buffer
        (with-current-buffer idle-reminder-buffer
          (reader-mode nil)
          (bury-buffer)))))


  ;; (show-reminder 'show-some-orgfile)

  )






(provide 'remember-config)

