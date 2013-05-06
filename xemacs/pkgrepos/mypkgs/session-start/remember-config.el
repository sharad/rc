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
(require 'remember-experimental)
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
                        remember-experimental
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

  (defmacro cdr-assoc-cdr-assoc (key1 key2 alist)
    `(cdr (assoc ,key2 (cdr (assoc ,key1 ,alist)))))

  (defun getelements (alist &rest keys)
    (reduce (lambda (alist k)
              (cdr (assoc k alist)))
            keys
            :initial-value alist))

  ;; (reduce #'list '(1 2 3 4)
  ;;         :initial-value 'foo)

  ;; (defun run-list-until-success (flist)
  ;;   (some 'funcall flist))


  (defun sharad/remember-fun-set-orgnizer (fun)
    (eval
     `(defadvice ,fun (around Ad-organizer activate)
        (let ((remember-annotation-functions
               (cdr-assoc-cdr-assoc remember-organizer 'annotation sharad/remember-functions-alist))
              (remember-handler-functions
               (cdr-assoc-cdr-assoc remember-organizer 'handler sharad/remember-functions-alist))
              (remember-mode-hook
               (cdr-assoc-cdr-assoc remember-organizer 'hook sharad/remember-functions-alist)))
          ad-do-it))))

  (defun sharad/remember-set-orgnizer ()
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
      (sharad/remember-fun-set-orgnizer fun)))

  (sharad/remember-set-orgnizer)

  ;; (unless (ad-find-advice 'ccm-first-start 'before 'reset-ccm-vpos)
  ;;   (defadvice ccm-first-start (before reset-ccm-vpos (animate) activate)
  ;;     (setq ccm-vpos nil) t))



  (defun dontforgetme (&optional arg)
    (interactive "P")
    (let ((old-remember-organizer remember-organizer)
          (organizer
           (intern
            (ido-completing-read "Organizer: "
                                 (mapcar (lambda (e)
                                           (symbol-name (car e)))
                                         sharad/remember-functions-alist)
                                 nil t))))
      (setq remember-organizer organizer)
      (remember arg)
      ;; will not work, think more or live with it.
      ;; (setq remember-organizer old-remember-organizer)
      ))

  (defun sharad/remember-org (&optional initial)
    (interactive)
    (let ((organizer 'org))
      (setq remember-organizer organizer)
      (remember initial)))

  (defun sharad/remember-planner (&optional initial)
    (interactive)
    (let ((organizer 'planner))
      (setq remember-organizer organizer)
      (remember initial))))


(provide 'remember-config)

