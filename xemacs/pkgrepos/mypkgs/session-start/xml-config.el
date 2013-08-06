;; $Id: dot_emacs,v 1.2 2003/07/18 23:28:28 tonygraham Exp $



(deh-section "xml"
  ;; from: http://www.emacswiki.org/emacs/XmlParserExamples

  ;; You can also walk the tree converting nodes to sexp’s. The
  ;; following function transforms an xml document into a tree-widget
  ;; using tree-widget.el. This could form the basis of a really cool
  ;; xml editing interface for emacs.

  (defun xml->tree-widget (root)
    (cond ((null root) nil)
          ((listp root) (let ((elem (xml-node-name root))
                              (children (remove-if (function stringp) (xml-node-children root))))
                          `(tree-widget :node (push-button
                                               :tag ,(format "%s" elem)
                                               :format "%[%t%]\n"
                                               :xml-node ,root
                                               :notify ,(lambda (widget &rest rest)
                                                                (message (format "%s" (widget-get widget :xml-node)))))
                                        ,@(mapcar (lambda (x) (xml->tree-widget x)) children))))))

  (when nil
    (let* ((xml "<post time=\"20050716234509\" id=\"010101\">
               <login>Test</login>
               <msg>Here is the message</msg>
               <info>My UA</info>
             </post>")
           (root (with-temp-buffer
                   (insert xml)
                   (xml-parse-region (point-min) (point-max))))
           (post (car root))
           (attrs (xml-node-attributes post))
           (time (cdr (assq 'time attrs)))
           (msg (car (xml-get-children post 'msg)))
           (text (car (xml-node-children msg))))
      (message "time: %s, message '%s'" time text))
    (let* ((xml "<post time=\"20050716234509\" id=\"010101\">
               <login>Test</login>
               <msg>Here is the message</msg>
               <info>My UA</info>
             </post>")
           (root (with-temp-buffer
                   (insert xml)
                   (xml-parse-region (point-min) (point-max))))
           (post (car root))
           (attrs (xml-node-attributes post))
           (time (cdr (assq 'time attrs)))
           (msg (car (xml-get-children post 'msg)))
           (text (car (xml-node-children msg))))
      (tree-mode-insert (xml->tree-widget root) (get-buffer-create "*xmltree*")))))


(deh-section "imenu"
  (defun xml-imenu-get-tree ()
  "Produce the index for Imenu."
  (mapc (lambda (x) (move-marker x nil)) org-imenu-markers)
  (setq org-imenu-markers nil)
  (let* ((n org-imenu-depth)
	 (re (concat "^" outline-regexp))
	 (subs (make-vector (1+ n) nil))
	 (last-level 0)
	 m level head)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-max))
	(while (re-search-backward re nil t)
	  (setq level (org-reduced-level (funcall outline-level)))
	  (when (<= level n)
	    (looking-at org-complex-heading-regexp)
	    (setq head (org-link-display-format
			(org-match-string-no-properties 4))
		  m (org-imenu-new-marker))
	    (org-add-props head nil 'org-imenu-marker m 'org-imenu t)
	    (if (>= level last-level)
		(push (cons head m) (aref subs level))
	      (push (cons head (aref subs (1+ level))) (aref subs level))
	      (loop for i from (1+ level) to n do (aset subs i nil)))
	    (setq last-level level)))))
    (aref subs 1)))

  ;; (set (make-local-variable imenu-create-index-function) xml-imenu-get-tree)
  ;; [[file:/usr/share/emacs/23.4/lisp/imenu.el.gz::imenu%20el%20framework%20for%20mode%20specific%20buffer%20indexes][imenu.el]]
  ;; [[file:~/.setup-trunk/xemacs/pkgrepos/world/misc/pde/imenu-tree.el::A%20list%20to%20search%20icon%20for%20the%20button%20in%20the%20tree][imenu-tree.el]]
  )



(deh-section "xstylesheet"

  ;; XSL mode
  (autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)

  ;; Uncomment if you want to use `xsl-grep' outside of XSL files.
  (autoload 'xsl-grep "xslide" "Grep for PATTERN in files matching FILESPEC." t)

  ;; Uncomment if you want to use `xslide-process' in `xml-mode'.
  (autoload 'xsl-process "xslide-process" "Process an XSL stylesheet." t)
  (add-hook 'xml-mode-hook
            (lambda ()
              (if (and
                   (boundp 'xml-mode-map)
                   xml-mode-map)
                  (define-key xml-mode-map [(control c) (meta control p)]
                    'xsl-process))))

  ;; Turn on font lock when in XSL mode
  (add-hook 'xsl-mode-hook
            'turn-on-font-lock)


  ;; mode is not that much good.
  ;; (setq auto-mode-alist
  ;;       (append
  ;;        (list
  ;; 	'("\\.fo" . xsl-mode)
  ;; 	'("\\.xsl" . xsl-mode))
  ;;        auto-mode-alist))

  ;; Uncomment if using abbreviations
  (abbrev-mode t)
  )

(deh-section "xquery-config"

  (deh-require-maybe xquery-mode))



(deh-section "xslt-config"

  (when (xrequire 'string)
    (deh-require-maybe xslt-process
      (autoload 'xslt-process-mode "xslt-process" "Emacs XSLT processing" t)
      (autoload 'xslt-process-install-docbook "xslt-process"
        "Register the DocBook package with XSLT-process" t)
      (add-hook 'sgml-mode-hook 'xslt-process-mode)
      (add-hook 'xml-mode-hook 'xslt-process-mode)
      (add-hook 'xsl-mode-hook 'xslt-process-mode)

      (defadvice xml-mode (after run-xml-mode-hooks act)
        "Invoke `xml-mode-hook' hooks in the XML mode."
        (run-hooks 'xml-mode-hook)))))

(deh-require-maybe nxml-mode
  (setq nxml-child-indent 2))


(provide 'xml-config)
