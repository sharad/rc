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


(provide 'xml-config)
