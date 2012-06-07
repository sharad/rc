
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
      (run-hooks 'xml-mode-hook))))

(provide 'xslt-config)
