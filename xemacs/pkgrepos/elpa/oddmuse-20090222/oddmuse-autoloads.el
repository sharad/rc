;;; oddmuse-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (oddmuse-kill-url oddmuse-browse-this-page oddmuse-browse-page
;;;;;;  emacswiki-post oddmuse-delete oddmuse-redirect oddmuse-insert-pagename
;;;;;;  oddmuse-revert oddmuse-post oddmuse-follow oddmuse-edit oddmuse-toggle-minor)
;;;;;;  "oddmuse" "oddmuse.el" (20362 28458))
;;; Generated autoloads from oddmuse.el

(autoload 'oddmuse-toggle-minor "oddmuse" "\
Toggle minor mode state.

\(fn &optional ARG)" t nil)

(autoload 'oddmuse-edit "oddmuse" "\
Edit a page on a wiki.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to edit.
Use a prefix argument to force a reload of the page.

\(fn WIKI PAGENAME)" t nil)

(autoload 'oddmuse-follow "oddmuse" "\
Figure out what page we need to visit
and call `oddmuse-edit' on it.

\(fn ARG)" t nil)

(autoload 'oddmuse-post "oddmuse" "\
Post the current buffer to the current wiki.
The current wiki is taken from `oddmuse-wiki'.

\(fn SUMMARY)" t nil)

(autoload 'oddmuse-revert "oddmuse" "\
Revert this oddmuse page.

\(fn)" t nil)

(autoload 'oddmuse-insert-pagename "oddmuse" "\
Insert a PAGENAME of current wiki with completion.

\(fn PAGENAME)" t nil)

(autoload 'oddmuse-redirect "oddmuse" "\
Insert a #REDIRECT directive to `pagename' for current `oddmuse-page-name'.

\(fn PAGENAME)" t nil)

(autoload 'oddmuse-delete "oddmuse" "\
Add a page name to the DeletedPage stack.
User will be asked to confirm it is correct.

\(fn SUMMARY)" t nil)

(autoload 'emacswiki-post "oddmuse" "\
Post the current buffer to the EmacsWiki.
If this command is invoked interactively: with prefix argument, prompts pagename,
otherwise set pagename as basename of `buffer-file-name'.

This command is intended to post current EmacsLisp program easily.

\(fn &optional PAGENAME SUMMARY)" t nil)

(autoload 'oddmuse-browse-page "oddmuse" "\
Ask a WWW browser to load an oddmuse page.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to browse.

\(fn WIKI PAGENAME)" t nil)

(autoload 'oddmuse-browse-this-page "oddmuse" "\
Ask a WWW browser to load current oddmuse page.

\(fn)" t nil)

(autoload 'oddmuse-kill-url "oddmuse" "\
Make the URL of current oddmuse page the latest kill in the kill ring.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("oddmuse-pkg.el") (20362 28458 974568))

;;;***

(provide 'oddmuse-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; oddmuse-autoloads.el ends here
