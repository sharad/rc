;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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


;; $Id: dot_emacs,v 1.2 2003/07/18 23:28:28 tonygraham Exp $



(progn ;; "xml"
  ;; http://stackoverflow.com/a/11409099
  (setq rng-nxml-auto-validate-flag nil)

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


(progn ;; "imenu"
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
  ;; [[file:~/.setup/xemacs/pkgrepos/world/misc/pde/imenu-tree.el::A%20list%20to%20search%20icon%20for%20the%20button%20in%20the%20tree][imenu-tree.el]]
  )



(progn ;; "xstylesheet"

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

;; (provide 'config)
;;; config.el ends here
