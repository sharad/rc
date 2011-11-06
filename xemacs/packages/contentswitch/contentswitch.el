<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: contentswitch.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=contentswitch.el" /><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: contentswitch.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=contentswitch.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for contentswitch.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=contentswitch.el" /><meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/></head><body class="http://www.emacswiki.org/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/emacs/SiteMap"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&amp;q=%22contentswitch.el%22">contentswitch.el</a></h1></div><div class="wrapper"><div class="content browse"><p class="download"><a href="download/contentswitch.el">Download</a></p><pre class="code"><span class="linecomment">;;; contentswitch.el --- switch to buffer/file by content</span>

<span class="linecomment">;; Copyright (C) 2008  Tamas Patrovics</span>

<span class="linecomment">;; $LastChangedDate: 2008-08-22 19:58:23 +0200 (P, 22 aug. 2008) $</span>

<span class="linecomment">;; Contributors:</span>
<span class="linecomment">;;     Ted Zlatanov</span>

<span class="linecomment">;; This file is free software; you can redistribute it and/or modify</span>
<span class="linecomment">;; it under the terms of the GNU General Public License as published by</span>
<span class="linecomment">;; the Free Software Foundation; either version 2, or (at your option)</span>
<span class="linecomment">;; any later version.</span>

<span class="linecomment">;; This file is distributed in the hope that it will be useful,</span>
<span class="linecomment">;; but WITHOUT ANY WARRANTY; without even the implied warranty of</span>
<span class="linecomment">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>
<span class="linecomment">;; GNU General Public License for more details.</span>

<span class="linecomment">;; You should have received a copy of the GNU General Public License</span>
<span class="linecomment">;; along with GNU Emacs; see the file COPYING.  If not, write to</span>
<span class="linecomment">;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,</span>
<span class="linecomment">;; Boston, MA 02110-1301, USA.</span>

<span class="linecomment">;;; Commentary:</span>

<span class="linecomment">;; Since the advent of timid.el, anything.el and similar tools one can</span>
<span class="linecomment">;; quickly open a recent file by typing a part of its name without</span>
<span class="linecomment">;; knowing in which directory the file is located. It's very</span>
<span class="linecomment">;; convenient, but what if one doesn't remember the name either, only</span>
<span class="linecomment">;; some of the file contents?</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This package provides a command `contentswitch' which allows the</span>
<span class="linecomment">;; user to switch to a buffer or an unopened file by typing a part of</span>
<span class="linecomment">;; its content, instead of its name, and selecting a file from the</span>
<span class="linecomment">;; result list with the cursor keys and ENTER. (By default the</span>
<span class="linecomment">;; substring is also tested on the file/buffer name, but the purists</span>
<span class="linecomment">;; can disable it with an option.) Note that the search works for</span>
<span class="linecomment">;; files only if you (as all other sane people) use recentf, savehist</span>
<span class="linecomment">;; or a similar package.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Matching on the file content can be useful if one can come up with</span>
<span class="linecomment">;; a fairly unique string from the file, but what if the string is not</span>
<span class="linecomment">;; unique enough? Usually, the first thing popping into one's mind is</span>
<span class="linecomment">;; the direct context of what he was working on. E.g. I began to</span>
<span class="linecomment">;; implement that for loop for counting things. "for" is not a really</span>
<span class="linecomment">;; unique word, so typing it probably results in lots of unwanted</span>
<span class="linecomment">;; matches. To avoid this the package can list those matches first</span>
<span class="linecomment">;; which have the given string within the context of the current</span>
<span class="linecomment">;; position of point in the file. See option</span>
<span class="linecomment">;; `contentswitch-context-bias' about tuning this behavior. The</span>
<span class="linecomment">;; context search requires saveplace.el, otherwise point location</span>
<span class="linecomment">;; cannot be determined for unopened files.</span>
<span class="linecomment">;; </span>
<span class="linecomment">;; See additional configuration options below.</span>
<span class="linecomment">;; </span>

<span class="linecomment">;; Tested on Emacs 22</span>

<span class="linecomment">;;; Code:</span>

(defgroup contentswitch nil
  "<span class="quote">The contentswitch package.</span>"
  :version "<span class="quote">22.1</span>")

(defcustom contentswitch-context-bias (if (featurep 'saveplace) 100)
  "<span class="quote">If nil then matching files are simply listed in LRU order.

If t then the closer the match to the current position of point in
the buffer is, the higher it is listed in the results.

If it is a number then it is the same as the previous option with
the difference that those files which have a match within the
specified distance (in characters) from the current position of
point are given the same priority and listed in LRU
order. Probably this is what most people want, since the user
tends to remember what he did last time in a file, but didn't
know which word was the closest to point, so the most recent file
with a match within the given context is very likely the one he
is after.

If you use a non-nil value for this variable then you should also
use saveplace.el, because saveplace information is used to
restore point location for unopened files. Without that
information this feature useless.</span>"
  :group 'contentswitch
  :type '(radio (const :format "<span class="quote">LRU order </span>" nil)
		(const :format "<span class="quote">Distance from point</span>" t)
		(integer :format "<span class="quote">Match within distance: %v</span>")))

(defcustom contentswitch-enable-name-matches t
  "<span class="quote">Match the query string against the buffer and file name as well.</span>"
  :group 'contentswitch
  :type 'boolean)

(defcustom contentswitch-jump-to-match-location nil
  "<span class="quote">After switching, put point at match.</span>"
  :group 'contentswitch
  :type 'boolean)

(defcustom contentswitch-file-history-variable
  (if (featurep 'recentf)
      'recentf-list
    'file-name-history)
  "<span class="quote">The file names are taken from this variable.

If you use recentf then make sure it is loaded before this
package, so the default can be initialized properly.

If you don't use recentf and fall back to the standard variable
`file-name-history' then it is recommended to use savehist.el or
a similar package, so that the list of recently opened files is
restored when Emacs is restarted.</span>"
  :group 'contentswitch
  :type  '(choice :tag "<span class="quote">File history</span>"
		  (const :tag "<span class="quote">recentf</span>" recentf-list)
		  (const :tag "<span class="quote">file name history</span>" file-name-history)
		  (const :tag "<span class="quote">Empty</span>"  nil)))

(defcustom contentswitch-ignore '("<span class="quote">^*</span>")
  "<span class="quote">List of regexps for excluding buffer and files from the results.</span>"
  :group 'contentswitch
  :type '(repeat (regexp :tag "<span class="quote">Regular expression</span>")))

(defcustom contentswitch-ignore-remote-files t
  "<span class="quote">Do not search remote unopened files for matches.</span>"
  :group 'contentswitch
  :type 'boolean)

(defcustom contentswitch-ignore-encrypted-files t
  "<span class="quote">Do not search encrypted unopened files for matches.</span>"
  :group 'contentswitch
  :type 'boolean)

(defcustom contentswitch-max-files-from-history 30
  "<span class="quote">Do not show more matches from file history than this limit.</span>"
  :group 'contentswitch
  :type 'integer)

(defcustom contentswitch-file-completion-delay 0.3
  "<span class="quote">Delay before showing completions from file history.</span>"
  :group 'contentswitch
  :type 'float)

(defcustom contentswitch-max-name-length 25
  "<span class="quote">Width of the name column in the result list.</span>"
  :group 'contentswitch
  :type 'integer)

(defcustom contentswitch-before-context-length 10
  "<span class="quote">Number of characters from context shown before the actual match.</span>"
  :group 'contentswitch
  :type 'integer)

(defcustom contentswitch-selection-face 'highlight
  "<span class="quote">Face for selection.</span>"
  :group 'contentswitch
  :type 'face)

(defcustom contentswitch-context-face 'header-line
  "<span class="quote">Face for match context.</span>"
  :group 'contentswitch
  :type 'face)

(defcustom contentswitch-match-face 'lazy-highlight
  "<span class="quote">Face for match.</span>"
  :group 'contentswitch
  :type 'face)


(defvar contentswitch-map 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<span class="quote">&lt;down&gt;</span>") 'contentswitch-next-line)
    (define-key map (kbd "<span class="quote">&lt;up&gt;</span>") 'contentswitch-previous-line)
    (define-key map (kbd "<span class="quote">&lt;prior&gt;</span>") 'contentswitch-previous-page)
    (define-key map (kbd "<span class="quote">&lt;next&gt;</span>") 'contentswitch-next-page)
    (define-key map (kbd "<span class="quote">&lt;RET&gt;</span>") 'exit-minibuffer)
    map)
  "<span class="quote">Keymap.</span>")

<span class="linecomment">;;; end of user configuration</span>

(require 'cl)


(defconst contentswitch-buffer "<span class="quote">*contentswitch*</span>"
  "<span class="quote">Buffer used for finding files.</span>")
 
(defvar contentswitch-overlay nil
  "<span class="quote">Overlay used to highlight the current selection.</span>")

(defvar contentswitch-current-input "<span class="quote"></span>"
  "<span class="quote">The previous input substring used for searching.</span>")
 
(defvar contentswitch-idle-timer nil
  "<span class="quote">Idle timer for file matches.</span>")
 


(defun contentswitch-mark-current-line ()
  "<span class="quote">Mark current line with a distinctive color.</span>"
  (move-overlay contentswitch-overlay (point-at-bol) (1+ (point-at-eol))))
 
 
(defun contentswitch-previous-line ()
  "<span class="quote">Move selection to the previous line.</span>"
  (interactive)
  (contentswitch-move-selection 'next-line -1))
 
 
(defun contentswitch-next-line ()
  "<span class="quote">Move selection to the next line.</span>"
  (interactive)
  (contentswitch-move-selection 'next-line 1))
 
 
(defun contentswitch-previous-page ()
  "<span class="quote">Move selection back with a pageful.</span>"
  (interactive)
  (contentswitch-move-selection 'scroll-down nil))
 
 
(defun contentswitch-next-page ()
  "<span class="quote">Move selection forward with a pageful.</span>"
  (interactive)
  (contentswitch-move-selection 'scroll-up nil))
 
 
(defun contentswitch-move-selection (movefunc movearg)
  "<span class="quote">Move the selection marker to a new position determined by
MOVEFUNC and MOVEARG.</span>"
  (unless (= (buffer-size (get-buffer contentswitch-buffer)) 0)
    (save-selected-window
      (select-window (get-buffer-window contentswitch-buffer))
      (condition-case nil
          (funcall movefunc movearg)
        (beginning-of-buffer (goto-char (point-min)))
        (end-of-buffer (goto-char (point-max))))

      <span class="linecomment">;; if line end is point-max then it's either an incomplete line or</span>
      <span class="linecomment">;; the end of the output, so move up a line</span>
      (if (= (point-at-eol) (point-max))
          (next-line -1))

      (contentswitch-mark-current-line)

      (let* ((object (plist-get (contentswitch-get-jump-info) 'object))
             (file (if (bufferp object)
                       (if (buffer-file-name object)
                           (expand-file-name (buffer-file-name object)))
                     object)))
        (when file
          (message "<span class="quote">File path: %s</span>" file))))))
      

(defun contentswitch-get-buffer-info (buffer)
  "<span class="quote">Return match information in BUFFER or nil if there is no match.</span>"
  (append (with-current-buffer buffer
            (if (if contentswitch-context-bias
                    (let* ((point (point))
                           (backward (save-excursion
                                       (if (search-backward 
                                            contentswitch-current-input nil t)
                                           (cons (abs (- point (point))) 
                                                 (match-data)))))
                           (forward (save-excursion
                                      (if (search-forward 
                                           contentswitch-current-input nil t)
                                          (cons (abs (- point (point))) 
                                                (match-data))))))
                      (if backward
                          (if forward
                              (progn
                                (set-match-data 
                                 (if (&lt; (car forward) (car backward))
                                     (cdr forward)
                                   (cdr backward)))
                                t)

                            (set-match-data  (cdr backward))
                            t)

                        (when forward
                          (set-match-data  (cdr forward))
                          t)))
                  
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward contentswitch-current-input nil t)))

                (save-excursion
                  (goto-char (match-beginning 0))
                  (list
                   'line (buffer-substring-no-properties
                          (point-at-bol)
                          (point-at-eol))
                   'line-start (point-at-bol)
                   'start (match-beginning 0)
                   'end (match-end 0)))))

          (if contentswitch-enable-name-matches
              (let ((start (string-match 
                            (regexp-quote contentswitch-current-input)
                            (buffer-name buffer))))
                (if start                    
                    (list 'name-start start
                          'name-end (match-end 0)))))))


(defun contentswitch-check-input ()
  "<span class="quote">Check input string and update the list of matching files.</span>"
  (unless (equal (minibuffer-contents) contentswitch-current-input)
    (setq contentswitch-current-input (minibuffer-contents))

    (when contentswitch-idle-timer
      (cancel-timer contentswitch-idle-timer)
      (setq contentswitch-idle-timer nil))

    (with-current-buffer contentswitch-buffer
      (erase-buffer))

    (contentswitch-display-matches
     (delete-if-not
      (lambda (info)
        info)

      (mapcar (lambda (buffer)
                (let ((info (unless (equal contentswitch-current-input "<span class="quote"></span>")
                              (contentswitch-get-buffer-info buffer))))
                  (if (or info
                          (equal contentswitch-current-input "<span class="quote"></span>"))
                      (plist-put (plist-put info 'object buffer)
                                 'point (with-current-buffer buffer
                                          (point))))))

              (delete-if (lambda (buffer)
                           (or (eq (aref (buffer-name buffer) 0) ?\ )
                               (some (lambda (regex)
                                       (string-match
                                        regex (buffer-name buffer)))
                                     contentswitch-ignore)
                               (eq buffer (get-buffer contentswitch-buffer))))
                           
                         (buffer-list)))))

      
    (unless (equal contentswitch-current-input "<span class="quote"></span>")
      (setq contentswitch-idle-timer
            (run-with-idle-timer 
             contentswitch-file-completion-delay nil 
             (lambda ()
               (setq contentswitch-idle-timer nil)
               (contentswitch-display-matches
                (let (infos files)
                  (dolist (file (symbol-value contentswitch-file-history-variable))
                    (unless (or (get-file-buffer file) <span class="linecomment">; opened files</span>
                                                       <span class="linecomment">; are dealt</span>
                                                       <span class="linecomment">; with above</span>
                                (member file files)
                                (file-directory-p file)
                                (and contentswitch-ignore-remote-files
                                     (file-remote-p file))
                                (and contentswitch-ignore-encrypted-files
                                     (featurep 'epa-file)
                                     (string-match epa-file-name-regexp file))
                                (not (file-readable-p file))
                                (some (lambda (regex)
                                        (string-match regex file))
                                     contentswitch-ignore))
                      (let* ((buffer (find-file-noselect file t))
                             (point (with-current-buffer buffer
                                      (if (and contentswitch-context-bias
                                               (featurep 'saveplace))
                                          <span class="linecomment">;; restore saved point position</span>
                                          (save-place-find-file-hook))
                                      (point)))
                             (info (contentswitch-get-buffer-info buffer)))

                        <span class="linecomment">;; since we suppressed the opening ceremony</span>
                        <span class="linecomment">;; above there no need to performing the</span>
                        <span class="linecomment">;; closing one</span>
                        (let ((kill-buffer-hook nil))
                          (kill-buffer buffer))

                        (when info
                          (push file files)
                          (push (plist-put (plist-put info 'object file) 
                                           'point point)
                                infos)
                          (if (&gt; (length files)
                                 contentswitch-max-files-from-history)
                              (return))))))

                  <span class="linecomment">;; because of push</span>
                  (nreverse infos)))))))))
         

(defun contentswitch-display-matches (infos)
  "<span class="quote">Display INFOS in the result buffer.</span>"
  (unless (equal contentswitch-current-input "<span class="quote"></span>")
    (if contentswitch-context-bias
        (setq infos 
              (sort infos 
                    (lambda (info1 info2)
                      (if (plist-get info1 'start)
                          (if (plist-get info2 'start)
                              (let ((diff1 (abs (- (plist-get info1 'point)
                                                   (plist-get info1 'start))))
                                    (diff2 (abs (- (plist-get info2 'point)
                                                   (plist-get info2 'start)))))
                                (if (numberp contentswitch-context-bias)
                                    (if (&lt;= diff1 contentswitch-context-bias)
                                        (&gt; diff2 contentswitch-context-bias)

                                      (unless (&lt;= diff2 contentswitch-context-bias)
                                        (&lt; diff1 diff2)))

                                  (&lt; diff1 diff2)))

                            <span class="linecomment">;; info2 has no content match, so info1</span>
                            <span class="linecomment">;; is ranked higher</span>
                            t)

                        <span class="linecomment">;; if info2 has content match then</span>
                        <span class="linecomment">;; it is ranked higher</span>
                        (plist-get info2 'start)))))))

  (let ((window-width (window-width (selected-window))))
    (with-current-buffer contentswitch-buffer
      (save-excursion
        (goto-char (point-max))

        (dolist (info infos)
          (let* ((name (if (bufferp (plist-get info 'object))
                           (buffer-name (plist-get info 'object))
                         (file-name-nondirectory (plist-get info 'object))))
                 padded-name match)

            (if (not (plist-get info 'line))
                (insert name)
                     
              (setq padded-name (concat 
                                 name
                                 (make-string
                                  (+ (max 0 (- contentswitch-max-name-length
                                               (length name)))
                                     4)
                                  ? )))

              (let ((start-in-line (- (plist-get info 'start)
                                      (plist-get info 'line-start))))
                (setq match (concat 
                             padded-name
                             (substring
                              (plist-get info 'line)
                              (max 0 (- start-in-line
                                        contentswitch-before-context-length))
                              start-in-line)))

                (insert match)
                (insert (substring (plist-get info 'line)
                                   start-in-line
                                   (+ start-in-line
                                      (min (- (length (plist-get info 'line))
                                              start-in-line)
                                           (- window-width 1 (length match))))))))

            (put-text-property (point-at-bol) (point-at-eol) 
                               'contentswitch-jump-info info)

            (insert "<span class="quote">\n</span>")

            (save-excursion
              (forward-line -1)

              (when match
                (let ((overlay (make-overlay (+ (point-at-bol) (length padded-name))
                                             (1+ (point-at-eol)))))
                  (overlay-put overlay 'face contentswitch-context-face))

                (let ((overlay (make-overlay (+ (point-at-bol)
                                                (length match))
                                             (+ (point-at-bol)
                                                (length match)
                                                (- (plist-get info 'end)
                                                   (plist-get info 'start))))))
                  (overlay-put overlay 'face contentswitch-match-face)))

              (if (plist-get info 'name-start)                    
                  (let ((overlay (make-overlay 
                                  (+ (point-at-bol)
                                     (plist-get info 'name-start))
                                  (+ (point-at-bol)
                                     (plist-get info 'name-end)))))
                    (overlay-put overlay 'face contentswitch-match-face)))))))))


  (if (and (&gt; (with-current-buffer contentswitch-buffer (buffer-size)) 0)
           (= (overlay-start contentswitch-overlay) <span class="linecomment">; no selection yet</span>
              (overlay-end contentswitch-overlay)))
      (save-selected-window
        (select-window (get-buffer-window contentswitch-buffer))
        (goto-char (point-min))
        (contentswitch-mark-current-line))))


(defun contentswitch-get-jump-info ()
  "<span class="quote">Get the jump information for the current line.</span>"
  (with-current-buffer  contentswitch-buffer
    (get-text-property (overlay-start contentswitch-overlay)
                       'contentswitch-jump-info)))


(defun contentswitch-do ()
  (erase-buffer)
  (setq mode-name "<span class="quote">Switch by Content</span>")

  (if contentswitch-overlay
      <span class="linecomment">;; make sure the overlay belongs to the contentswitch buffer if</span>
      <span class="linecomment">;; it's newly created</span>
      (move-overlay contentswitch-overlay (point-min) (point-min)
                    (get-buffer contentswitch-buffer))

    (setq contentswitch-overlay (make-overlay (point-min) (point-min)))
    (overlay-put contentswitch-overlay 'face contentswitch-selection-face))

  (setq contentswitch-current-input nil)
  (add-hook 'post-command-hook 'contentswitch-check-input)

  (setq contentswitch-idle-timer nil)
 
  (with-current-buffer contentswitch-buffer
    (setq cursor-type nil))
 
  (unwind-protect
      (let ((minibuffer-local-map contentswitch-map))
        (read-string "<span class="quote">string: </span>"))
 
    (remove-hook 'post-command-hook 'contentswitch-check-input)
 
    (with-current-buffer contentswitch-buffer
      (setq cursor-type t))))


(defun contentswitch ()
  (interactive)
  (let ((winconfig (current-window-configuration))
        <span class="linecomment">;; we need coding system and format conversion for</span>
        <span class="linecomment">;; `find-file-noselect', but don't need any of the</span>
        <span class="linecomment">;; `after-find-file' nonsense, so the definition of</span>
        <span class="linecomment">;; `after-find-file' is suppressed temporarily</span>
        (orig-fun (symbol-function 'after-find-file)))

    (fset 'after-find-file (lambda (&optional error warn noauto
                                              after-find-file-from-revert-buffer
                                              nomodes)))
    (pop-to-buffer contentswitch-buffer)

    (unwind-protect
        (contentswitch-do) 

      (fset 'after-find-file orig-fun)
      (set-window-configuration winconfig)))
 
  (unless (= (buffer-size (get-buffer contentswitch-buffer)) 0)
    (let* ((info (contentswitch-get-jump-info))
           (object (plist-get info 'object)))
      (if (bufferp object)
          (switch-to-buffer object)
        (find-file object))
      (if (and contentswitch-jump-to-match-location
               (plist-get info 'start))
          (goto-char (plist-get info 'start))))))
        


(put 'contentswitch 'timid-completion 'disabled)

(provide 'contentswitch)
<span class="linecomment">;;; contentswitch.el ends here</span></span></pre></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=contentswitch.el;missing=de_en_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=contentswitch.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=contentswitch.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=contentswitch.el">Administration</a></span><span class="time"><br /> Last edited 2008-09-05 05:54 UTC by 146.124.141.59 <a class="diff" rel="nofollow" href="http://www.emacswiki.org/emacs?action=browse;diff=2;id=contentswitch.el">(diff)</a></span><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>
