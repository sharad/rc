

;; C-\ 						     toggle-input-method
;;                  from multilingual mule

;; see http://www.emacswiki.org/emacs/TreeMode#toc3
;; C-c v                                              imenu-tree
;;                                                    tags-tree
;;                                                    glade-mode.el

;; glasses-mode Minor mode for making identifiers likeThis readable.

;; TTT RRR YYY
;; scroll-all-mode see http://osdir.com/ml/help-gnu-emacs-gnu/2009-04/msg00660.html

;; M-.                                                 find-tag
;; M-*                                                 pop-tag-mark

;; C-x C-+, C-x C--, C-x C-0                          text-scale-adjust

;; C-+                                                toggle-hiding

;; C-x C-o                                            delete-blank-lines

;; C-x C-r                                            find-alternative-file-with-sudo
;; C-x 4 c                                            clone-indirect-buffer-other-window

;; <any control/meta/... key> F1
;; F1 F1
;; C-c <f1>
;; C-x RET <f1>
;; C-x RET <f1>
;; M-Space                                              (just-one-space &optional N)
;; C-x l                                                 count-lines-page
;; C-h l, <f1> l, <help> l                               view-lossage



;; <M-prior>       scroll-other-window-down
;; <M-return>      complete-tag
;; <M-right>       forward-word
;; <M-up>          recenter-zero
;; <S-delete>      kill-region
;; <S-down-mouse-1>                mouse-appearance-menu
;; <S-f1>          ??
;; <S-insert>      yank
;; <S-insertchar>  yank



;; M-


;; C-,                                                  scroll-up-slowly
;; C-.                                                  scroll-down-slowly
;; C-x C-l                                              downcase-region
;; C-x *                                                calc-dispatch
;; C-Enter                                              calculator

;; C-h m                                                describe-mode

;; C-h w                                                where-is
;; C-h g                                                describe-generic (describe generic function)

;; C-c q                                                 boxes-create
;; C-c r                                                 boxes-remove

;; C-M-\                                                indent-region

;; C-M-q                                                indent-pp-sexp     (emacs/lisp mode)
;; C-M-q                                                cperl-indent-exp   (cperl mode)


;; Function key map translations Starting With C-x @:
;; key             binding
;; ---             -------

;; C-x @ S         event-apply-shift-modifier
;; C-x @ a         event-apply-alt-modifier
;; C-x @ c         event-apply-control-modifier
;; C-x @ h         event-apply-hyper-modifier
;; C-x @ m         event-apply-meta-modifier
;; C-x @ s         event-apply-super-modifier

;; C-x TAB         (indent-rigidly START END ARG)
   ;; Indent all lines starting in the region sideways by ARG columns.
   ;; Called from a program, takes three arguments, START, END and ARG.
   ;; You can remove all indentation from a region by giving a large negative ARG.

;;; Section for ECB
;; try ecb-show-help
;; Directories: Ctrl-c . g d
;; Methods: Ctrl-c . g m
;; Sources: Ctrl-c . g s
;; History: Ctrl-c . g h


;;{{ Narrowing and Hiding
;; C-x n d         narrow-to-defun
;; C-x n n         narrow-to-region
;; C-x n p         narrow-to-page
;; C-x n s         org-narrow-to-subtree
;; C-x n w         widen

;; s for super
;; s-c hs-hide-all-but-at-point
;;     hs-hide-block-at-point
;; C-c @ C-c       hs-toggle-hiding
;; C-c @ C-h       hs-hide-block
;; C-c @ C-l       hs-hide-level
;; C-c @ C-s       hs-show-block
;; C-c @ ESC       Prefix Command

;; C-c @ C-M-h     hs-hide-all
;; C-c @ C-M-s     hs-show-all
;;}}


;;{{ Planer, Remember
;; s-p     plan
;;}}


;;{{ http://lifealgorithms.wordpress.com/2009/12/02/adjusting-resizing-buffer-size-from-keyboard/
;; Of course there are! Shame on me that I doubted it. Here is the
;; respective info node – it is definitely worth a look. I compiled
;; the default key bindings as I could not find them elsewhere in the
;; Internet.

;; Binding 	Function Name 	Description
;; C-x ^ 	enlarge-window 	enlarge vertically
;; C-x } 	enlarge-window-horizontally 	enlarge horizontally
;; NA 	shrink-window 	shrink vertically
;; C-x { 	shrink-window-horizontally 	shrink horizontally
;; C-x - 	shrink-window-if-larger-than-buffer 	shrink vertically as much as possible
;; NA 	fit-window-to-buffer 	expand for best fit, maybe delete buffer
;; C-x + 	balance-windows 	same height and width for buffers
;; NA 	balance-windows-area 	same area for each buffer

;; The default unit for shrinking and expanding is one column
;; (horizontal) or one row (vertical). This means that with prefixing
;; these commands the desired size can be achieved easily. With for
;; example C-u C-u C-x { or M-1 M-6 C-x { we can shrink the buffer by
;; 16 columns. C-x { and C-x } solve the acute resizing problems, but
;; the other functions surely are also worth trying out.
;;}}


;; TEXT MODE
;; M-q    Fill current paragraph (fill-paragraph).
;; C-x f    Set the fill column (set-fill-column).
;; M-x fill-region   | Fill each paragraph in the region (fill-region).
;; M-x fill-region-as-paragraph |    Fill the region, considering it as one paragraph.
;; M-o M-s |    Center a line.

;;; GNUS
;; C-d, A D, <menu-bar> <Article> <Enter digest buffer>. gnus-summary-enter-digest-group
;; `gnus-agent-group-mode' Minor Mode Bindings Starting With J:
;; key             binding
;; ---             -------


;; Summary Mode
;; t               gnus-summary-toggle-header
;; u               w3m-print-this-url

;; Queue and Agent related.
;; J S             gnus-group-send-queue
;; J Y             gnus-agent-synchronize-flags
;; J a             gnus-agent-add-group
;; J c             gnus-enter-category-buffer
;; J j             gnus-agent-toggle-plugged     NOTE
;; J o             gnus-agent-toggle-group-plugged
;; J r             gnus-agent-remove-group
;; J s             gnus-agent-fetch-session
;; J u             gnus-agent-fetch-groups

;; Draft
;; D e             gnus-draft-edit-message

;; (setq hinitial-scratch-message (purecopy "\

;; C-u g          show raw article gnus-summary-show-article

;; Footnote
;; C-c ! a         Footnote-add-footnote
;; C-c ! b         Footnote-back-to-message
;; C-c ! c         Footnote-cycle-style
;; C-c ! d         Footnote-delete-footnote
;; C-c ! g         Footnote-goto-footnote
;; C-c ! r         Footnote-renumber-footnotes
;; C-c ! s         Footnote-set-style

;; Sending
;; C-c C-f, S o m	   gnus-summary-mail-forward
                           ;; This will not work in message window only work in summary window
;; S ESC           Prefix Command
;; S B             Prefix Command
;; S D             gnus-send-bounce-map
;; S F             gnus-summary-followup-with-original
;; S N             gnus-summary-followup-to-mail-with-original
;; S O             Prefix Command
;; S R             gnus-summary-reply-with-original
;; S V             gnus-summary-very-wide-reply-with-original
;; S W             gnus-summary-wide-reply-with-original
;; S c             gnus-summary-cancel-article
;; S f             gnus-summary-followup
;; S i             gnus-summary-news-other-window
;; S m             gnus-summary-mail-other-window
;; S n             gnus-summary-followup-to-mail
;; S o             Prefix Command
;; S p             gnus-summary-post-news
;; S r             gnus-summary-reply
;; S s             gnus-summary-supersede-article
;; S u             gnus-uu-post-news
;; S v             gnus-summary-very-wide-reply (reply all)
;; S w             gnus-summary-wide-reply      (reply all)
;; S y             gnus-summary-yank-message

;; S D b           gnus-summary-resend-bounced-mail
;; S D e           gnus-summary-resend-message-edit
;; S D r           gnus-summary-resend-message

;; S O m           gnus-uu-digest-mail-forward
;; S O p           gnus-uu-digest-post-forward

;; S o m           gnus-summary-mail-forward
;; S o p           gnus-summary-post-forward

;; S B R           gnus-summary-reply-broken-reply-to-with-original
;; S B r           gnus-summary-reply-broken-reply-to

;; S M-c           gnus-summary-mail-crosspost-complaint


;; B ESC           Prefix Command
;; B B             gnus-summary-crosspost-article
;; B I             gnus-summary-create-article
;; B c             gnus-summary-copy-article
;; B e             gnus-summary-expire-articles
;; B i             gnus-summary-import-article
;; B m             gnus-summary-move-article
;; B p             gnus-summary-article-posted-p
;; B q             gnus-summary-respool-query
;; B r             gnus-summary-respool-article
;; B t             gnus-summary-respool-trace
;; B w             gnus-summary-edit-article
;; B DEL           gnus-summary-delete-article
;; B <backspace>   gnus-summary-delete-article
;; B <delete>      gnus-summary-delete-article
;; B C-M-e         gnus-summary-expire-articles-now


;; Attachment
;; C-d, A D        gnus-summary-enter-digest-group



;; Mime
;; o               gnus-mime-save-part
;; K C             gnus-article-view-part-as-charset
;; K E             gnus-article-encrypt-body
;; K H             gnus-article-browse-html-article
;; K O             gnus-article-save-part-and-strip
;; K b             gnus-summary-display-buttonized
;; K c             gnus-article-copy-part
;; K d             gnus-article-delete-part
;; K e             gnus-article-view-part-externally
;; K i             gnus-article-inline-part
;; K j             gnus-article-jump-to-part
;; K m             gnus-summary-repair-multipart
;; K o             gnus-article-save-part
;; K r             gnus-article-replace-part
;; K t             gnus-article-view-part-as-type
;; K v             gnus-article-view-part
;; K |             gnus-article-pipe-part




(setq Info-directory-list
      '("/usr/share/info/"
        "/usr/local/share/info/")
      Info-additional-directory-list
      '("/usr/share/info/emacs-snapshot"
        "/usr/share/info/emacs-23/"))




;; {{ from: http://www.gnu.org/software/emacs/manual/html_node/emacs/Deletion.html
;; Deletion
;; M-\        Delete spaces and tabs around point (delete-horizontal-space).
;; M-<SPC>    Delete spaces and tabs around point, leaving one space (just-one-space).
;; C-x C-o    Delete blank lines around the current line (delete-blank-lines).
;; M-^        Join two lines by deleting the intervening newline, along with any indentation following it (delete-indentation).
;; }}


;; {{ SLIME
;; M-? runs the command slime-edit-uses, which is an interactive Lisp
;;                        It is bound to M-_, M-?.
;;                        (slime-edit-uses SYMBOL)
;;                        Lookup all the uses of SYMBOL.
;; CL clisp Hyperspec
;; C-c C-d h slime-hyperspec-lookup SYMBOL-NAME
;; M-C-x evaluates current form via SLIME
;; M-C-q re-indents form following cursor
;; C-c C-d h on symbol or function retrieves HyperSpec entry
;; M-x describe-mode for more
;; C-c C-d C-a     slime-apropos
;; C-c C-d C-d     slime-describe-symbol
;; C-c C-d C-f     slime-describe-function
;; C-c C-d C-p     slime-apropos-package
;; C-c C-d C-z     slime-apropos-all
;; C-c C-d #       common-lisp-hyperspec-lookup-reader-macro
;; C-c C-d a       slime-apropos
;; C-c C-d d       slime-describe-symbol
;; C-c C-d f       slime-describe-function
;; C-c C-d h       slime-hyperspec-lookup
;; C-c C-d p       slime-apropos-package
;; C-c C-d z       slime-apropos-all
;; C-c C-d ~       common-lisp-hyperspec-format
;; C-c C-d C-#     common-lisp-hyperspec-lookup-reader-macro
;; C-c C-d C-~     common-lisp-hyperspec-format
;; }}



;; {{ ERROR: TODOs
;; Remove these errors comes when server loads.
;; error: problems setting volume
;; [0x958de64] main input error: ES_OUT_SET_(GROUP_)PCR  is called too late (jitter of 23742 ms ignored)
;; bbdb.el:15:1:Error: Cannot open load file: bbdb-vcard-import
;; g-client.el:2:1:Error: Cannot open load file:
;; }}

;; {{ TODOs
;; * Find about hightlight-change-mode
;; * In scratch.el smooth-step is ready
;; }}

	;; this.onCaCertDelete = function(server)
	;; {
        ;;     var cert_group = this.L2tpObject.cert_group;
	;;     var len = this.L2tpObject.cert_group.length;
	;;     for(var i=0;i<len;i++) {
	;; 	if(server == this.L2tpObject.cert_group[i].server) {
	;; 	    is_exists = true;
	;; 	    len = i;
	;; 	    break;
	;; 	}
	;;     }

	;;     if(is_exists) {
	;; 	this.L2tpObject.cert_group[len]
	;;     // } else {
	;;     //     this.L2tpObject.cert_group[len] = new Object();
	;;     //     this.L2tpObject.cert_group[len].server = server;
	;;     //     this.L2tpObject.cert_group[len].ca_cert = ca_cert;
	;;     }
        ;;     this.certGroup();
        ;; }

(user-provide 'info)

