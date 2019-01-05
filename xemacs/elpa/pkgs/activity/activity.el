;;; activity.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: data

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

;; This package meant to log, analyze and report all emacs activity of
;; user which could further utilized to visualize activity of user
;; during period of time or editing session.

;; Enable Activity for the current buffer by invokingi
;; `activity-mode'. If you wish to activate it globally, use
;; `global-activity-mode'.

;; Set variable `activity-api-key' to your API key. Point
;; `activity-cli-path' to the absolute path of the CLI script
;; (activity-cli.py).

;; See http://nullprogram.com/blog/2013/04/07/ for help
;; add example code directly here for quick reference.

;;; Code:

(provide 'activity)


(defgroup activity nil
  "Customizations for Activity"
  :group 'convenience
  :prefix "activity-")

;; ;;;###autoload
;; (defvar activity-subdirs
;;   (mapcar
;;    #'(lambda (dir)
;;        (expand-file-name dir (file-name-directory load-file-name)))
;;    '("node-dest" "activities")))

;; ;;;###autoload
;; (dolist (dir (mapcar
;;               #'(lambda (dir)
;;                   (expand-file-name dir (file-name-directory load-file-name)))
;;               '("node-dest" "activities")))
;;   (message "adding %s to load path" dir)
;;   (add-to-list 'load-path dir))

;; ;;;###autoload
;; (eval-when-compile
;;   '(dolist (dir (mapcar
;;                  #'(lambda (dir)
;;                      (expand-file-name dir (file-name-directory load-file-name)))
;;                  '("node-dest" "activities")))
;;      (message "adding %s to load path" dir)
;;      (add-to-list 'load-path dir)))

;; ;;;###autoload
;; (defun activity-add-subdirs-load-path ()
;;   (dolist (dir activity-subdirs)
;;     (add-to-list 'load-path dir)))


(require '@)
(require 'activity-base)



(setf @activity
      (@drive-object @activity-base "activities"
                     "Activity class"

                     (def@ @@ :init ()
                       (@^:init)
                       (message "@activity-class :init")
                       (setf @:occuredon (current-time)))

                     (setf
                      @:active      nil
                      @:insinuate   nil
                      @:uninsinuate nil)

                     (def@ @@ :reset ()
                       (@:deactivate-all)
                       (setf
                        @:active      nil
                        @:insinuate   nil
                        @:uninsinuate nil))

                     (def@ @@ :activate (key)
                       (let ((c (assoc key @:insinuate)))
                         (if (member key @:active)
                             (lwarn 'activity "key %s already active" key)
                           (when c
                             (progn
                               (push key @:active)
                               (funcall (cdr c)))))))

                     (def@ @@ :deactivate (key)
                       (let ((c (assoc key @:uninsinuate)))
                         (if (member key @:active)
                             (lwarn 'activity "key %s not active" key)
                           (when c
                             (remove key @:active)
                             (funcall (cdr c))))))


                     (def@ @@ :activate-all ()
                       (dolist (act @:insinuate)
                         (@:activate (car act))))

                     (def@ @@ :deactivate-all ()
                       (dolist (act @:uninsinuate)
                         (@:deactivate (car act))))

                     (def@ @@ :add (key active deactive)
                       (push (cons key active) @:insinuate)
                       (push (cons key deactive) @:uninsinuate))


                     (def@ @@ :inspect ()
                       (message
                        "active: [%s], insinuate: [%s], uninsinuate: [%s]"
                        @:active
                        @:insinuate
                        @:uninsinuate))))

(defun activity-inspect ()
  (interactive)
  (@! @activity :inspect))

;;;###autoload
(defun activity-reset ()
  (interactive)
  (@! @activity :reset))

;;;###autoload
(defun activity-activate (key)
  (interactive
   (list
    (completing-read "activity: "
                     (mapcar #'car (@ @activity :insinuate)))))
  ;; (activity-add-subdirs-load-path)
  (@! @activity :activate key))

;;;###autoload
(defun activity-deactivate (key)
  (interactive
   (list
    (completing-read "activity: "
                     (mapcar #'car (@ @activity :uninsinuate)))))
  ;; (activity-add-subdirs-load-path)
  (@! @activity :activate key))

;;;###autoload
(defun activity-activate-all ()
  (interactive)
  ;; (activity-add-subdirs-load-path)
  (@! @activity :activate-all))

;;;###autoload
(defun activity-deactivate-all ()
  (interactive)
  (@! @activity :deactivate-all))

;;;###autoload
(defun activity-register (key active deactive)
  (interactive)
  (@! @activity :add key active deactive))


(when nil
  (activity-inspect)
  )



;; (require 'buff-trans)
;; (require 'mail-event)
;; (require 'org-clock-trans)



(defun activity-bind-hooks ()
  "Watch for activity in buffers."
  ;; (add-hook 'after-save-hook 'activity-save nil t)
  ;; (add-hook 'auto-save-hook 'activity-save nil t)
  ;; (add-hook 'first-change-hook 'activity-ping nil t)
  )

(defun activity-unbind-hooks ()
  "Stop watching for activity in buffers."
  ;; (remove-hook 'after-save-hook 'activity-save t)
  ;; (remove-hook 'auto-save-hook 'activity-save t)
  ;; (remove-hook 'first-change-hook 'activity-ping t)
  )

(defun activity-turn-on (defer)
  "Turn on Activity."
  (activity-bind-hooks))

(defun activity-turn-off ()
  "Turn off Activity."
  (activity-unbind-hooks))

;;;###autoload
(define-minor-mode activity-mode
  "Toggle Activity (Activity mode)."
  :lighter    " act"
  :init-value nil
  :global     nil
  :group      'activity
  (cond
    (noninteractive (setq activity-mode nil))
    (activity-mode (activity-turn-on t))
    (t (activity-turn-off))))

;;;###autoload
(define-globalized-minor-mode global-activity-mode activity-mode
  (lambda () (activity-mode 1)))

;;; activity.el ends here




;; Debugger entered--entering a function:
;; * command-error-default-function((quit) "" nil)
;; read-from-minibuffer("Session: " nil (keymap (right . ido-exit-minibuffer) (left . ido-delete-backward-updir) (down . ido-next-match) (up . ido-prev-match) (22 . spacemacs/ido-invoke-in-horizontal-split) (20 . spacemacs/ido-invoke-in-new-frame) (19 . spacemacs/ido-invoke-in-vertical-split) (15 . spacemacs/ido-invoke-in-other-window) (33554448 . previous-history-element) (33554446 . next-history-element) (33554444 . ido-next-match-dir) (33554443 . previous-history-element) (33554442 . next-history-element) (33554440 . ido-prev-match-dir) (16 . ido-prev-match) (14 . ido-next-match) (12 . ido-exit-minibuffer) (11 . ido-prev-match) (10 . ido-next-match) (8 . ido-delete-backward-updir) (27 keymap (33554464 . spacemacs/ido-navigation-transient-state/body) (32 . spacemacs/ido-navigation-transient-state/body) (13 . ido-select-text)) (C-return . ido-select-text) keymap (4 . ido-magic-delete-char) (6 . ido-magic-forward-char) (2 . ido-magic-backward-char) (63 . ido-completion-help) (left . ido-prev-match) (right . ido-next-match) (0 . ido-restrict-to-matches) (27 keymap (32 . ido-take-first-match)) (67108896 . ido-restrict-to-matches) (26 . ido-undo-merge-work-directory) (20 . ido-toggle-regexp) (67108908 . ido-prev-match) (67108910 . ido-next-match) (19 . ido-next-match) (18 . ido-prev-match) (16 . ido-toggle-prefix) (13 . ido-exit-minibuffer) (10 . ido-select-text) (32 . ido-complete-space) (9 . ido-complete) (5 . ido-edit-input) (3 . ido-toggle-case) (1 . ido-toggle-ignore) keymap (18 . helm-minibuffer-history) (26 . undefined) ...) nil nil)
;; #[(item prompt hist &optional default require-match initial) "p\306\307\306\307\211\211\211\211\211\211\211\211\211\306\307	\n\307@ABCDEF2GHIJKLMN\310 \210OP\307Q\311\312!\2102\204:\313\314P\"\210\307R\306S\307T\315UJ\203\315 \316=\203} \317V!\203x \320V!\202\260 V\202\260 V;\203\225 \321>\203\220 \322V!\202\260 V\202\260 \323=\205\260 W\205\260 \324XY\"\211Z\205\257 ZA)\211I[\235\203\274 \307I\313\325I\"\210I\203\312 \326\\\307JK\203\325 \307LA\203\257@\327>\203\257B\204\257ED]\330PA\331=@\332=#^_`a\313\333^\"\210^\204Y@\332=\203:\315Xa`_\257\307\211ED\307]\306BA\331=\2052\331@\307A\202\256aE`D_]\306BA\331=\205Q\331@\307A\202\256^\306=\203f\307A\202\256^\334=\203v\306@\307A\202\256PXa`_\257\335^@A@!\210\336P!\203\225\315P^E\307D^]\307S\306\211BA\313\337\306\"\210,B\203\275\307\211BS\202N\323=\203\334\307Db?\205\327c?\205\327\340I!E\202N\341=\203\373\307Db?\205\366c?\205\366\342I!E\202N\316=\203\307D\343I!E\202N\344=\203\307D\345I!E\307CK\203,\306L\307K\346 \210]\203>@\331=\203>\306@d\206K\347\350!\205K(\306e\351 T\307\211fghij(\307R\3522x\353\354k\"P\307Pl\307m%0G.\313\355G\"\210n\203\223\356n!\203\223\357n!\210\313\360R\"\210R\361=\203\265A\331=\203H \334 \203H \307A\306B\202H R\362=\203\311\3062UH\307R\202H R\363>\203\242N\364>\203\206\307oR\365=5XZPp\306q\315Pq\203\2025\203\3661\367k\370P\371Z!ZpP\307p%0\202\210ZpP\202pq\372q!\206'\373Z\322q!p\3065\374Z!\204k\375\376\377Z\"!\203\356\201v 1V\201w Z\306\"\210\3060\202h\210\201x \201y !\210\201z \201{ !\210\307\203\356\335Z\307R\201| =#\210pP\307\211q\204\363-\202H \201} 1\232\201~ k\370PG\"0\202\235\210GP\202H R\201 =\203\261\306B\202H R\201\200 >\203\301\306\2112\202J R\201\201 >\203<\201\202  \203\372\201\203 \201\204 X\"\203H \201\205 \201\206 X\"P\335\201\205 \201{ X\"!\210\306J\202H \201\207  \204H R\201\210 =\203*NUBQBQ\341N\322X\326\201\211 O!P\313\201\212 Q\"\210\335\372X\326\201\211 O!!\210\306J\202H R\201\213 =\203m\313\201\214 Q\"\210Q@rQAQ\335XUP!\210r@NrAP)\202H R\201\215 =\203\243\313\201\216 Q\"\210Q\203H Q@rQAQ\335XUP!\210r@NrAP)\202~e\203\324e\201\217 >\204\324c\203\302\201\220 XGP!\202\306\201\221  \204\324\201\222 \201\223 !\210\202H R\201\224 =\204\342]\204\347G\202\356\201\225 ]@!H\201\226 >\203\377\306\2112\202J H\201\227 \230\204H H\201\230 \230\203*\201\207  \204$\335\372X\326\201\211 O!!\210\306J\202H \201\203 s\2038\201\231 \202;\201\232 H\"\203y\201\207  \203y\201\233 H!\204y\335XH\"\210\313\201\234 H\"\210\201\235  \203s\201\236 R\306\2112\202J \306J\202H \201\203 t\201\237 >\203\213\201\240 \202\216\201\241 H\"\203\241\335\372H!!\210\306J\202H \201\203 \201\242 H\"\203\267\201\243 H!\210\202H \336H!\2033W\203\343\324XY\"\211u\203\330uH\241\210\202\342XHBYBY)\335XH\"\210Q\203-\307rQ@\211r\203)\201\220 XrAP!\203)QA\211Q\203\335XrA\"\210\202!rAPr@N\202\362)\202H \306J\202H \306\2112\203M \201\244 m:\203Om@\206\\\201\245 \202\\m\203Ym\202\\\201\245 H\"\210H.\207" [item ido-case-fold ido-enable-prefix ido-enable-regexp ido-show-confirm-message ido-pre-merge-state t nil ido-setup-completion-map run-hooks ido-setup-hook ido-trace "\n_LOOP_" "" buffer bufferp buffer-name (file dir) file-name-nondirectory file assoc "new default" 0 (t wide) ido-make-merged-file-list auto wide "merged" input-pending-p ido-set-current-directory ido-final-slash "Merged" ido-make-file-list dir ido-make-dir-list ido-make-buffer-list list ido-make-choice-list ido-set-matches boundp max-mini-window-height minibuffer-depth ido read-from-minibuffer ido-make-prompt "read-from-minibuffer" get-buffer kill-buffer "\n_EXIT_" refresh ...] 22 ("/usr/share/emacs/25.2/lisp/ido.elc" . 47957)](list "Session: " nil nil nil nil)
;; ad-Advice-ido-read-internal(#[(item prompt hist &optional default require-match initial) "p\306\307\306\307\211\211\211\211\211\211\211\211\211\306\307	\n\307@ABCDEF2GHIJKLMN\310 \210OP\307Q\311\312!\2102\204:\313\314P\"\210\307R\306S\307T\315UJ\203\315 \316=\203} \317V!\203x \320V!\202\260 V\202\260 V;\203\225 \321>\203\220 \322V!\202\260 V\202\260 \323=\205\260 W\205\260 \324XY\"\211Z\205\257 ZA)\211I[\235\203\274 \307I\313\325I\"\210I\203\312 \326\\\307JK\203\325 \307LA\203\257@\327>\203\257B\204\257ED]\330PA\331=@\332=#^_`a\313\333^\"\210^\204Y@\332=\203:\315Xa`_\257\307\211ED\307]\306BA\331=\2052\331@\307A\202\256aE`D_]\306BA\331=\205Q\331@\307A\202\256^\306=\203f\307A\202\256^\334=\203v\306@\307A\202\256PXa`_\257\335^@A@!\210\336P!\203\225\315P^E\307D^]\307S\306\211BA\313\337\306\"\210,B\203\275\307\211BS\202N\323=\203\334\307Db?\205\327c?\205\327\340I!E\202N\341=\203\373\307Db?\205\366c?\205\366\342I!E\202N\316=\203\307D\343I!E\202N\344=\203\307D\345I!E\307CK\203,\306L\307K\346 \210]\203>@\331=\203>\306@d\206K\347\350!\205K(\306e\351 T\307\211fghij(\307R\3522x\353\354k\"P\307Pl\307m%0G.\313\355G\"\210n\203\223\356n!\203\223\357n!\210\313\360R\"\210R\361=\203\265A\331=\203H \334 \203H \307A\306B\202H R\362=\203\311\3062UH\307R\202H R\363>\203\242N\364>\203\206\307oR\365=5XZPp\306q\315Pq\203\2025\203\3661\367k\370P\371Z!ZpP\307p%0\202\210ZpP\202pq\372q!\206'\373Z\322q!p\3065\374Z!\204k\375\376\377Z\"!\203\356\201v 1V\201w Z\306\"\210\3060\202h\210\201x \201y !\210\201z \201{ !\210\307\203\356\335Z\307R\201| =#\210pP\307\211q\204\363-\202H \201} 1\232\201~ k\370PG\"0\202\235\210GP\202H R\201 =\203\261\306B\202H R\201\200 >\203\301\306\2112\202J R\201\201 >\203<\201\202  \203\372\201\203 \201\204 X\"\203H \201\205 \201\206 X\"P\335\201\205 \201{ X\"!\210\306J\202H \201\207  \204H R\201\210 =\203*NUBQBQ\341N\322X\326\201\211 O!P\313\201\212 Q\"\210\335\372X\326\201\211 O!!\210\306J\202H R\201\213 =\203m\313\201\214 Q\"\210Q@rQAQ\335XUP!\210r@NrAP)\202H R\201\215 =\203\243\313\201\216 Q\"\210Q\203H Q@rQAQ\335XUP!\210r@NrAP)\202~e\203\324e\201\217 >\204\324c\203\302\201\220 XGP!\202\306\201\221  \204\324\201\222 \201\223 !\210\202H R\201\224 =\204\342]\204\347G\202\356\201\225 ]@!H\201\226 >\203\377\306\2112\202J H\201\227 \230\204H H\201\230 \230\203*\201\207  \204$\335\372X\326\201\211 O!!\210\306J\202H \201\203 s\2038\201\231 \202;\201\232 H\"\203y\201\207  \203y\201\233 H!\204y\335XH\"\210\313\201\234 H\"\210\201\235  \203s\201\236 R\306\2112\202J \306J\202H \201\203 t\201\237 >\203\213\201\240 \202\216\201\241 H\"\203\241\335\372H!!\210\306J\202H \201\203 \201\242 H\"\203\267\201\243 H!\210\202H \336H!\2033W\203\343\324XY\"\211u\203\330uH\241\210\202\342XHBYBY)\335XH\"\210Q\203-\307rQ@\211r\203)\201\220 XrAP!\203)QA\211Q\203\335XrA\"\210\202!rAPr@N\202\362)\202H \306J\202H \306\2112\203M \201\244 m:\203Om@\206\\\201\245 \202\\m\203Ym\202\\\201\245 H\"\210H.\207" [item ido-case-fold ido-enable-prefix ido-enable-regexp ido-show-confirm-message ido-pre-merge-state t nil ido-setup-completion-map run-hooks ido-setup-hook ido-trace "\n_LOOP_" "" buffer bufferp buffer-name (file dir) file-name-nondirectory file assoc "new default" 0 (t wide) ido-make-merged-file-list auto wide "merged" input-pending-p ido-set-current-directory ido-final-slash "Merged" ido-make-file-list dir ido-make-dir-list ido-make-buffer-list list ido-make-choice-list ido-set-matches boundp max-mini-window-height minibuffer-depth ido read-from-minibuffer ido-make-prompt "read-from-minibuffer" get-buffer kill-buffer "\n_EXIT_" refresh ...] 22 ("/usr/share/emacs/25.2/lisp/ido.elc" . 47957)] list "Session: " nil nil nil nil)
;; apply(ad-Advice-ido-read-internal #[(item prompt hist &optional default require-match initial) "p\306\307\306\307\211\211\211\211\211\211\211\211\211\306\307	\n\307@ABCDEF2GHIJKLMN\310 \210OP\307Q\311\312!\2102\204:\313\314P\"\210\307R\306S\307T\315UJ\203\315 \316=\203} \317V!\203x \320V!\202\260 V\202\260 V;\203\225 \321>\203\220 \322V!\202\260 V\202\260 \323=\205\260 W\205\260 \324XY\"\211Z\205\257 ZA)\211I[\235\203\274 \307I\313\325I\"\210I\203\312 \326\\\307JK\203\325 \307LA\203\257@\327>\203\257B\204\257ED]\330PA\331=@\332=#^_`a\313\333^\"\210^\204Y@\332=\203:\315Xa`_\257\307\211ED\307]\306BA\331=\2052\331@\307A\202\256aE`D_]\306BA\331=\205Q\331@\307A\202\256^\306=\203f\307A\202\256^\334=\203v\306@\307A\202\256PXa`_\257\335^@A@!\210\336P!\203\225\315P^E\307D^]\307S\306\211BA\313\337\306\"\210,B\203\275\307\211BS\202N\323=\203\334\307Db?\205\327c?\205\327\340I!E\202N\341=\203\373\307Db?\205\366c?\205\366\342I!E\202N\316=\203\307D\343I!E\202N\344=\203\307D\345I!E\307CK\203,\306L\307K\346 \210]\203>@\331=\203>\306@d\206K\347\350!\205K(\306e\351 T\307\211fghij(\307R\3522x\353\354k\"P\307Pl\307m%0G.\313\355G\"\210n\203\223\356n!\203\223\357n!\210\313\360R\"\210R\361=\203\265A\331=\203H \334 \203H \307A\306B\202H R\362=\203\311\3062UH\307R\202H R\363>\203\242N\364>\203\206\307oR\365=5XZPp\306q\315Pq\203\2025\203\3661\367k\370P\371Z!ZpP\307p%0\202\210ZpP\202pq\372q!\206'\373Z\322q!p\3065\374Z!\204k\375\376\377Z\"!\203\356\201v 1V\201w Z\306\"\210\3060\202h\210\201x \201y !\210\201z \201{ !\210\307\203\356\335Z\307R\201| =#\210pP\307\211q\204\363-\202H \201} 1\232\201~ k\370PG\"0\202\235\210GP\202H R\201 =\203\261\306B\202H R\201\200 >\203\301\306\2112\202J R\201\201 >\203<\201\202  \203\372\201\203 \201\204 X\"\203H \201\205 \201\206 X\"P\335\201\205 \201{ X\"!\210\306J\202H \201\207  \204H R\201\210 =\203*NUBQBQ\341N\322X\326\201\211 O!P\313\201\212 Q\"\210\335\372X\326\201\211 O!!\210\306J\202H R\201\213 =\203m\313\201\214 Q\"\210Q@rQAQ\335XUP!\210r@NrAP)\202H R\201\215 =\203\243\313\201\216 Q\"\210Q\203H Q@rQAQ\335XUP!\210r@NrAP)\202~e\203\324e\201\217 >\204\324c\203\302\201\220 XGP!\202\306\201\221  \204\324\201\222 \201\223 !\210\202H R\201\224 =\204\342]\204\347G\202\356\201\225 ]@!H\201\226 >\203\377\306\2112\202J H\201\227 \230\204H H\201\230 \230\203*\201\207  \204$\335\372X\326\201\211 O!!\210\306J\202H \201\203 s\2038\201\231 \202;\201\232 H\"\203y\201\207  \203y\201\233 H!\204y\335XH\"\210\313\201\234 H\"\210\201\235  \203s\201\236 R\306\2112\202J \306J\202H \201\203 t\201\237 >\203\213\201\240 \202\216\201\241 H\"\203\241\335\372H!!\210\306J\202H \201\203 \201\242 H\"\203\267\201\243 H!\210\202H \336H!\2033W\203\343\324XY\"\211u\203\330uH\241\210\202\342XHBYBY)\335XH\"\210Q\203-\307rQ@\211r\203)\201\220 XrAP!\203)QA\211Q\203\335XrA\"\210\202!rAPr@N\202\362)\202H \306J\202H \306\2112\203M \201\244 m:\203Om@\206\\\201\245 \202\\m\203Ym\202\\\201\245 H\"\210H.\207" [item ido-case-fold ido-enable-prefix ido-enable-regexp ido-show-confirm-message ido-pre-merge-state t nil ido-setup-completion-map run-hooks ido-setup-hook ido-trace "\n_LOOP_" "" buffer bufferp buffer-name (file dir) file-name-nondirectory file assoc "new default" 0 (t wide) ido-make-merged-file-list auto wide "merged" input-pending-p ido-set-current-directory ido-final-slash "Merged" ido-make-file-list dir ido-make-dir-list ido-make-buffer-list list ido-make-choice-list ido-set-matches boundp max-mini-window-height minibuffer-depth ido read-from-minibuffer ido-make-prompt "read-from-minibuffer" get-buffer kill-buffer "\n_EXIT_" refresh ...] 22 ("/usr/share/emacs/25.2/lisp/ido.elc" . 47957)] (list "Session: " nil nil nil nil))
;; ido-read-internal(list "Session: " nil nil nil nil)
;; ido-completing-read("Session: " (".scratchpad [No match]" "bemacs: Emacs for me." "Default") nil nil nil)
;; fmsession-read-location-internal(nil)
;; fmsession-read-location(nil)
;; frame-session-set-this-location(#<frame F1 0xc3c680> t)
;; frame-session-restore(#<frame F1 0xc3c680> t)
;; lotus-desktop-session-restore()
;; apply(lotus-desktop-session-restore nil)
;; timer-event-handler([t 23600 62681 778116 nil lotus-desktop-session-restore nil nil 279000])
