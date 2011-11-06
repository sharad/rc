
;; ref: http://boxes.thomasjensen.com/

;; Jason L. Shiffer kindly submitted the following information on
;; integrating boxes with Emacs: The simple interface (only a single
;; box style, but easy):

(deh-section "art/box"
  (autoload 'boxes-command-on-region "boxes" nil t)
  (autoload 'boxes-remove "boxes" nil t)
  (autoload 'boxes-create "boxes" nil t)

  (defun boxes-create ()
    (interactive)
    (shell-command-on-region
     (region-beginning) (region-end) "boxes -d c-cmt2" nil 1 nil))

  (defun boxes-remove ()
    (interactive)
    (shell-command-on-region
     (region-beginning) (region-end) "boxes -r -d c-cmt2" nil 1 nil))

  ;;Jason also wrote a boxes mode for Emacs. Remember to update the
  ;;design list when you add new designs to your config file.

  ;; try table-insert  !! excellent.
  ;; +-------------+-----------+--------+
  ;; |sssd         |ddddd      |sdfsd   |
  ;; +-------------+-----------+--------+
  ;; |saddsa       |sadfsfsafds|dsasfds |
  ;; +-------------+-----------+--------+
  ;; |asdsadf      |asfdsfdsf  |asdsfdsf|
  ;; +-------------+-----------+--------+



  ;; (Defun box(min max)
  ;;   (let ((min min)
  ;;         (max max))
  ;;     ))
  )

(user-provide 'art)

