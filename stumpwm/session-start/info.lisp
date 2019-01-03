

;; http://quickdocs.org/stumpwm/api
;;
;; Variable *NEW-WINDOW-PREFERRED-FRAME*
;;
;; '(:empty :focused)
;;
;; This variable controls what frame a new window appears in. It is a list of
;; preferences. The first preference that is satisfied is used. Valid list
;; elements are as follows: @table @code @item :focused Choose the focused
;; frame. @item :last Choose the last focused frame. @item :empty Choose any
;; empty frame. @item :unfocused Choose any unfocused frame. @end table
;; Alternatively, it can be set to a function that takes one argument, the new
;; window, and returns the preferred frame or a list of the above preferences.
;;
;; Variable *MOUSE-FOCUS-POLICY*
;;
;; :ignore
;;
;; The mouse focus policy decides how the mouse affects input focus. Possible
;; values are :ignore, :sloppy, and :click. :ignore means stumpwm ignores the
;; mouse. :sloppy means input focus follows the mouse; the window that the mouse
;; is in gets the focus. :click means input focus is transfered to the window
;; you click on.

;; Function LOOKUP-COMMAND (keymap command)
;; Return a list of keys that are bound to command
