;;
;; bookmark.el
;; Login : <s@taj>
;; Started on  Thu Jan 20 23:10:32 2011 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;




(deh-featurep saveplace
  )

;; breadcrumb
(deh-require-maybe breadcrumb
  ;;
  (setq bc-bookmark-file (lotus-cache-file "breadcrumb/breadcrumbe.el"))

  (autoload 'bc-set               "breadcrumb" "Set bookmark in current point."   t)
  (autoload 'bc-previous          "breadcrumb" "Go to previous bookmark."         t)
  (autoload 'bc-next              "breadcrumb" "Go to next bookmark."             t)
  (autoload 'bc-local-previous    "breadcrumb" "Go to previous local bookmark."   t)
  (autoload 'bc-local-next        "breadcrumb" "Go to next local bookmark."       t)
  (autoload 'bc-goto-current      "breadcrumb" "Go to the current bookmark."      t)
  (autoload 'bc-list              "breadcrumb" "List all bookmarks in menu mode." t)
  (autoload 'bc-clear             "breadcrumb" "Clear all bookmarks."             t)

  )

;; bm
(deh-require-maybe bm
  ;;
 )


;; save bookmarks
(deh-require-maybe bookmark+
)




(deh-require-maybe linkd                ;excellent


  )



(provide 'bookmark-config)
