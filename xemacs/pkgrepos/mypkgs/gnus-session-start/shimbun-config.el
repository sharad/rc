;;; shimbun-config.el --- GNUS shimbun related behaviours.

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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



(deh-require (and shimbun sb-rss-blogs)

  (add-to-list 'shimbun-rss-blogs-group-url-regexp
               '("OSNews" "http://www.osnews.com/files/recent.xml"))

  (add-to-list 'shimbun-rss-blogs-group-url-regexp
               '("LinuxToday" "http://feedproxy.google.com/linuxtoday/linux"))


  (add-to-list 'shimbun-rss-blogs-group-url-regexp
               '("NDTV" "http://feeds2.feedburner.com/NdtvNews-TopStories")))

(setq shimbun-atom-hash-group-path-alist
      '(("LinuxToday" "http://feedproxy.google.com/linuxtoday/linux" t)
        ("OSNews" "http://www.osnews.com/files/recent.xml" t)
        ("PlanetEmacsen" "http://planet.emacsen.org/atom.xml" t)
        ("Stackoverflow: Emacs" "http://stackoverflow.com/feeds/tag/emacs" t))

      shimbun-rss-hash-group-path-alist
      '(("LinuxToday" "http://feedproxy.google.com/linuxtoday/linux" t)
        ("OSNews" "http://www.osnews.com/files/recent.xml" t)
        ("Stackoverflow: Emacs" "http://stackoverflow.com/feeds/tag/emacs" t)))



(provide 'shimbun-config)
;;; shimbun.el ends here
