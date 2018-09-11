;; Preamble


;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Preamble][Preamble:1]]
;;; org-context-clock.el --- org-context-clock               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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
;; Preamble:1 ends here

;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Preamble][Preamble:2]]
(defgroup org-context-clock nil
  "Emacs Org Context Clocking."
  :tag "Org Clock"
  :group 'org-progress)
;; Preamble:2 ends here

;; Required libraries


;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-context-clock/org-context-clock.org::*Required%20libraries][Required libraries:1]]
(require 'org-clock)

 (require 'timer-utils-lotus)
 (eval-when-compile
   (require 'timer-utils-lotus))
 (require 'org-misc-utils-lotus)
 (eval-when-compile
   (require 'org-misc-utils-lotus))
 (require 'lotus-misc-utils)
 (eval-when-compile
   (require 'lotus-misc-utils))

(require 'org-onchange)
;; Required libraries:1 ends here
