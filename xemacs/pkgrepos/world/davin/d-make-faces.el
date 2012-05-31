;;; d-make-faces.el --- My personal font preferences

;; Copyright (C) 2006-2011 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: Font lock syntax highlighting
;; Version: 1.0

;;; Limitation of Warranty

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains definitions of fonts that are used in the file
;; d-flock.el.

;;; Install Instructions:

;; See the following URL for the latest info and a tarball:

;; http://davin.50webs.com/research/2010/mopa2e2.html#d-make-faces

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'd-make-faces)

;;; Known Bugs:

;; None!

;;;
;;; ENSURES THAT CERTAIN FONTS EXIST:
;;;
(require 'font-lock)

^L

(global-set-key "\C-c\C-l" 'd-fonts)

(defun d-fonts--init-screen-dimensions ()

  (if emacs-dialect--xemacs-p
      (progn
        ;;(set-default-font "-b&h-lucidatypewriter-bold-r-normal-sans-0-0-100-100-m-0-iso8859-1")
        ;;(font-set-face-font 'default "-b&h-lucidatypewriter-bold-r-normal-sans-0-0-100-100-m-0-iso8859-1")
        (set-frame-position (car (frame-list)) 15 15)
        (set-frame-width (car (frame-list)) 120)
        (set-frame-height (car (frame-list)) 45)
        ))

  (if (and os-type--mswindows-p emacs-dialect--gnuemacs-p (string= emacs-version "21.3.1"))
      (progn
        ;;(set-default-font "8x13")
        ;;(set-default-font "-*-Courier-normal-r-*-*-13-97-96-96-c-*-iso8859-1")
        ;;(set-default-font "-*-Terminal-normal-r-*-*-16-120-96-96-c-*-ms-oem")
        ;;(set-default-font   "-*-Terminal-normal-r-*-*-18-135-96-96-c-100-ms-oem") ;;; BIG MS
        ;;(set-frame-position (caadr (current-frame-configuration)) 0 0)
        ;;(set-frame-size     (caadr (current-frame-configuration)) 100 38)
        ;;(set-default-font "-adobe-courier-bold-r-normal--20-140-100-100-m-110-iso8859-1") ;;; GOOD

        ;; (set-default-font "-adobe-courier-medium-r-normal--*-80-*-*-m-*-iso8859-1")
        ;; (set-default-font "-schumacher-clean-medium-r-normal--8-*-*-*-c-50-iso8859-1")

        ;; x-fixed-font-alist
        ))

  (if emacs-dialect--gnuemacs-p
      (progn
        ;;(set-default-font "-*-courier new-normal-r-*-*-17-*-*-*-c-*-fontset-standard")
        ;;(set-default-font "-*-courier new-normal-r-*-*-22-*-*-*-c-*-fontset-standard")
        ;;(set-default-font "8x13")
        )
    )

  (if os-type--linux-p
      (condition-case err
          (progn
            (set-frame-position (selected-frame) 0 0)
            (set-frame-size (caadr (current-frame-configuration)) 141 64)
            ;;(error "schmu")
            (set-default-font "-adobe-courier-medium-r-normal--20-140-100-100-m-110-iso8859-9"))
        (error "error=%s" (cdr err)))
    )

  (d-quote
      (progn
        (set-frame-size (selected-frame) 140 (if emacs-dialect--xemacs-p 45 50))
        (set-frame-position (selected-frame) 0 0)))
  )

(defun d-fonts--init-base-colours ()
  (progn ;; BLUE:

    (make-face 'fg:blue)
    (if prefs-bg-black
        (set-face-foreground 'fg:blue "#6666ff")
      (set-face-foreground 'fg:blue "#0000ff")
      (make-face-bold 'fg:blue))

    (make-face 'fg:lightblue)
    (set-face-foreground 'fg:lightblue (if prefs-bg-black "#77aaff" "#0000ff"))
    (if (not prefs-bg-black)
        (make-face-bold 'fg:lightblue)))

  (progn ;; BROWN:

    (make-face 'bg:brown)
    (set-face-background 'bg:brown "#aa7755")
    (set-face-foreground 'bg:brown bg-colour)
    (make-face 'fg:brown)
    (make-face-bold 'fg:brown)
    (set-face-foreground 'fg:brown "#aa7755"))

  (progn ;; CYAN:

    (make-face 'fg:cyan)
    (make-face-bold 'fg:cyan)
    (set-face-foreground 'fg:cyan "dark cyan")

    (copy-face 'default 'fg:lightcyan)
    (make-face-bold 'fg:lightcyan)
    (if prefs-bg-black
        (set-face-foreground 'fg:lightcyan "#0ff")
      (set-face-foreground 'fg:lightcyan "#0aa"))
    (set-face-background 'fg:lightcyan bg-colour)

    (make-face 'fg:cyan)
    (make-face 'bg:cyan)
    (set-face-background 'bg:cyan "dark cyan")
    (set-face-foreground 'bg:cyan bg-colour))

  (progn ;; GRAY:

    (make-face 'fg:darkgray)
    (set-face-foreground 'fg:darkgray "#888")

    (make-face 'bg:lightgray)
    (set-face-background 'bg:lightgray "#ccc")
    (set-face-foreground 'bg:lightgray "black")

    (make-face 'fg:lightgray)
    (if prefs-bg-black
        (progn
          (set-face-foreground 'fg:lightgray "#ccc")
          ;;(set-face-foreground
          )
      (progn
        (set-face-foreground 'fg:lightgray "#888")
        (set-face-background 'fg:lightgray bg-colour-lighter)))
    )

  (progn ;; GREEN:

    (make-face 'fg:green)
    (make-face-bold 'fg:green)
    (if prefs-bg-black
        (set-face-foreground 'fg:green "#0b0")
      (set-face-foreground 'fg:green "#0b0"))

    (make-face 'bg:green)
    (set-face-background 'bg:green "#00cc00")
    (set-face-foreground 'bg:green bg-colour)

    (make-face 'bg:lightgreen)
    (set-face-background 'bg:lightgreen "lightgreen")
    (set-face-foreground 'bg:lightgreen "black")

    (copy-face 'default 'fg:lightgreen)
    (if prefs-bg-black
        (set-face-foreground 'fg:lightgreen "#0d4")
      (set-face-foreground 'fg:lightgreen "#0c4"))
    (if (not prefs-bg-black)
        (make-face-bold 'fg:lightgreen))
    )

  (progn
    (copy-face 'default 'd-checkpoint-face)
    (set-face-foreground 'd-checkpoint-face "#c0c")
    (set-face-background 'd-checkpoint-face "#fff")
    (make-face-bold 'd-checkpoint-face))
  
  (progn
    (copy-face 'default 'd-debug-face)
    (set-face-foreground 'd-debug-face "#f00")
    (set-face-background 'd-debug-face "#fff")
    (make-face-bold 'd-debug-face))

  (progn ;; ORANGE:
    (make-face 'fg:orange)
    (make-face-bold 'fg:orange)
    (set-face-foreground 'fg:orange "#f80"))

  (progn ;; MAGENTA:
    (make-face 'fg:magenta)
    (make-face-bold 'fg:magenta)
    (set-face-foreground 'fg:magenta "magenta")

    (make-face 'bg:magenta)
    (set-face-background 'bg:magenta "magenta")
    (set-face-foreground 'bg:magenta bg-colour)

    (make-face 'fg:lightmagenta)
    (make-face-bold 'fg:lightmagenta)
    (set-face-foreground 'fg:lightmagenta "magenta")

    ;;
    ;; TODO: sdfasdfadfdf
    ;;
    (progn
      (make-face 'bg:lightmagenta)
      (make-face-bold 'bg:lightmagenta)
      (set-face-background 'bg:lightmagenta "magenta")
      (set-face-foreground 'bg:lightmagenta "white"))
    )

  (progn ;; :

    (make-face 'fg:red)
    (make-face-bold 'fg:red)
    (set-face-foreground 'fg:red "red")

    (make-face 'bg:red)
    (if prefs-bg-black
        (progn
          (set-face-background 'bg:red "#e00")
          (set-face-foreground 'bg:red bg-colour))
      (set-face-background 'bg:red "#f44")
      (set-face-foreground 'bg:red "#fff"))

    (make-face 'fg:lightred)
    (set-face-foreground 'fg:lightred "#f33")
    (if (not prefs-bg-black)
        (make-face-bold 'fg:lightred))

    (progn
      (make-face 'bg:lightred)
      (set-face-foreground 'bg:lightred (if prefs-bg-black "black" "white"))
      (set-face-background 'bg:lightred "red"))

    )

  (progn ;; WHITE:

    (copy-face 'default 'fg:white)
    (make-face-bold 'fg:white)
    (if prefs-bg-black
        (progn
          (set-face-foreground 'fg:white "white")
          )
      (progn
        (set-face-foreground 'fg:white "black")
        ;;(set-face-background 'fg:white bg-colour-lighter)
        )))

  (progn ;; YELLOW:

    (make-face 'fg:yellow)
    (make-face-bold 'fg:yellow)
    (if prefs-bg-black
        (progn
          (set-face-foreground 'fg:yellow "yellow")
          (set-face-background 'fg:yellow bg-colour))
      (progn
        (set-face-foreground 'fg:yellow "black")
        (set-face-background 'fg:yellow "yellow")))

    (progn
      (make-face 'bg:yellow)
      (set-face-background 'bg:yellow "yellow")
      (set-face-foreground 'bg:yellow "black")
      (make-face-bold 'bg:yellow)))

  )

(defun d-fonts--init-pairs ()

  (progn
    (copy-face 'default 'd-face-blue-and-white)
    (set-face-foreground 'd-face-blue-and-white (if prefs-bg-black "white" "white"))
    (set-face-background 'd-face-blue-and-white
                         (cond
                          (prefs-bg-black "#44c")
                          (prefs-lcd-emacs "#aaf")
                          (t "#8888ff")))
    )

  (make-face 'd-face-red-and-white)

  (set-face-foreground 'd-face-red-and-white (if prefs-bg-black "white" "black"))
  (set-face-background 'd-face-red-and-white
                       (cond
                        (prefs-bg-black "red")
                        (prefs-lcd-emacs "#f88")
                        (t "#ffaaaa")))

  (make-face 'd-face-red-and-yellow)
  (set-face-foreground 'd-face-red-and-yellow "#ff0")
  (set-face-background 'd-face-red-and-yellow "#e00")

  (make-face 'd-face-green-and-white)
  (set-face-foreground 'd-face-green-and-white (if prefs-bg-black "white" "black"))
  (set-face-background 'd-face-green-and-white (if prefs-bg-black "#080" "#8f8"))

  (progn
    (make-face 'd-face-green-and-yellow)
    (make-face-bold 'd-face-green-and-yellow)
    (set-face-background 'd-face-green-and-yellow (if prefs-bg-black "#080" "#8f8"))
    (set-face-foreground 'd-face-green-and-yellow (if prefs-bg-black "#ff0" "#000"))
    )

  )

(defun d-fonts--init-d--faces ()

  (progn
    (copy-face 'fg:lightred 'd-face-link)
    (set-face-underline-p 'd-face-link t))

  (progn
    (copy-face 'fg:lightblue 'd-face-line)
    (set-face-underline-p 'd-face-line t))

  (progn
    (if prefs-bg-black
        (copy-face 'fg:cyan 'd-face-html-entity)
      (copy-face 'default 'd-face-html-entity))
    (make-face-bold 'd-face-html-entity)
    (if (not prefs-bg-black)
        (set-face-foreground 'd-face-html-entity "#f00")))

  (progn
    (copy-face 'default 'd-face-cc-global)
    (set-face-foreground 'd-face-cc-global "#0c0")
    (set-face-background 'd-face-cc-global "white")
    (make-face-bold 'd-face-cc-global)
    ) ;; *cool-man*
  
  (progn
    (copy-face 'default 'd-face-el-d-stuff)
    (make-face-bold 'd-face-el-d-stuff)
    (set-face-foreground 'd-face-el-d-stuff (if prefs-bg-black "#cccc88" "#2244cc"))
    )

  (progn 
    (copy-face 'default 'd-face-lisp++-keywords)
    (set-face-background 'd-face-lisp++-keywords "#8f8")
    (set-face-foreground 'd-face-lisp++-keywords "#000")
    (make-face-bold 'd-face-lisp++-keywords))

  (d-quote 123)

  (copy-face 'fg:lightgreen 'd-face-el-quote)
  ;;(set-face-background 'd-face-el-quote "#0ff")
  ;;(set-face-foreground 'd-face-el-quote "#f00")

  (progn
    (copy-face 'default 'd-face-property)
    (make-face-bold 'd-face-property)
    (set-face-foreground 'd-face-property "#f90")
    
    (copy-face 'default 'd-face-property-inverse)
    (make-face-bold 'd-face-property-inverse)
    (set-face-background 'd-face-property-inverse "#f90")
    (set-face-foreground 'd-face-property-inverse "#fff"))
    
  ;; oridinary comment cf: font-lock-comment-face
  ;;; super comment    cf: d-face-super-comment
  (progn
    (copy-face 'default 'd-face-super-comment)
    ;;(set-face-foreground 'd-face-super-comment (if prefs-bg-black "#0f0" "#0a0"))
    (set-face-foreground 'd-face-super-comment (if prefs-bg-black "#0f0" "#f00"))
    (if (not prefs-bg-black)
        (make-face-italic 'd-face-super-comment))
    )
  (copy-face 'd-face-super-comment 'font-lock-doc-face)
  (copy-face 'd-face-super-comment 'font-lock-doc-string-face)

  (progn
    ;; MY CPLUSPLUS FACES:
    (copy-face 'default 'd-face-cc-clib)
    (make-face-bold 'd-face-cc-clib)
    (set-face-foreground 'd-face-cc-clib (if prefs-bg-black "#a88" "#844"))
    (copy-face 'd-face-cc-clib 'd-face-cc-allegro)
    (copy-face 'd-face-cc-clib 'd-face-cc-libd)
    (copy-face 'd-face-cc-clib 'd-face-cc-opengl)

    (copy-face 'default 'd-face-cc-ctor-dtor)
    (set-face-foreground 'd-face-cc-ctor-dtor (if prefs-bg-black "#f44" "#f0f"))
    (make-face-bold 'd-face-cc-ctor-dtor)

    (progn
      (make-face 'd-face-cc-debugging)
      (set-face-background 'd-face-cc-debugging "green")
      (make-face-bold 'd-face-cc-debugging))

    (progn
      (copy-face 'default 'd-face-cc-private)
      (make-face-bold 'd-face-cc-private)
      (set-face-foreground 'd-face-cc-private "black")
      ;;(set-face-background 'd-face-cc-private "#fcc")
      (set-face-background 'd-face-cc-private "#ffbbbb")
      )
    )

  (make-face 'd-face-m4-dnl)
  (set-face-background 'd-face-m4-dnl (if prefs-bg-black "#aa7755" "#fc9"))
  (set-face-foreground 'd-face-m4-dnl "black")

  (progn
    (copy-face 'default 'd-face-special-reference)
    (make-face-bold 'd-face-special-reference)
    (if prefs-bg-black
        (progn
          (set-face-foreground 'd-face-special-reference "#fff")
          (set-face-background 'd-face-special-reference "#666"))
      (progn
        (set-face-foreground 'd-face-special-reference "black")
        (set-face-background 'd-face-special-reference bg-colour-lighter))))

  (if (not (and os-type--mswindows-p emacs-dialect--gnuemacs-p))
      (make-face-bold 'd-face-special-reference))

  (copy-face 'bg:lightmagenta 'd-face-makefile-space)
  (copy-face 'default 'd-face-makefile-tab)
  (set-face-background 'd-face-makefile-tab bg-colour-lighter)
  (copy-face 'd-face-red-and-white 'd-face-linefeed)

  (copy-face 'bg:red 'd-face-m4)
  (set-face-background 'd-face-m4 (if prefs-bg-black "#f00" "#faa"))

  (copy-face 'default 'd-face-m5)
  (set-face-background 'd-face-m5 "lightgreen")
  (set-face-foreground 'd-face-m5 "black")

  (copy-face 'default 'd-face-m6)
  (set-face-background 'd-face-m6 "#ccf")
  (set-face-foreground 'd-face-m6 "black")

  (if prefs-bg-black
      (set-face-foreground 'd-face-m4 "#000")
    (set-face-foreground 'd-face-m4 "#000"))

  (progn
    (copy-face 'default 'd-face-cc-illegal-type)
    (make-face-bold 'd-face-cc-illegal-type)
    (set-face-foreground 'd-face-cc-illegal-type "red")
    (set-face-background 'd-face-cc-illegal-type "#0ff")
    (copy-face 'd-face-cc-illegal-type 'd-face-cc-searching)
    )

  (progn
    (make-face 'd-face-makefile-dollar-dollar)
    (set-face-background 'd-face-makefile-dollar-dollar (if prefs-bg-black "#090" "#8f8"))
    (set-face-foreground 'd-face-makefile-dollar-dollar (if prefs-bg-black "#fff" "#000"))
    (copy-face 'd-face-makefile-dollar-dollar 'd-face-makefile-dollar-dollar-highlight)
    (make-face-bold 'd-face-makefile-dollar-dollar-highlight)
    ;;(make-face 'd-face-makefile-dollar-dollar-highlight)
    ;;(set-face-background 'd-face-makefile-dollar-dollar-highlight (if prefs-bg-black "#8b0" "#8f8"))
    ;;(set-face-foreground 'd-face-makefile-dollar-dollar-highlight (if prefs-bg-black "#ff0" "#fff"))
    )

  (progn
    (copy-face 'd-face-cc-illegal-type 'd-face-cc-digits)
    (set-face-background 'd-face-cc-digits bg-colour)

   (if prefs-bg-black
        (progn
          (set-face-background 'd-face-cc-digits "#666")
          (set-face-foreground 'd-face-cc-digits "#fff")
          )
      ;;(set-face-foreground 'd-face-cc-digits "#000")
      ;;(set-face-background 'd-face-cc-digits "#e8d4ff")
      (set-face-background 'd-face-cc-digits bg-colour)
      (set-face-foreground 'd-face-cc-digits "#f0f"))
   
      )
  )

;; (d-fonts--init-standard-font-lock)
(defun d-fonts--init-standard-font-lock ()
  
  (progn
    ;;
    ;; NOTE: Version 22 bindings
    ;;
    (make-face 'mode-line-buffer-id)
    (make-face 'tabbar-default-face)
    (set-face-foreground 'tabbar-default-face "#000")
    (set-face-background 'tabbar-default-face "#eee")
    )

  (progn
    (copy-face 'default 'underline)
    (set-face-underline-p 'underline t))

  (progn
    (copy-face 'default 'bold-underline)
    (make-face-bold 'bold-underline)
    (set-face-underline-p 'bold-underline t))
;;
;;  (progn
;;    (copy-face 'default 'bold-italic)
;;    (make-face-bold 'bold-italic)
;;    (make-face-italic 'bold-italic))
;;
  ;;(set-face-foreground 'italic "#0af")
  ;;(set-face-foreground 'bold "#dd0")
  ;;(copy-face 'fg:lightcyan             'bold-italic)

  ;;(copy-face 'bg:lightmagenta          'highlight)

  (copy-face 'fg:lightcyan             'font-lock-type-face)
  (copy-face 'd-face-blue-and-white      'font-lock-string-face)
  (setq font-lock-string-face          'font-lock-string-face)
  ;;(set-face-background 'font-lock-string-face (if prefs-bg-black "#0000ff"  "#dedeff"))

  (progn
    (set-face-foreground 'font-lock-type-face (if prefs-bg-black "#00ffff" "#0000ff"))
    (make-face-bold 'font-lock-type-face)
    (set-face-foreground 'font-lock-variable-name-face (if prefs-bg-black "#009999" "#0000ff")))

  (copy-face 'default 'font-lock-comment-face)
  (set-face-foreground 'font-lock-comment-face
                       (cond
                        (prefs-bg-black  "#080")
                        (prefs-lcd-emacs "#888")
                        (t "#999")))
  (if (not prefs-bg-black)
      (make-face-italic 'font-lock-comment-face))

  (progn
    (copy-face 'default 'font-lock-keyword-face)
    (if prefs-bg-black
        (progn
          (copy-face 'fg:lightred 'font-lock-keyword-face)
          ;;(set-face-foreground 'font-lock-keyword-face "white")
          ))
    (make-face-bold 'font-lock-keyword-face)
    )

  (if prefs-bg-black
      (copy-face 'fg:white 'font-lock-reference-face)
    (copy-face 'fg:lightgray 'font-lock-reference-face))

  ;; DLDLD
  ;;(require 'foo)
  (set-face-background 'font-lock-reference-face bg-colour-lighter)

  (progn
    (if prefs-bg-black
        (copy-face 'fg:yellow 'font-lock-function-name-face)
      (copy-face 'bg:yellow 'font-lock-function-name-face))
    (make-face-bold 'font-lock-function-name-face))

  (if prefs-bg-black
      (copy-face 'bg:green 'show-paren-match-face)
    (copy-face 'd-face-cc-illegal-type 'show-paren-match-face)
    (set-face-foreground 'show-paren-match-face "#000")
    )
  (if emacs-dialect--xemacs-p (copy-face 'show-paren-match-face 'paren-match))

  (set-face-underline-p 'font-lock-reference-face t)

  (progn
    (copy-face 'default 'font-lock-builtin-face)
    (make-face-bold 'font-lock-builtin-face)
    (set-face-foreground 'font-lock-builtin-face "#f00")
    ;;(set-face-background 'font-lock-builtin-face "#0f0")
    ;;(set-face-background 'font-lock-builtin-face bg-colour-lighter)
    )

  (progn
    (copy-face 'default 'font-lock-constant-face)
    (make-face-bold 'font-lock-constant-face)
    
    (if (not prefs-bg-black)
        (set-face-foreground 'font-lock-constant-face "#f00"))

    (d-quote (if prefs-bg-black
               (set-face-background 'font-lock-constant-face "#000")
             (progn
               (set-face-foreground 'font-lock-constant-face "black")
               (set-face-background 'font-lock-constant-face bg-colour-lighter)))))

  (progn
    (make-face 'highlight)
    (set-face-foreground 'highlight "#fff")
    (set-face-background 'highlight "#88f"))

  (progn
    (copy-face 'default 'region)
    (set-face-foreground 'region "#fff")
    (set-face-background 'region (if prefs-bg-black "#f4a" "#f8a")))

  (progn
    (copy-face 'default 'font-lock-warning-face)
    (set-face-foreground 'font-lock-warning-face "red")
    ;;(set-face-background 'font-lock-warning-face bg-colour-lighter)
    (make-face-bold 'font-lock-warning-face))

  (if (not (and os-type--mswindows-p emacs-dialect--gnuemacs-p))
      (make-face-bold 'font-lock-warning-face))

  (copy-face 'default 'secondary-selection)
  (set-face-foreground 'secondary-selection "lightgreen")
  (copy-face 'd-face-cc-searching 'secondary-selection)

  (copy-face 'bg:lightmagenta 'info-menu-5)
  (copy-face 'fg:lightcyan 'info-node)

  (copy-face 'bold 'info-xref)
  (set-face-underline-p 'info-xref t)
  (set-face-foreground 'info-xref "#00f")1

  (copy-face 'region                   'd-face-cc-searching)
  (copy-face 'fg:darkgray              'dired-face-permissions)
  (set-face-background                 'dired-face-permissions bg-colour-lighter)
  (copy-face 'fg:white                 'modeline-buffer-id)

  (progn
    (copy-face 'd-face-blue-and-white 'dired-marked)
    (make-face-bold 'dired-marked)
    (copy-face 'd-face-red-and-white  'dired-flagged)
    (set-face-background 'dired-flagged "#ffbbbb")
    (make-face-bold 'dired-flagged)
    
    (copy-face 'fg:lightblue          'dired-directory)
    (set-face-background 'dired-directory "#ffffff")
    (set-face-foreground 'dired-directory "#4433ff")
    (copy-face 'fg:green              'd-face-path)
    )

  (copy-face 'd-face-blue-and-white      'dired-face-marked)
  (copy-face 'd-face-red-and-white       'dired-face-flagged)
  (copy-face 'region                     'zmacs-region)
  (copy-face 'region                     'primary-selection)
  (copy-face 'font-lock-reference-face   'font-lock-preprocessor-face)
  (copy-face 'fg:lightgray               'modeline-mousable)
  (copy-face 'fg:red                     'modeline-mousable-minor-mode)

  (copy-face (if prefs-bg-black 'fg:lightred 'fg:lightblue) 'diary-face)

  (when emacs-dialect--xemacs-p
    (set-face-foreground 'blue       "#88f")
    (copy-face 'fg:lightblue         'hyper-apropos-hyperlink)
    (copy-face 'fg:lightred          'hyper-apropos-documentation)
    (copy-face 'fg:lightblue         'custom-group-tag-face)
    (copy-face 'fg:darkgray          'dired-face-boring)
    (set-face-foreground             'dired-face-boring "#888")
    (copy-face 'fg:lightgray         'dired-face-permissions)
    (set-face-foreground             'dired-face-permissions "#999")
    (copy-face 'fg:lightcyan         'dired-face-symlink)
    ;;(copy-face 'd-face-green-and-yellow 'isearch)
    (copy-face 'd-face-green-and-white 'isearch-secondary)
    (copy-face 'bg:lightgray         'modeline-buffer-id)
    ;;(copy-face 'bg:lightgray 'widget-button-face)
    )

  (progn
    (copy-face 'default 'isearch-lazy-highlight-face)
    (set-face-foreground 'isearch-lazy-highlight-face "#000")
    (set-face-background 'isearch-lazy-highlight-face "#0ff"))

  (copy-face 'd-face-blue-and-white  'secondary-selection)
  
  (progn
    (set-face-background 'isearch "#000")
    (set-face-foreground 'isearch "#ff0000")
    (make-face-bold 'isearch))
  
  (progn
    (make-face 'dired-directory)
    (make-face-bold 'dired-directory)
    (set-face-foreground 'dired-directory "#00f"))

  )

(defun d-fonts ()
  (interactive)

  (setq prefs-bg-black (not prefs-bg-black))

  (setq bg-colour (if prefs-bg-black "#262626" "#ffffff"))

  ;;(setq bg-colour "#ffffff")

  (setq bg-colour-lighter (if prefs-bg-black "#444" "#eee"))
  (setq bg-colour-lighter-lighter (if prefs-bg-black "#555" "#ddd"))

  (if (not emacs-dialect--xemacs-p) (set-background-color bg-colour))

  (setq cursor-colour (if prefs-bg-black "#ffff00" "#88cc88"))
  ;;(setq cursor-colour "#ff0000")
  ;; 
  (when (not emacs-dialect--xemacs-p)
    (make-face 'cursor)
    (set-face-background 'cursor cursor-colour)
    (set-face-foreground 'cursor "black"))

  (if emacs-dialect--xemacs-p
      (progn
        (make-face 'text-cursor)
        (set-face-background 'text-cursor cursor-colour)
        (set-face-foreground 'text-cursor "#000")))


  ;; (copy-face 'fg:yellow 'border)
  ;;(set-face-foreground 'default "black")
  (set-face-foreground 'default (if prefs-bg-black  "lightgray" "black"))

  (if (string= (user-real-login-name) "root")
      (set-face-background 'default "#660000")
    (set-face-background 'default bg-colour))
  ;;(set-face-background 'default "#ffffff")

  (set-face-background 'modeline "grey")
  (set-face-foreground 'modeline "black")

  (d-fonts--init-screen-dimensions)
  (d-fonts--init-base-colours)
  (d-fonts--init-pairs)
  (d-fonts--init-d--faces)
  (d-fonts--init-standard-font-lock)

  (setq font-lock-maximum-size nil)
  (setq font-lock-maximum-decoration t)

  ;; get graphics characters online...
  (if (and (< emacs-major-version 20) (not emacs-dialect--xemacs-p))
      (let ((i 127))
        (while (< i 256)
          (aset standard-display-table i (vector i))
          (incf i))))

  (balance-windows)
  ;;(recenter)

  (if (and os-type--mswindows-p emacs-dialect--gnuemacs-p) (set-cursor-color cursor-colour))

  )

(setq prefs-bg-black t)

(d-fonts)

(provide 'd-make-faces)


