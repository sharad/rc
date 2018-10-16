;;; math-config.el --- Math and Maxima

;; Copyright (C) 2014  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords: tools

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


(deh-require-maybe (progn maxima imaxima)
  ;; M-x imaxima
  ;; http://www.emacswiki.org/emacs/MaximaMode
  ;; (add-to-list 'load-path "/usr/local/share/maxima/5.18.1/emacs/")
  (autoload 'maxima-mode "maxima" "Maxima mode" t)
  (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
  (autoload 'maxima "maxima" "Maxima interaction" t)
  (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
  (setq imaxima-use-maxima-mode-flag t)
  (add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode)))

(deh-require-maybe gnuplot
  ;; M-x gnuplot-make-buffer
  ;; http://www.emacswiki.org/emacs/GnuplotMode
  ;; see http://xafs.org/BruceRavel/GnuplotMode
  ;; see http://xafs.org/BruceRavel/GnuplotMode?action=AttachFile&do=view&target=gpelcard.pdf
  ;; this line automatically causes all files with the .gp extension to
  ;; be loaded into gnuplot mode
  (setq auto-mode-alist
        (append
         (list
          '("\\.gp$" . gnuplot-mode)
          '("\\.plt$" . gnuplot-mode))
         auto-mode-alist))

  ;; if you have the latest win32 version of gnuplot
  (add-hook 'gnuplot-load-hook
            '(lambda ()
              ;; (setq gnuplot-gnuplot-buffer "plot.plt") ; name of a new gnuplot file
              ;;; (setq show-trailing-whitespace t)
              (setq whitespace-check-buffer-ateol t))))



(provide 'math-config)
;;; math-config.el ends here
