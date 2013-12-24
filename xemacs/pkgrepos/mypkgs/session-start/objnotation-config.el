;;; objnotation-config.el --- object notation like json yaml etc configs

;; Copyright (C) 2013  Sharad Pratap

;; Author: Sharad Pratap <spratap@merunetworks.com>
;; Keywords: languages

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

;; http://www.commandlinefu.com/commands/view/12218/convert-yaml-to-json
;; python -c 'import sys, yaml, json; json.dump(yaml.load(sys.stdin), sys.stdout, indent=4)' < file.yaml > file.json

(deh-require-maybe (progn json yaml-mode)
  )


(provide 'objnotation-config)
;;; objnotation-config.el ends here
