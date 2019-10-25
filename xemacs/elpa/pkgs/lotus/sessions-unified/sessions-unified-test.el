;;; sessions-unified-test.el --- Sessions Unified Unit Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
;; Keywords: convenience, abbrev

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



(require 'ert)
(require 'ert-x)

;; https://www.gnu.org/software/emacs/manual/html_node/ert/index.html

(ert-deftest ert-sessions-unified-test-frame-param-spec-id ()
  "Test"
  :expected-result :passed
  :tags '(sessions-unified)
  (should
   (let ((frame-spec-id (frame-parameter (selected-frame) 'frame-spec-id)))
     (and
      frame-spec-id
      (stringp frame-spec-id)))))


(provide 'sessions-unified-test)
;;; sessions-unified-test.el ends here
