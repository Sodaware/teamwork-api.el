;;; teamwork-api-comment.el --- Wrapper for comment endpoints

;; Copyright (C) 2014 Phil Newton

;; Author: Phil Newton <phil@sodaware.net>

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

;; Functions for working with comment endpoints on the Teamwork API

;;; Code:

;; Dependencies

(require 'teamwork-api-shared)


;; Constants and Configuration


;; API Methods

(defun teamwork-api-get-recent-comments (resource resource-id)
  )


(defun teamwork-api--get-resource-name (resource)
  "Get the teamwork name for RESOURCE symbol."
  (replace-regexp-in-string ":" "" (downcase (format "%s" resource))))

(provide 'teamwork-api-comments)
;;; teamwork-api-comments.el ends here
