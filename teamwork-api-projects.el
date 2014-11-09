;;; teamwork-api-projects.el --- Project 

;; Copyright (C) 2014 Phil Newton

;; Author: Phil Newton <phil@sodaware.net>
;; Version: 0.1.0
;; Package-Requires: ((json "1.2"))
;; Keywords: bter crypto bitcoin litecoin dogecoin

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

;; This file contains various bits of code used throughout the teamwork api
;; client.  Mostly stuff to do with authentication, building uri's and fetching
;; json.

;;; Code:

;; Dependencies

(require 'teamwork-api-shared)


(defun teamwork-api-get-projects ()
  "Retrieve a list of all projects for the current user."
  (let* ((response (teamwork-api--get "projects")))
    (assoc-default 'projects response)))

(defun teamwork-api-get-project (project)
  "Retrieve data for PROJECT."
  (assoc-default 'project (teamwork-api--get (format "projects/%s" project))))

(provide 'teamwork-api-projects)
;;; teamwork-api-projects.el ends here
