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
  (let ((project (assoc-default 'project (teamwork-api--get (format "projects/%s" project)))))
    (teamwork-api--format-project project)))

(defun teamwork-api-get-starred-projects ()
  "Retrieve a list of all starred projects for the current user."
  (let* ((response (teamwork-api--get "projects/starred")))
    (assoc-default 'projects response)))

(defun teamwork-api-star-project (project-id)
  "Mark PROJECT-ID as starred."
  (teamwork-api--put (format "projects/%s/star" project-id)))

(defun teamwork-api-unstar-project (project-id)
  "Remove star from PROJECT-ID."
  (teamwork-api--put (format "projects/%s/unstar" project-id)))

;; Data Helpers

;; [todo] - Include company information
;; [todo] - Include category information
(defun teamwork-api--format-project (project)
  "Convert PROJECT from a JSON object."
  `((:id . ,(assoc-default 'id project))
    (,:name . ,(assoc-default 'name project))
    (,:description . ,(assoc-default 'description project))
    (,:status . ,(assoc-default 'status project))
    (,:start-page . ,(assoc-default 'start-page project))
    (,:logo . ,(assoc-default 'logo project))
    (,:start-date . ,(assoc-default 'startDate project))
    (,:end-date . ,(assoc-default 'endDate project))
    (,:last-changed . ,(assoc-default 'last-changed-on project))
    (,:starred . ,(assoc-default 'starred project))))

(provide 'teamwork-api-projects)
;;; teamwork-api-projects.el ends here
