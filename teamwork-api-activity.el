;;; teamwork-api-activity.el --- Wrapper for activity endpoints

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

;; Functions for working with activity endpoints on the Teamwork API

;;; Code:

;; Dependencies

(require 'teamwork-api-shared)


;; Constants and Configuration


;; API Methods

(defun teamwork-api-get-latest-activity (&optional count only-starred)
  "Get all activity, optionally limited to COUNT and ONLY-STARRED."
  (let* ((count (if (null count) 60 count))
         (only-starred (if (null only-starred) "false" "true"))
         (response (teamwork-api--get "latestActivity"
                                      `((:maxItems . ,count)
                                        (:onlyStarred . ,only-starred)))))
    (mapcar 'teamwork-api--format-activity
            (assoc-default 'activity response))))

(defun teamwork-api-get-project-activity (project-id &optional count)
  "Get all activity for PROJECT-ID, optionally limited to COUNT."
  (let* ((count (if (null count) 60 count))
         (response (teamwork-api--get (format "%s/latestActivity" project-id)
                                      `((:maxItems . ,count)))))
    (mapcar 'teamwork-api--format-activity
            (assoc-default 'activity response))))

(defun teamwork-api-delete-activity (id)
  "Delete activity log entry referenced by ID."
  (teamwork-api--delete (format "activity/%s" id)))


;; Internal helpers

(defun teamwork-api--format-activity (activity)
  "Convert json represenation of ACTIVITY into a Lisp-friendly structure."
  `((:id . ,(string-to-number (assoc-default 'id activity)))
    (:date . ,(assoc-default 'datetime activity))
    (:due-date . ,(assoc-default 'due-date activity))
    (:description . ,(assoc-default 'description activity))
    (:extra-description . ,(assoc-default 'extradescription activity))
    (:public-info . ,(assoc-default 'publicinfo activity))
    (:user-id . ,(string-to-number (assoc-default 'userid activity)))
    (:item-id . ,(string-to-number (assoc-default 'itemid activity)))
    (:for-user-id . ,(string-to-number (assoc-default 'foruserid activity)))
    (:for-username . ,(assoc-default 'forusername activity))
    (:for-user-avatar-url . ,(assoc-default 'for-user-avatar-url activity))
    (:from-username . ,(assoc-default 'fromusername activity))
    (:from-user-avatar-url . ,(assoc-default 'from-user-avatar-url activity))
    (:type . ,(assoc-default 'type activity))
    (:activity-type . ,(assoc-default 'activitytype activity))
    (:is-private . ,(not (string= "0" (assoc-default 'isprivate activity))))
    (:link . ,(assoc-default 'link activity))
    (:item-link . ,(assoc-default 'itemlink activity))
    (:project-id . ,(string-to-number (assoc-default 'project-id activity)))
    (:project-name . ,(assoc-default 'project-name activity))
    (:todo-list-name . ,(assoc-default 'todo-list-name activity))))


(provide 'teamwork-api-activity)
;;; teamwork-api-activity.el ends here
