;;; teamwork-api-account.el --- Wrapper for account endpoints

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

;; Functions for working with account endpoints on the Teamwork API

;;; Code:

;; Dependencies

(require 'teamwork-api-shared)


;; Constants and Configuration



;; API Methods

(defun teamwork-api-get-account-details ()
  "Get details for the account connected to the current API key."
  (let ((response (teamwork-api--get "account")))
    (when (string= "OK" (assoc-default 'STATUS response))
      (teamwork-api--format-account (assoc-default 'account response)))))


;; Data Helpers

(defun teamwork-api--format-account (account)
  "Convert ACCOUNT from a JSON object."
  `((:id . ,(string-to-number (assoc-default 'id account)))
    (:name . ,(assoc-default 'name account))
    (:code . ,(assoc-default 'code account))
    (:url . ,(assoc-default 'URL account))
    (:logo . ,(assoc-default 'logo account))
    (:company-id . ,(string-to-number (assoc-default 'companyid account)))
    (:company-name . ,(assoc-default 'companyname account))
    (:account-holder-id . ,(string-to-number (assoc-default 'account-holder-id account)))
    (:language . ,(assoc-default 'lang account))
    (:ssl-enabled . ,(not (eq :json-false (assoc-default 'ssl-enabled account))))
    (:require-https . ,(not (eq :json-false (assoc-default 'requirehttps account))))
    (:time-tracking-enabled . ,(not (eq :json-false (assoc-default 'time-tracking-enabled account))))
    (:email-notifications-enabled . ,(not (eq :json-false (assoc-default 'email-notifications-enabled account))))
    (:created-at . ,(assoc-default 'created-at account))
    (:date-signed-up . ,(assoc-default 'datesignedup account))
    (:cache-uuid . ,(assoc-default 'cacheUUID account))))

(provide 'teamwork-api-account)
;;; teamwork-api-account.el ends here







