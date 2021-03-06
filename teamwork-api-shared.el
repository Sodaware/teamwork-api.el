;;; teamwork-api-shared.el --- Functionality shared between submodules

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

;; This file contains various bits of code used throughout the teamwork api
;; client.  Mostly stuff to do with authentication, building uri's and fetching
;; json.

;;; Code:

;; Dependencies

(require 'json)
(require 'url-http)

(defvar url-http-end-of-headers)

;; Configuration

(defvar teamwork-api-username nil
  "Your Teamwork.com username.")

(defvar teamwork-api-key nil
  "Your Teamwork.com api key.")

(defconst teamwork-api-endpoint "https://%s.teamwork.com/")
(defconst teamwork-api-authentication-endpoint "https://authenticate.teamworkpm.net/")


;; Sending requests

(defun teamwork-api--get (action &optional args)
  "Perform a HTTP GET request to ACTION with optional ARGS."  
  (let* ((http-auth-token (teamwork-api--create-auth teamwork-api-key))
         (url-request-method "GET")
         (url (teamwork-api--generate-endpoint action args))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic " http-auth-token)))))
    (teamwork-api--get-url-as-json url)))

(defun teamwork-api--post (action &optional args)
  "Perform a HTTP POST request to ACTION with optional ARGS."
  (let* ((http-auth-token (teamwork-api--create-auth teamwork-api-key))
         (url-request-method "POST")
         (url-request-data args)
         (url (format "%s%s.json" (teamwork-api--generate-user-uri teamwork-api-username) action))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic " http-auth-token)))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

(defun teamwork-api--put (action &optional args)
  "Perform a HTTP PUT request to ACTION with optional ARGS."
  (let* ((http-auth-token (teamwork-api--create-auth teamwork-api-key))
         (url-request-method "PUT")
         (url-request-data args)
         (url (format "%s%s.json" (teamwork-api--generate-user-uri teamwork-api-username) action))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic " http-auth-token)))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

(defun teamwork-api--delete (action &optional args)
  "Perform a HTTP DELETE request to ACTION with optional ARGS."
  (let* ((http-auth-token (teamwork-api--create-auth teamwork-api-key))
         (url-request-method "DELETE")
         (url-request-data args)
         (url (format "%s%s.json" (teamwork-api--generate-user-uri teamwork-api-username) action))
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic " http-auth-token)))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

(defun teamwork-api--get-url-as-json (url)
  "Perform a HTTP GET request to URL and return as a parsed JSON object."
  (let* ((http-auth-token (teamwork-api--create-auth teamwork-api-key))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic " http-auth-token)))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))


;; URI generation

(defun teamwork-api--generate-user-uri (username)
  "Generate an api domain for USERNAME."
  (format teamwork-api-endpoint username))

(defun teamwork-api--generate-endpoint (action &optional query-vars)
  "Generate a URI endpoint for ACTION using the current user."
  (format "%s%s.json%s"
          (teamwork-api--generate-user-uri teamwork-api-username)
          action
          (teamwork-api--build-query query-vars)))

(defun teamwork-api--build-query (query-vars)
  "Build a query string using QUERY-VARS.

QUERY-VARS should be a list of symbols and their corresponding
values."
  (if (null query-vars)
      ""
    (let (query-string)
      (dolist (pair query-vars)
        (let ((name (car pair))
              (value (cdr pair)))
          (setq query-string (concat query-string (substring (symbol-name name) 1)))
          (unless (null value)
            (setq query-string (format "%s=%s" query-string value))))
        (setq query-string (concat query-string "&")))
      (concat "?" (substring query-string 0 -1)))))


;; Authorization

(defun teamwork-api--create-auth (api-key)
  "Use API-KEY to create a HTTP authorization token."
  (base64-encode-string (concat api-key ":x") t))


;; JSON response helpers

(defun teamwork-api--create-pair (key field &optional object conversion)
  "Create a new pair with KEY as the value of FIELD.

If OBJECT is not set, will attempt to use global *RESPONSE* variable.  If 
CONVERSION is a valid function, it will be used to convert to the field's value."  
  (let* ((object (if (boundp '*response*) *response* object))
         (value (assoc-default field object)))
    
    ;; Check object is not null
    (when (null object)
      (error "OBJECT must be set if *RESPONSE* is NIL."))
    
    ;; If conversion function specified, convert the value first
    (when (not (null conversion))
      (unless (functionp conversion)
        (error "CONVERSION must be a valid function"))
      (setq value (funcall conversion value)))

    ;; Return the value as a dotted pair
    `(,key . ,value)))


;; HTTP helpers

(defun teamwork-api--parse-http-headers (headers)
  "Parse HTTP HEADERS into an assoc list."
  (let ((header-list (split-string headers "\n")))
    (mapcar (lambda (header)
              (let ((parts (split-string header ": ")))
                `(,(car parts) . ,(cadr parts))))
            header-list)))


(provide 'teamwork-api-shared)
;;; teamwork-api-shared.el ends here
