;; Required testing libraries
(require 'cl)
(require 'el-mock)

;; Add library path
(add-to-list 'load-path
             (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; Setup test paths
(setq test-directory (file-name-directory (directory-file-name (file-name-directory load-file-name))))


(defun url-retrieve-user-fixture (file)
  "Reads FILE from the fixtures directory and returns it as a fake HTTP response."

  (let* ((file-path (concat test-directory "/fixtures/" file))
         (file-contents (with-temp-buffer
                          (insert-file-contents file-path)
                          (buffer-string)))
         (buffer-name (format "*http %s.teamwork.com:443*" teamwork-api-username))
         (response-buffer (get-buffer-create buffer-name)))

    ;; Insert the file contents into a new buffer along with a HTTP header
    ;; NOTE: I don't really think we need to bother adding the header, as it's
    ;; snipped off using url-http-end-of-headers
    (with-current-buffer response-buffer
      (insert (concat "HTTP/1.0 200 OK\n\n" file-contents))
      (set (make-local-variable 'url-http-end-of-headers) 18)
      (current-buffer))))
