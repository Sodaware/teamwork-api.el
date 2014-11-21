(require 'teamwork-api-shared)

;; Internal helper tests

(ert-deftest teamwork-api-shared-test/can-generate-user-uri ()
  (should (string= "https://test_user.teamwork.com/"
                   (teamwork-api--generate-user-uri "test_user"))))

(ert-deftest teamwork-api-shared-test/can-generate-endpoint-without-query-vars ()
  (let ((teamwork-api-username "test_user"))    
    (should (string= "https://test_user.teamwork.com/endpoint.json"
                     (teamwork-api--generate-endpoint "endpoint")))))

(ert-deftest teamwork-api-shared-test/can-generate-endpoint-with-query-vars ()
  (let ((teamwork-api-username "test_user")
        (query-vars '((:arg . "value"))))    
    (should (string= "https://test_user.teamwork.com/endpoint.json?arg=value"
                     (teamwork-api--generate-endpoint "endpoint" query-vars)))))

(ert-deftest teamwork-api-shared-test/can-create-auth-token ()
  (should (string= "dGVzdC1rZXk6eA==" (teamwork-api--create-auth "test-key"))))

(ert-deftest teamwork-api-shared-test/can-build-query-with-valid-values ()
  (should (string= "?string-param=value&number-param=1234"
                   (teamwork-api--build-query '((:string-param . "value")
                                                (:number-param . 1234))))))

(ert-deftest teamwork-api-shared-test/can-build-query-with-empty-list ()
  (should (string= "" (teamwork-api--build-query nil))))

(ert-deftest teamwork-api-shared-test/can-build-query-with-nil-values ()
  (should (string= "?string-param&number-param"
                   (teamwork-api--build-query '((:string-param . nil)
                                                (:number-param . nil))))))

(ert-deftest teamwork-api-shared-test/can-make-get-request ()
  (let ((teamwork-api-username "test_user")
        (teamwork-api-key "test-key")
        (teamwork-uri "https://test_user.teamwork.com/test.json"))
    (with-mock
     (mock (url-retrieve-synchronously teamwork-uri) => (url-retrieve-user-fixture "test.json"))
     (teamwork-api--get "test"))))

(ert-deftest teamwork-api-shared-test/can-make-post-request ()
  (let ((teamwork-api-username "test_user")
        (teamwork-api-key "test-key")
        (teamwork-uri "https://test_user.teamwork.com/test.json"))
    (with-mock
     (mock (url-retrieve-synchronously teamwork-uri) => (url-retrieve-user-fixture "test.json"))
     (teamwork-api--post "test"))))


;; Pair creation tests

(ert-deftest teamwork-api-shared-test/can-create-pair ()
  (let* ((json-response (read-fixture-as-json "account.json"))
         (account (assoc-default 'account json-response))
         (test-value (teamwork-api--create-pair :company-name 'companyname account)))
    (should (equal :company-name (car test-value)))
    (should (string= "Owner Company Name" (cdr test-value)))))

(ert-deftest teamwork-api-shared-test/can-create-pair-with-converted-value ()
  (let* ((json-response (read-fixture-as-json "account.json"))
         (account (assoc-default 'account json-response))
         (test-value (teamwork-api--create-pair :company-id 'companyid account 'string-to-number)))
    (should (equal :company-id (car test-value)))
    (should (eq 1 (cdr test-value)))))

(ert-deftest teamwork-api-shared-test/can-use-global-value-to-create-pair ()
  (let* ((json-response (read-fixture-as-json "account.json"))
         (*response* (assoc-default 'account json-response))
         (test-value (teamwork-api--create-pair :company-name 'companyname)))
    (should (string= "Owner Company Name" (cdr test-value)))))

(ert-deftest teamwork-api-shared-test/create-pair-throws-error-with-invalid-arguments ()
  (let* ((json-response (read-fixture-as-json "account.json"))
         (account (assoc-default 'account json-response))
         (*response* nil))
    (should-error (teamwork-api--create-pair :symbol 'key account))
    (should-error (teamwork-api--create-pair :symbol 'key account 'some-invalid-function))))


;; HTTP Helper tests

(ert-deftest teamwork-api-shared-test/can-parse-http-headers ()
  (let* ((headers "HTTP/1.1 200 OK\nContent-Type: text/html\nConnection: keep-alive\n")
         (parsed-headers (teamwork-api--parse-http-headers headers)))
    (should (string= "text/html" (assoc-default "Content-Type" parsed-headers)))))
