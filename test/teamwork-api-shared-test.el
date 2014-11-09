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
        (query-vars (list :arg "value")))    
    (should (string= "https://test_user.teamwork.com/endpoint.json?arg=value"
                     (teamwork-api--generate-endpoint "endpoint" query-vars)))))

(ert-deftest teamwork-api-shared-test/can-create-auth-token ()
  (should (string= "dGVzdC1rZXk6eA==" (teamwork-api--create-auth "test-key"))))

(ert-deftest teamwork-api-shared-test/can-build-query-with-valid-values ()
  (should (string= "?string-param=value&number-param=1234"
                   (teamwork-api--build-query (list :string-param "value"
                                                    :number-param 1234)))))

(ert-deftest teamwork-api-shared-test/can-build-query-with-empty-list ()
  (should (string= "" (teamwork-api--build-query nil))))

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
