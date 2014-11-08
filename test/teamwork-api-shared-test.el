(require 'teamwork-api-shared)

;; Internal helper tests

(ert-deftest teamwork-api-shared-test/can-generate-user-uri ()
  (should (string= "https://test_user.teamwork.com/"
                   (teamwork-api--generate-user-uri "test_user"))))

(ert-deftest teamwork-api-shared-test/can-create-auth-token ()
  (should (string= "dGVzdC1rZXk6eA==" (teamwork-api--create-auth "test-key"))))

(ert-deftest teamwork-api-test/can-build-query-with-valid-values ()
  (should (string= "string-param=value&number-param=1234"
                   (teamwork-api--build-query (list :string-param "value"                                                    :number-param 1234)))))

(ert-deftest teamwork-api-test/can-build-query-with-empty-list ()
  (should (string= "" (teamwork-api--build-query nil))))
