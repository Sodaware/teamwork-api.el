(require 'teamwork-api-shared)

(ert-deftest teamwork-api-shared-test/can-generate-user-uri ()
  (should (string= "https://test_user.teamwork.com/"
                   (teamwork-api--generate-user-uri "test_user"))))

(ert-deftest teamwork-api-shared-test/can-create-auth-token ()
  (should (string= "dGVzdC1rZXk6eA==" (teamwork-api--create-auth "test-key"))))
