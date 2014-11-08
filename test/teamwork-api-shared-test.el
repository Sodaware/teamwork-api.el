(require 'teamwork-api-shared)

(ert-deftest teamwork-api-shared-test/can-generate-user-uri ()
  (should (string= "https://test_user.teamwork.com/"
                   (teamwork-api--generate-user-uri "test_user"))))
