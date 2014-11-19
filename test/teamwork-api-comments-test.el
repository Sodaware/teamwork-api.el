(require 'teamwork-api-comments)

(ert-deftest teamwork-api-comments-test/can-get-resource-name-from-symbol ()
  (should (string= "links" (teamwork-api--get-resource-name :links))))

(ert-deftest teamwork-api-comments-test/can-get-resource-name-from-quoted ()
  (should (string= "links" (teamwork-api--get-resource-name 'links))))
