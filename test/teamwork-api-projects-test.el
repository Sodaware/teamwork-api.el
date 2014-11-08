(require 'teamwork-api-projects)

(ert-deftest teamwork-api-projects-test/can-get-projects ()
  (with-mock
   (mock (teamwork-api--get "projects") => (read-fixture-as-json "projects.json"))
   (let ((projects (teamwork-api-get-projects)))
     (should (= 1 (length projects))))))
