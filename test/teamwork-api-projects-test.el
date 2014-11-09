(require 'teamwork-api-projects)

(ert-deftest teamwork-api-projects-test/can-get-projects ()
  (with-mock
   (mock (teamwork-api--get "projects") => (read-fixture-as-json "projects.json"))
   (let ((projects (teamwork-api-get-projects)))
     (should (= 1 (length projects))))))

(ert-deftest teamwork-api-projects-test/can-get-single-project ()
  (with-mock
   (mock (teamwork-api--get "projects/test") => (read-fixture-as-json "projects-test.json"))
   (let ((project (teamwork-api-get-project "test")))
     (should (string= "demo" (assoc-default :name project))))))

(ert-deftest teamwork-api-projects-test/can-get-starred-projects ()
  (with-mock
   (mock (teamwork-api--get "projects/starred") => (read-fixture-as-json "projects.json"))
   (let ((projects (teamwork-api-get-starred-projects)))
     (should (= 1 (length projects))))))
