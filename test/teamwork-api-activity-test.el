(require 'teamwork-api-activity)

(ert-deftest teamwork-api-activity-test/can-get-latest-activity ()
  (with-mock
   (mock (teamwork-api--get "latestActivity") => (read-fixture-as-json "latestActivity.json"))
   (let* ((activity (teamwork-api-get-latest-activity))
          (first-item (car activity)))
     (should-not (null first-item))
     (should-key-equal :user-id 999 first-item)
     (should-key-equal :project-id 999 first-item)
     (should-key-equal :item-id 999 first-item)
     (should-key-equal :todo-list-name "Things to do" first-item)
     (should-key-equal :from-user-avatar-url "https://s3.amazonaws.com/TWFiles/2/users/999.avatar" first-item)
     (should-key-equal :description "Register domain" first-item)
     (should-key-equal :for-username "For User Name" first-item)
     (should-key-equal :public-info "Public Info" first-item)
     (should-key-equal :for-user-id 123 first-item)
     (should-key-equal :item-link "tasklists/58758" first-item)
     (should-key-equal :date "2014-03-28T15:24:58Z" first-item)
     (should-key-equal :activity-type "new" first-item)
     (should-key-equal :project-name "demo" first-item)
     (should-key-equal :link "tasks/436522" first-item)
     (should-key-equal :extra-description "Things to do" first-item)
     (should (null (assoc-default :is-private first-item)))
     (should-key-equal :id 999 first-item)
     (should-key-equal :due-date "20140329" first-item)
     (should-key-equal :from-username "Demo U." first-item)
     (should-key-equal :type "task" first-item)
     (should-key-equal :for-user-avatar-url "http://demo1company.teamwork.com/images/photo.jpg" first-item))))

(ert-deftest teamwork-api-activity-test/can-get-project-activity ()
  (with-mock
   (mock (teamwork-api--get "1/latestActivity") => (read-fixture-as-json "project-latestActivity.json"))
   (let* ((activity (teamwork-api-get-project-activity 1))
          (first-item (car activity)))
     (should-not (null first-item))
     (should-key-equal :user-id 999 first-item)
     (should-key-equal :project-id 999 first-item)
     (should-key-equal :item-id 999 first-item)
     (should-key-equal :from-user-avatar-url "https://s3.amazonaws.com/TWFiles/2/users/999.avatar" first-item)
     (should-key-equal :description " This is a test message " first-item)
     (should-key-equal :for-username "For User Name" first-item)
     (should-key-equal :public-info "Public Info" first-item)
     (should-key-equal :for-user-id 123 first-item)
     (should-key-equal :item-link "" first-item)
     (should-key-equal :date "2014-03-31T11:00:45Z" first-item)
     (should-key-equal :activity-type "new" first-item)
     (should-key-equal :project-name "demo" first-item)
     (should-key-equal :link "messages/12#pmp16" first-item)
     (should-key-equal :extra-description "" first-item)
     (should (null (assoc-default :is-private first-item)))
     (should-key-equal :id 999 first-item)
     (should-key-equal :due-date "" first-item)
     (should-key-equal :from-username "Demo U." first-item)
     (should-key-equal :type "message" first-item)
     (should-key-equal :for-user-avatar-url "http://demo1company.teamwork.com/images/photo.jpg" first-item))))
