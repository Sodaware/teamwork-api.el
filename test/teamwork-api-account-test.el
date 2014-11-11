(require 'teamwork-api-account)

(ert-deftest teamwork-api-account-test/can-get-account-details ()
  (with-mock
   (mock (teamwork-api--get "account") => (read-fixture-as-json "account.json"))
   (let ((account (teamwork-api-get-account-details)))
     (should-not (null account))
     (should-key-equal :id 1 account)
     (should-key-equal :name "Teamwork Account Name" account)
     (should-key-equal :code "teamworksitecode" account)
     (should-key-equal :url "http://sampleaccount.teamwork.com/" account)
     (should-key-equal :logo "http://www.someteamworkurl.com/images/349C6BDFA9EA4F814B6822C2F8C13A61%2Ejpg" account)
     (should-key-equal :company-id 1 account)
     (should-key-equal :company-name "Owner Company Name" account)
     (should-key-equal :account-holder-id 1 account)
     (should-key-equal :language "EN" account)
     (should-key-equal :ssl-enabled t account)
     (should-key-equal :require-https nil account)
     (should-key-equal :time-tracking-enabled t account)
     (should-key-equal :email-notifications-enabled t account)
     (should-key-equal :created-at "2011-08-22T12:57:00Z" account)
     (should-key-equal :date-signed-up "2013-03-05T00:00:00Z" account)
     (should-key-equal :cache-uuid "C14A34C3-D5AE-86A3-B9A88A5377D2CD79" account))))

(ert-deftest teamwork-api-account-test/can-get-authenticate-details ()
  (with-mock
   (mock (teamwork-api--get-url-as-json "https://authenticate.teamworkpm.net/authenticate.json") => (read-fixture-as-json "authenticate.json"))
   (let ((account (teamwork-api-get-authenticate-details)))
     (should-not (null account))
     (should-key-equal :id 999 account)
     (should-key-equal :user-id 999 account)
     (should-key-equal :first-name "Demo" account)
     (should-key-equal :last-name "User" account)
     (should-key-equal :avatar-url "https://s3.amazonaws.com/TWFiles/2/users/999.avatar" account)
     (should-key-equal :start-on-sundays nil account)
     (should-key-equal :name "Demo Site" account)
     (should-key-equal :code "testing" account)
     (should-key-equal :url "http://demo1company.teamwork.com" account)
     (should-key-equal :logo "http://demo1company.teamwork.com/images/logo.jpg" account)
     (should-key-equal :company-id 999 account)
     (should-key-equal :company-name "Demo 1 Company" account)
     (should-key-equal :language "EN" account)
     (should-key-equal :ssl-enabled nil account)
     (should-key-equal :require-https nil account)
     (should-key-equal :user-is-owner-company-member t account)
     (should-key-equal :user-is-admin t account)
     (should-key-equal :can-add-projects t account)
     (should-key-equal :can-manage-people t account)
     (should-key-equal :time-format "h:mmtt" account)
     (should-key-equal :date-format "dd/mm/yyyy" account)
     (should-key-equal :date-seperator "/" account))))
