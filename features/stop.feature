Feature: Stop

  Background:
    Given I add the following services:
      | name | cwd | command | path    | env               | kill-process-buffer-on-stop |
      | foo  | foo | server  | ("foo") | (("PORT" "6001")) | t                           |
      | bar  | bar | server  | ("bar") | (("PORT" "6002")) | nil                         |
    And I start prodigy

  Scenario: Not started
    When I stop service
    Then I should see message "Service is not running"

  Scenario: Stop process at line
    When I start service
    Then requesting "http://127.0.0.1:6002" should respond with "BAR"
    When I stop service
    Then requesting "http://127.0.0.1:6002" should not respond
    And I should see services:
      | name | highlighted | marked | started |
      | bar  | t           | nil    | nil     |
      | foo  | nil         | nil    | nil     |

  Scenario: Stop marked processes
    When I press "M"
    And I start services
    Then requesting "http://127.0.0.1:6001" should respond with "FOO"
    And requesting "http://127.0.0.1:6002" should respond with "BAR"
    And I should see services:
      | name | highlighted | marked | started |
      | bar  | t           | t      | t       |
      | foo  | nil         | t      | t       |
    And I stop services
    Then requesting "http://127.0.0.1:6001" should not respond
    And requesting "http://127.0.0.1:6002" should not respond
    And I should see services:
      | name | highlighted | marked | started |
      | bar  | t           | t      | nil     |
      | foo  | nil         | t      | nil     |

  Scenario: Kill process buffer globally
    Given I turn on kill process buffer on stop
    When I start service
    Then requesting "http://127.0.0.1:6002" should respond with "BAR"
    And the buffer "*prodigy-bar*" should exist
    And I stop services
    Then the buffer "*prodigy-bar*" should not exist

  Scenario: Kill process buffer locally
    When I press "M"
    And I press "s"
    And I start services
    Then requesting "http://127.0.0.1:6001" should respond with "FOO"
    And requesting "http://127.0.0.1:6002" should respond with "BAR"
    And the buffer "*prodigy-foo*" should exist
    And the buffer "*prodigy-bar*" should exist
    And I stop services
    Then the buffer "*prodigy-foo*" should not exist
    But the buffer "*prodigy-bar*" should exist
