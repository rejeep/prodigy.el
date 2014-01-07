Feature: Start

  Background:
    Given I add the following services:
      | name | cwd | command | path    | env               |
      | foo  | foo | server  | ("foo") | (("PORT" "6001")) |
      | bar  | bar | server  | ("bar") | (("PORT" "6002")) |
    And I start prodigy

  Scenario: Already started
    When I start service
    Then requesting "http://127.0.0.1:6002" should respond with "BAR"
    Then I start service
    Then I should see message "Service already started"

  Scenario: Start process at line
    When I start service
    Then requesting "http://127.0.0.1:6002" should respond with "BAR"
    And I should see services:
      | name | highlighted | marked | started |
      | bar  | t           | nil    | t       |
      | foo  | nil         | nil    | nil     |

  Scenario: Start marked processes
    When I press "M"
    When I start services
    Then requesting "http://127.0.0.1:6001" should respond with "FOO"
    And requesting "http://127.0.0.1:6002" should respond with "BAR"
    And I should see services:
      | name | highlighted | marked | started |
      | bar  | t           | t      | t       |
      | foo  | nil         | t      | t       |
