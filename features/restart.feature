Feature: Restart

  Background:
    Given I add the following services:
      | name | cwd | command | path    | env               |
      | foo  | foo | server  | ("foo") | (("PORT" "6001")) |
      | bar  | bar | server  | ("bar") | (("PORT" "6002")) |
    And I start prodigy

  Scenario: At line not started
    When I restart service
    Then requesting "http://127.0.0.1:6002" should respond with "BAR"

  Scenario: At line started
    When I start service
    And I restart service
    Then requesting "http://127.0.0.1:6002" should respond with "BAR"

  Scenario: Marked not started
    When I press "M"
    And I restart services
    Then requesting "http://127.0.0.1:6001" should respond with "FOO"
    And requesting "http://127.0.0.1:6002" should respond with "BAR"

  Scenario: Marked started
    When I press "M"
    And I start services
    And I restart services
    Then requesting "http://127.0.0.1:6001" should respond with "FOO"
    And requesting "http://127.0.0.1:6002" should respond with "BAR"
