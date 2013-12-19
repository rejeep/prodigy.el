Feature: Path

  Scenario: Add path
    Given I add the following services:
      | name | cwd | command | path    | env               |
      | foo  | foo | server  | ("foo") | (("PORT" "6001")) |
    And I start prodigy
    When I start service
    Then requesting "http://127.0.0.1:6001" should respond with "FOO"
