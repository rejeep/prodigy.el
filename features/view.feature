Feature: View

  Background:
    Given I add the following services:
      | name   | cwd    | command | path       | env               |
      | append | append | server  | ("append") | (("PORT" "6001")) |
    And I start prodigy

  Scenario: Not started
    When I press "$"
    Then I should see message "Nothing to show for append"

  Scenario: View mode
    When I start service
    Then requesting "http://127.0.0.1:6001" should respond with "APPEND"
    When I press "$"
    Then I should be in buffer "*prodigy-append*"
    And I should see:
      """
      Starting append...
      """
    And view mode should be enabled

  Scenario: Appends output
    When I start service
    And I request "http://127.0.0.1:6001"
    And I request "http://127.0.0.1:6001"
    And I request "http://127.0.0.1:6001"
    And I press "$"
    Then I should see:
      """
      Line 1
      Line 2
      Line 3
      """
    When I go to line "2"
    And I request "http://127.0.0.1:6001"
    Then I should see:
      """
      Line 1
      Line 2
      Line 3
      Line 4
      """
    And I should be on line "2"
