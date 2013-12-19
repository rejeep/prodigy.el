Feature: View

  Background:
    Given I add the following services:
      | name   | cwd    | command | path       | env               |
      | append | append | server  | ("append") | (("PORT" "6001")) |
    And I start prodigy

  # Scenario: Not started
  #   When I press "$"
  #   Then I should see message "Nothing to show for foo"

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
    And I press "$"
      """
      """
      """
      """
