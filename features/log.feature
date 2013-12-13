Feature: Log

  Background:
    Given I add the following services:
      | name | cwd | command | args                             |
      | foo  | foo | python  | ("-m" "SimpleHTTPServer" "6001") |
    And I start prodigy

  Scenario: Not started
    When I press "l"
    Then I should see services:
      | name | highlighted |
      | foo  | t           |

  Scenario: Started
    When I press "s"
    And I press "l"
    Then I should be in buffer "*prodigy-foo*"
