Feature: Jump

  Background:
    Given I add the following services:
      | name | cwd |
      | foo  | foo |
    And I start prodigy

  Scenario: Magit
    When I press "j m"
    Then I should be in magit status mode

  Scenario: Dired
    When I press "j d"
    Then I should be in dired mode
