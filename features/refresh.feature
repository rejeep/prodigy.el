Feature: Refresh

  Scenario: Save line
    Given I add the following services:
      | name |
      | foo  |
      | bar  |
    And I start prodigy
    And I press "n"
    And I add the following services:
      | name |
      | baz  |
    When I press "g"
    Then I should see services:
      | name | highlighted |
      | bar  | nil         |
      | baz  | t           |
      | foo  | nil         |
