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
    Then I should see the following services:
      | name |
      | bar  |
      | baz  |
      | foo  |
    And I should be on service line "2"
