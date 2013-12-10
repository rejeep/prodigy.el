Feature: Refresh

  Scenario: Save line
    Given I add the following processes:
      | name |
      | foo  |
      | bar  |
    And I start prodigy
    And I press "n"
    And I add the following processes:
      | name |
      | baz  |
    When I press "g"
    Then I should see the following processes:
      | name |
      | bar  |
      | baz  |
      | foo  |
    And I should be on service line "2"
