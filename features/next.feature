Feature: Next

  Scenario: No processes
    Given I start prodigy
    When I press "n"
    Then the point should be on line "1"
    When I press "n"
    Then the point should be on line "1"

  Scenario: Single process
    Given I add the following processes:
      | name |
      | foo  |
    Given I start prodigy
    Then I should see the following processes:
      | name |
      | foo  |
    And the point should be on line "1"
    When I press "n"
    Then the point should be on line "1"
    When I press "n"
    Then the point should be on line "1"

  Scenario: Multiple processes
    Given I add the following processes:
      | name |
      | foo  |
      | bar  |
    Given I start prodigy
    Then I should see the following processes:
      | name |
      | bar  |
      | foo  |
    And the point should be on line "1"
    When I press "n"
    Then the point should be on line "2"
    When I press "n"
    Then the point should be on line "2"
