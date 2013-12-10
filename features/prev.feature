Feature: Prev

  Scenario: No processes
    Given I start prodigy
    When I press "p"
    Then I should see message "Cannot move further up"

  Scenario: Single process
    Given I add the following processes:
      | name |
      | foo  |
    Given I start prodigy
    Then I should see the following processes:
      | name |
      | foo  |
    And I should be on service line "1"
    When I press "p"
    Then I should see message "Cannot move further up"

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
    And I should be on service line "1"
    When I press "n"
    Then I should be on service line "2"
    When I press "p"
    Then I should be on service line "1"
    When I press "p"
    Then I should see message "Cannot move further up"
