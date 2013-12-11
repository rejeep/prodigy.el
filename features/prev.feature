Feature: Prev

  Scenario: No services
    Given I start prodigy
    When I press "p"
    Then I should see message "Cannot move further up"

  Scenario: Single service
    Given I add the following services:
      | name |
      | foo  |
    Given I start prodigy
    Then I should see the following services:
      | name |
      | foo  |
    And I should be on service line "1"
    When I press "p"
    Then I should see message "Cannot move further up"

  Scenario: Multiple services
    Given I add the following services:
      | name |
      | foo  |
      | bar  |
    Given I start prodigy
    Then I should see the following services:
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
