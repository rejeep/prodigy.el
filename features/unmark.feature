Feature: Unmark

  Scenario: No service
    Given I start prodigy
    When I press "u"
    Then the buffer should be empty

  Scenario: Single service
    Given I add the following services:
      | name |
      | foo  |
    And I start prodigy
    When I press "m"
    And I press "u"
    Then I should be on service line "1"
    And service at line "1" should not be marked

  Scenario: Multiple services
    Given I add the following services:
      | name |
      | foo  |
      | bar  |
    And I start prodigy
    When I press "m"
    When I press "p"
    And I press "u"
    Then I should be on service line "2"
    And service at line "1" should not be marked
    And service at line "2" should not be marked
    When I press "n"
    And I press "m"
    And I press "u"
    Then I should be on service line "2"
    And service at line "1" should not be marked
    And service at line "2" should not be marked

  Scenario: Already marked
    Given I add the following services:
      | name |
      | foo  |
    And I start prodigy
    When I press "u"
    And I press "u"
    Then I should be on service line "1"
    And service at line "1" should not be marked
