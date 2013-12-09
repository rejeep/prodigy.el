Feature: Refresh

  Scenario: Save line
    Given I add the following processes:
      | name |
      | Foo  |
      | Bar  |
    And I start prodigy
    And I press "n"
    And I add the following processes:
      | name |
      | Baz  |
    When I press "g"
    Then I should see the following processes:
      | name |
      | Bar  |
      | Baz  |
      | Foo  |
    And the point should be on line "2"
