Feature: Quit

  Scenario: Quit Prodigy mode
    Given I start prodigy
    Then I should be in prodigy mode
    When I press "q"
    Then I should not be in prodigy mode
