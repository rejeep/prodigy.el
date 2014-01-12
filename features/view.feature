Feature: View

  Scenario: Prodigy mode
    Given I start prodigy view mode
    Then I should be in prodigy view mode

  Scenario: Font lock
    Given I start prodigy view mode
    Then font lock mode should be enabled
