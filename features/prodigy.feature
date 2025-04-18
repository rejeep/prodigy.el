Feature: Prodigy

  Scenario: Prodigy mode
    Given I start prodigy
    Then I should be in prodigy mode

  Scenario: Prodigy buffer
    Given I start prodigy
    Then I should be in buffer "*prodigy*"
    And the buffer should be read only

  Scenario: Mode hook
    Given I load the following:
      """
      (add-hook 'prodigy-mode-hook
          (lambda () (setq foo "bar")))
      """
    Then the variable "foo" should be undefined
    When I start prodigy
    Then the variable "foo" should have value "bar"

  Scenario: Sorted by name
    Given I add the following services:
      | name |
      | baz  |
      | foo  |
      | qux  |
      | bar  |
    Given I start prodigy
    Then I should see services:
      | name | highlighted |
      | bar  | t           |
      | baz  | nil         |
      | foo  | nil         |
      | qux  | nil         |

  Scenario: Prodigy buffer respects display-buffer-alist
    Given I start prodigy
    Then The current frame has "2" window/s
    When I kill the prodigy buffer
    Then I delete the other windows
    Given I add the display-buffer-same-window rule to display-buffer-alist
    Then I start prodigy
    Then The current frame has "1" window/s
