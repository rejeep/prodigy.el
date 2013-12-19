Feature: Init

  Scenario: Run init function before starting process
    Given I add the following services:
      | name | cwd | command | path    | env               | init                         |
      | foo  | foo | server  | ("foo") | (("PORT" "6001")) | (lambda () (setq foo "BAR")) |
    And I start prodigy
    When I start service
    Then requesting "http://127.0.0.1:6001" should respond with "FOO"
    And the variable "foo" should have value "BAR"

  Scenario: Async
    Given I add the following services:
      | name | cwd | command | path    | env               | init-async                                                    |
      | foo  | foo | server  | ("foo") | (("PORT" "6001")) | (lambda (done) (setq foo "BAR") (sleep-for 2) (funcall done)) |
    And I start prodigy
    When I start service
    Then requesting "http://127.0.0.1:6001" should respond with "FOO"
    And the variable "foo" should have value "BAR"
