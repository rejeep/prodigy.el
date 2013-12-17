Feature: View

  Background:
    Given I add the following services:
      | name | cwd | command | args                             |
      | foo  | foo | python  | ("-m" "SimpleHTTPServer" "6001") |
    And I start prodigy

  Scenario: Not started
    When I press "$"
    Then I should be in prodigy mode
    And I should see message "Nothing to show for foo"

  Scenario: View mode
    And I press "s"
    Then requesting "http://127.0.0.1:6001/index.html" should respond with:
      """
      <!DOCTYPE>
      <html>
        <head></head>
        <body>
          FOO
        </body>
      </html>
      """
    And I press "$"
    Then I should be in buffer "*prodigy-foo*"
    And I should see:
      """
      Serving HTTP on 0.0.0.0 port 6001
      """
    And I should see:
      """
      GET /index.html HTTP/1.1
      """
    And view mode should be enabled
