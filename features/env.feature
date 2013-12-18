Feature: Env

  Scenario: Add env
    Given I add the following services:
      | name | cwd | command | env               | path    |
      | qux  | qux | server  | (("PORT" "6004")) | ("qux") |
    And I start prodigy
    When I press "s"
    Then requesting "http://127.0.0.1:6004/index.html" should respond with:
      """
      <!DOCTYPE>
      <html>
        <head></head>
        <body>
          QUX
        </body>
      </html>
      """
