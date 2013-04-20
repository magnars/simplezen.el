Feature: Expanding tags

  Scenario: don't fail on blank
    When I press "TAB"

  Scenario: tagname
    Given I type "div"
    And I press "TAB"
    Then I should see "<div></div>"

  Scenario: tagname, part 2
    Given I type "h3"
    And I press "TAB"
    Then I should see "<h3></h3>"

  Scenario: keeps other stuff on the line
    Given I insert "abc div def"
    When I go to the end of the word "div"
    And I press "TAB"
    Then I should see "abc <div></div> def"

  Scenario: completes from end of tag
    Given I insert "<div>span"
    And I press "TAB"
    Then I should see "<div><span></span>"

  Scenario: only completes valid html tags
    Given I type "abc"
    And I press "TAB"
    Then I should not see "<abc"

  Scenario: does not move point needlessly
    Given I type "abc"
    And I press "TAB"
    And I type "def"
    Then I should see "abcdef"

  Scenario: completes class names
    Given I type ".abc"
    And I press "TAB"
    Then I should see "<div class="abc"></div>"

  Scenario: completes ids
    Given I type "#abc"
    And I press "TAB"
    Then I should see "<div id="abc"></div>"

  Scenario: completes combinations names
    Given I type "ul#hmm.abc.def"
    And I press "TAB"
    Then I should see "<ul id="hmm" class="abc def"></ul>"

  Scenario: leaves self-closing tags closed
    Given I type "input.required"
    And I press "TAB"
    Then I should see "<input class="required">"
    And I should not see "</input>"

  Scenario: places point inside tags
    Given I type "h3"
    And I press "TAB"
    And I type "title"
    Then I should see "<h3>title</h3>"
