Feature: No of Days in Month
	In order to keep track of dates
	As a Calendar
	I want to be told how many days are present in a month

Scenario: No of Days present in January
	Given I have an year
	And I have January month in that year
	Then the day count in the month should be 31

Scenario: No of Days present in February in Leap year
	Given I have a leap year
	And I have February month in that year
	Then the day count in the month should be 29

Scenario: No of Days present in February in non-Leap year
	Given I have a non-leap year
	And I have February month in that year
	Then the day count in the month should be 28

Scenario: No of Days present in March
	Given I have an year
	And I have March month in that year
	Then the day count in the month should be 31

Scenario: No of Days present in April
	Given I have an year
	And I have April month in that year
	Then the day count in the month should be 30

Scenario: No of Days present in May
	Given I have an year
	And I have May month in that year
	Then the day count in the month should be 31

Scenario: No of Days present in June
	Given I have an year
	And I have April month in that year
	Then the day count in the month should be 30

Scenario: No of Days present in July
	Given I have an year
	And I have July month in that year
	Then the day count in the month should be 31

Scenario: No of Days present in August
	Given I have an year
	And I have August month in that year
	Then the day count in the month should be 31

Scenario: No of Days present in September
	Given I have an year
	And I have April month in that year
	Then the day count in the month should be 30

Scenario: No of Days present in October
	Given I have an year
	And I have October month in that year
	Then the day count in the month should be 31

Scenario: No of Days present in November
	Given I have an year
	And I have April month in that year
	Then the day count in the month should be 30

Scenario: No of Days present in December
	Given I have an year
	And I have December month in that year
	Then the day count in the month should be 31