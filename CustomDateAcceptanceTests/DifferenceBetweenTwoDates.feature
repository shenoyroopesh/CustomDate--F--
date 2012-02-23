Feature: Date diff feature
	In order to count days
	As a Calendar
	I want to be told the number of days between two dates

Scenario: Difference between Two dates in same month
	Given I have a date 21-01-2001
	Then the difference between it and another date 25-01-2001 should be 5

Scenario: Difference between Two dates in same month (2)
	Given I have a date 21-01-2001
	Then the difference between it and another date 26-01-2001 should be 6

Scenario: Difference between Two dates in same month in Reverse order
	Given I have a date 26-01-2001
	Then the difference between it and another date 21-01-2001 should be 6

Scenario: Difference between Two dates in same year but different months
	Given I have a date 21-01-2001
	Then the difference between it and another date 25-02-2001 should be 36

Scenario: Difference between Two dates in same year but different months (2)
	Given I have a date 21-01-2001
	Then the difference between it and another date 25-03-2001 should be 64

Scenario: Difference between Two dates in same year but different months (3)
	Given I have a date 21-01-2000
	Then the difference between it and another date 25-03-2000 should be 65

Scenario: Difference between Two dates in same year but different months in reverse order
	Given I have a date 25-03-2000
	Then the difference between it and another date 21-01-2000 should be 65

Scenario: Difference between Two dates in different years	
	Given I have a date 21-01-2000
	Then the difference between it and another date 25-03-2001 should be 430

Scenario: Difference between Two dates in different years (2)
	Given I have a date 21-01-2001
	Then the difference between it and another date 25-03-2002 should be 429

Scenario: Difference between Two dates in different years (3)
	Given I have a date 29-01-2001
	Then the difference between it and another date 30-03-2011 should be 3713

Scenario: Difference between Two dates in different years in reverse order
	Given I have a date 25-03-2001
	Then the difference between it and another date 21-01-2000 should be 430