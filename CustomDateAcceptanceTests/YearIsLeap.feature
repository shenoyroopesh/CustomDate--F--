Feature: Year is leap
	In order to get a handle on dates
	As a Calendar
	I want to be told if a given year is a leap year

Scenario: Year divisible by 4 is a leap year
	Given I have an year 1996
	Then the year should be a leap year

Scenario: Year divisible by 4 is a leap year (2)
	Given I have an year 2012
	Then the year should be a leap year

Scenario: Year divisible by 4 but divisible by 100 and not divisible by 400 is not a leap year
	Given I have an year 1900
	Then the year should not be a leap year

Scenario: Year divisible by 400 is a leap year
	Given I have an year 2000
	Then the year should be a leap year

Scenario: Year divisible by 400 is a leap year (2)
	Given I have an year 1600
	Then the year should be a leap year
