using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using TechTalk.SpecFlow;
using CustomDate;
using Should.Fluent;
using NUnit.Framework;
using System.Collections;

namespace CustomDateTests
{
    [Binding]
    public class MonthHasDaysCountSteps
    {
        [Given(@"I have (.*) month in that year")]
        public void GivenIHaveMonthInThatYear(string monthName)
        {
            var year = ScenarioContext.Current.Get<Year>();
            var month = year.GetMonth(monthName);
            //confirm that things are fine
            ScenarioContext.Current.Set(month);
        }

        [Then(@"the day count in the month should be (.*)")]
        public void ThenTheDayCountInTheMonthShouldBe(int num)
        {
            var month = ScenarioContext.Current.Get<Month>();
            Assert.AreEqual(num, month.GetDaysCount());
        }

        [Given(@"I have a leap year")]
        public void GivenIHaveALeapYear()
        {
            var year = new Year(2000); //pick a random leap year
            ScenarioContext.Current.Set(year);
        }

        [Given(@"I have a non-leap year")]
        public void GivenIHaveANonLeapYear()
        {
            var year = new Year(2001);
            ScenarioContext.Current.Set(year);
        }
    }
}