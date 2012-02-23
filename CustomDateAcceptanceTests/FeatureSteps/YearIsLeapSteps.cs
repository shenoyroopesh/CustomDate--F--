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
    public class YearIsLeapSteps
    {
        [Given(@"I have an year (.*)")]
        public void GivenIHaveAnYearAD(int AD)
        {
            var year = new Year(AD);
            ScenarioContext.Current.Set(year);
        }

        [Then(@"the year should be a leap year")]
        public void ThenTheYearShouldBeALeapYear()
        {
            var year = ScenarioContext.Current.Get<Year>();
            year.Leap.Should().Be.True();
        }

        [Then(@"the year should not be a leap year")]
        public void ThenTheYearShouldNotBeALeapYear()
        {
            var year = ScenarioContext.Current.Get<Year>();
            year.Leap.Should().Be.False();
        }
    }
}