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
    public class YearHas12MonthsSteps
    {
        [Given(@"I have an year")]
        public void GivenIHaveAnYear()
        {
            var year = new Year(2000); //pick a random year
            ScenarioContext.Current.Set(year);
        }

        [Then(@"The year should have 12 months")]
        public void ThenTheYearShouldHave12Months()
        {
            var year = ScenarioContext.Current.Get<Year>();
            Assert.AreEqual(year.Months.Count(), 12, "Year should have 12 months");
            Assert.AreEqual(Enum.GetValues(typeof(MonthType)).Length, 12, "There should be 12 month types");

            foreach (MonthType monthType in Enum.GetValues(typeof(MonthType)))
            {
                Assert.AreEqual(year.Months.Where(p => p.MonthType == monthType).Count(), 1);
            }

            foreach (var month in year.Months)
            {
                month.MonthType.Should().Be.OfType(typeof(MonthType));
                month.MonthType.Should().Not.Be.Null();
            }
        }
    }
}