﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using TechTalk.SpecFlow;
using CustomDate;
using NUnit.Framework;
using System.Collections;

namespace CustomDateTests
{
    [Binding]
    public class DateDiffFeatureSteps
    {
        [Given(@"I have a date (.*)")]
        public void GivenIHaveADate(string firstDate)
        {
            CustomDate.CustomDate.Date date = CustomDate.CustomDate.toDate(firstDate);
            ScenarioContext.Current.Set(date);
        }

        [Then(@"the difference between it and another date (.*) should be (.*)")]
        public void ThenTheDifferenceBetweenItAndAnotherDateShouldBe(string secondDate, int diff)
        {
            var date = ScenarioContext.Current.Get<CustomDate.CustomDate.Date>();
            Assert.AreEqual(diff, CustomDate.CustomDate.dateDiff(date, CustomDate.CustomDate.toDate(secondDate)));
        }
    }
}