using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CustomDate;
using NUnit.Framework;

namespace CustomDateTests.UnitTests
{
    [NUnit.Framework.TestFixtureAttribute()]
    [NUnit.Framework.DescriptionAttribute("Year Unit Tests")]
    class MonthTests
    {
        [Test]
        public virtual void TestMonthInstantiation()
        {
            var year = new Year(2001);
            var month = new Month(MonthType.January, year);
            Assert.AreEqual(MonthType.January, month.MonthType);
            Assert.AreEqual(year, month.Year);
        }
    }
}