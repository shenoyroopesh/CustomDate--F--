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
    class YearTests
    {
        [Test]
        public virtual void TestYearDaysCount()
        {
            //leap years
            var year = new Year(2000);
            Assert.AreEqual(year.DaysCount, 366);

            year = new Year(1968);
            Assert.AreEqual(year.DaysCount, 366);

            year = new Year(2012);
            Assert.AreEqual(year.DaysCount, 366);

            //non-leap years

            year = new Year(2001);
            Assert.AreEqual(year.DaysCount, 365);

            year = new Year(1900);
            Assert.AreEqual(year.DaysCount, 365);

            year = new Year(2100);
            Assert.AreEqual(year.DaysCount, 365);
        }
    }
}