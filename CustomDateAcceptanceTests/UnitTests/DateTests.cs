using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CustomDate;
using NUnit.Framework;

namespace CustomDateTests.UnitTests
{
    [NUnit.Framework.TestFixtureAttribute()]
    [NUnit.Framework.DescriptionAttribute("Date Unit Tests")]
    class DateTests
    {
        [Test]
        public virtual void TestDateStructure()
        {
            var date = new Date("21-01-2001");

            Year year = date.Year;
            Month month = date.Month;

            Assert.AreEqual(2001, year.AD, "Date should have an year 2001");
            Assert.AreEqual(MonthType.January, date.Month.MonthType, "Date should have month January");
            Assert.AreEqual(year.AD, date.Month.Year.AD, "Month in the date should belong to same year");
            Assert.AreEqual(21, date.Day);
        }

        [Test]
        public virtual void TestGetDaysRemainingInMonth()
        {
            var date = new Date("21-01-2001");
            Assert.AreEqual(date.GetDaysRemainingInMonth(), 11);

            date = new Date("21-02-2001");
            Assert.AreEqual(date.GetDaysRemainingInMonth(), 8);

            date = new Date("21-02-2000");
            Assert.AreEqual(date.GetDaysRemainingInMonth(), 9);

            date = new Date("21-06-2000");
            Assert.AreEqual(date.GetDaysRemainingInMonth(), 10);
        }

        [Test]
        public virtual void TestGetDaysRemainingInYear()
        {
            var date = new Date("21-01-2001");
            Assert.AreEqual(date.GetDaysRemainingInYear(), 345);

            date = new Date("21-02-2001");
            Assert.AreEqual(date.GetDaysRemainingInYear(), 314);

            date = new Date("21-02-2000");
            Assert.AreEqual(date.GetDaysRemainingInYear(), 315);

            date = new Date("21-06-2000");
            Assert.AreEqual(date.GetDaysRemainingInYear(), 194);
        }

        [Test]
        public virtual void TestInvalidDateFormatCtr()
        {
            Assert.Catch<ArgumentException>(() => { var date = new Date(""); });
            Assert.Catch<ArgumentException>(() => { var date = new Date("Test"); });
            Assert.Catch<ArgumentException>(() => { var date = new Date("21-13-2001"); });
        }
    }
}