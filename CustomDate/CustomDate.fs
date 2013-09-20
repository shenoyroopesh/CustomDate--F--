namespace CustomDate
module CustomDate =
    open System.Collections.Generic
    open System.Linq
    open System

    type public MonthType = 
        | January = 1
        | February = 2
        | March = 3
        | April = 4
        | May = 5
        | June = 6
        | July = 7
        | August = 8
        | September = 9
        | October = 10
        | November = 11
        | December = 12

    type public Month(monthType:MonthType, year:Year) = 
        member this.Year = year
        member this.MonthType = monthType

    and public Year(ad:int) =
        member this.AD = ad
        member public this.Months = Enum.GetValues(typeof<MonthType>).Cast().ToArray()
                                    |> Array.map(fun n -> new Month (n, this))
        member public this.Leap = 
            if this.AD % 400 = 0 then true
            else if this.AD % 100 = 0 then false
            else if this.AD % 4 = 0 then true
            else false
        member this.DaysCount = if this.Leap then 366 else 365

    let monthsWith31Days = [  MonthType.January; 
                              MonthType.March;
                              MonthType.May;
                              MonthType.July;
                              MonthType.August;
                              MonthType.October;
                              MonthType.December ]

    let GetDaysCount (month:Month) = 
        if (monthsWith31Days.Contains(month.MonthType)) then 31
        else if (month.MonthType = MonthType.February)
            then (if month.Year.Leap then 29 else 28)
        else 30

    type public Date (day:int, month:Month, year: Year) =
        member this.Year = year
        member this.Month = month
        member this.Day = day

    let GetDaysRemainingInMonth (date:Date) = GetDaysCount date.Month - date.Day + 1    

    let GetDaysRemainingInYear (date:Date) = GetDaysRemainingInMonth(date) + 
                                                       (date.Year.Months 
                                                       |> Array.filter(fun p -> (int)p.MonthType > (int) date.Month.MonthType)
                                                       |> Array.map(GetDaysCount)
                                                       |> Array.sum)

    let GetDaysOverInYear (date:Date) = date.Year.DaysCount - GetDaysRemainingInYear(date) + 1

    let GetDaysBetweenTwoYears(year1:Year, year2:Year) = [year1.AD .. year2.AD - 1] |>  List.map(fun p -> (new Year(p)).DaysCount) |> List.sum 

    let GetDiffInternal (firstDate:Date, secondDate:Date) = 
            if secondDate.Year.AD <> firstDate.Year.AD  
            then GetDaysRemainingInYear(firstDate) + GetDaysBetweenTwoYears(firstDate.Year, secondDate.Year) + GetDaysOverInYear(secondDate) 
            else (if (int)secondDate.Month.MonthType <> (int)firstDate.Month.MonthType 
                  then GetDaysRemainingInYear(firstDate) - 
                       GetDaysRemainingInYear(secondDate) + 1 // to include both starting and ending dates
                  else secondDate.Day - firstDate.Day + 1)
        
    let public GetDiff (firstDate:Date, secondDate:Date) = 
            if firstDate.Year.AD > secondDate.Year.AD ||
               (firstDate.Year.AD = secondDate.Year.AD && (int)firstDate.Month.MonthType > (int)secondDate.Month.MonthType) ||
               (firstDate.Year.AD = secondDate.Year.AD && (int)firstDate.Month.MonthType = (int)secondDate.Month.MonthType && firstDate.Day > secondDate.Day)
            then GetDiffInternal(secondDate, firstDate) //reverse the dates
            else GetDiffInternal(firstDate, secondDate)

    let fromStringToDate(date:String) =
       try
          let splitted = date.Split('-');
          let year = new Year(Int32.Parse(splitted.[2]))
          let month =
             let monthInt = Int32.Parse(splitted.[1])
             if monthInt > 12 then raise(System.ArgumentException())
             else new Month(enum<MonthType> (monthInt), year)
          let day = 
             let dayInt = Int32.Parse(splitted.[0])
             if dayInt > GetDaysCount month then raise(System.ArgumentException())
             else dayInt
          new Date(day, month, year)
       with
          | ex -> raise(System.ArgumentException("Input not formatted correctly"))