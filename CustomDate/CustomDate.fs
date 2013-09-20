namespace CustomDate
module CustomDate =
    open System.Collections.Generic
    open System.Linq
    open System

    type public Month = 
        | January = 1 | February = 2 | March = 3 | April = 4 | May = 5 
        | June = 6 | July = 7 | August = 8 | September = 9 | October = 10 
        | November = 11 | December = 12

    type public Year = { AD : int; Leap : Boolean; dayCount : int }

    type public Date = { Year : Year; Month : Month; Day : int }

    let monthsWith31Days = [  Month.January; 
                              Month.March;
                              Month.May;
                              Month.July;
                              Month.August;
                              Month.October;
                              Month.December ]

    let allMonths = Enum.GetValues(typeof<Month>).Cast<Month>();

    let toYear ad =
        let leap = ad % 4 = 0 && (not (ad % 100 = 0) || ad % 400 = 0)
        let dayCount = if leap then 366 else 365
        { AD = ad; Leap = leap; dayCount = dayCount }

    let dayCount year month = 
        if monthsWith31Days.Contains month then 31
        else if month = Month.February then if year.Leap then 29 else 28
        else 30

    let daysRemainingInMonth date = dayCount date.Year date.Month - date.Day + 1    

    let daysRemainingInYear date = daysRemainingInMonth date + 
                                 (allMonths |> Seq.filter((<) date.Month)
                                 |> Seq.map(dayCount date.Year) |> Seq.sum)

    let daysOverInYear date = date.Year.dayCount - daysRemainingInYear date + 1

    let daysBetween y1 y2 = [y1.AD + 1 .. y2.AD - 1] 
                            |> List.map (fun p -> (toYear p).dayCount)
                            |> List.sum 

    let diffInternal d1 d2 = 
        if d2.Year.AD <> d1.Year.AD then 
           daysRemainingInYear d1 + daysBetween d1.Year d2.Year + daysOverInYear d2 
        else if d2.Month <> d1.Month then 
           daysRemainingInYear d1 - daysRemainingInYear d2 + 1
        else d2.Day - d1.Day + 1
        
    let toInt dt = sprintf "%i%i%i" dt.Year.AD ((int)dt.Month) dt.Day |> Int32.Parse

    let public dateDiff d1 d2 = 
        if toInt d1 > toInt d2 then diffInternal d2 d1 else diffInternal d1 d2

    let toDate(date:String) =
        let splitted = date.Split('-') |> Array.map Int32.Parse
        let year = toYear splitted.[2]
        let month = enum<Month> splitted.[1]
        let day =    
           if splitted.[0] > dayCount year month then raise(System.FormatException())
           else splitted.[0]
        {Day = day; Month = month; Year = year}