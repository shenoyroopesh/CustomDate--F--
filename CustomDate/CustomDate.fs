namespace CustomDate
module CustomDate =
    open System.Linq
    open System

    type public Month = 
        | January = 1 | February = 2 | March = 3 | April = 4 | May = 5 
        | June = 6 | July = 7 | August = 8 | September = 9 | October = 10 
        | November = 11 | December = 12

    type public Date = { Y : int; M : Month; D : int }

    let monthsWith31Days = 
        [  Month.January; Month.March; Month.May; Month.July; 
           Month.August; Month.October; Month.December ]

    let allMonths = (Enum.GetValues typeof<Month>).Cast<Month>()

    let leap yr = yr % 4 = 0 && (not (yr % 100 = 0) || yr % 400 = 0)

    let dayCountYr year = if leap year then 366 else 365

    let dayCount year month = 
        match month with
           | Month.February -> if leap year then 29 else 28
           | _ -> if monthsWith31Days.Contains month then 31 else 30

    let daysRemInYear date = (allMonths |> Seq.filter(fun p -> p >= date.M)
                             |> Seq.map(dayCount date.Y) |> Seq.sum) - date.D

    let daysOverInYear date = dayCountYr date.Y - daysRemInYear date + 1

    let daysBetween y1 y2 = [y1 + 1 .. y2 - 1] |> List.map (dayCountYr) |> List.sum

    let toInt dt = sprintf "%i%i%i" dt.Y ((int)dt.M) dt.D |> Int32.Parse

    let rec public dateDiff d1 d2 = 
        if toInt d1 > toInt d2 then dateDiff d2 d1 
        else if d2.Y > d1.Y then 
           daysRemInYear d1 + daysBetween d1.Y d2.Y + daysOverInYear d2 
        else if d2.M > d1.M then 
           daysRemInYear d1 - daysRemInYear d2 + 1
        else d2.D - d1.D + 1

    let toDate(date:String) =
        try
            let splitted = date.Split('-') |> Array.map Int32.Parse
            let y = splitted.[2]
            let m = enum<Month> splitted.[1]
            let d =    
               if splitted.[0] > dayCount y m then raise(System.ArgumentException())
               else splitted.[0]
            {D = d; M = m; Y = y}
        with
            | ex -> raise(System.FormatException())