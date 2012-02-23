namespace CustomDate

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

        member public this.Year 
            with get () = year
        member public this.MonthType 
            with get () = monthType

        member public this.GetDaysCount () = 
            let monthsWith31Days = [  MonthType.January; 
                                      MonthType.March;
                                      MonthType.May;
                                      MonthType.July;
                                      MonthType.August;
                                      MonthType.October;
                                      MonthType.December ]

            let has31 = monthsWith31Days |> List.filter(fun n -> (int)n = (int)this.MonthType) |> List.length

            if (has31 > 0)
            then 31
            else if (this.MonthType = MonthType.February)
            then (if this.Year.Leap then 29 
                  else 28)
            else 30


    and public Year(ad:int) = 
        member public this.AD
            with get() = ad

        member public this.Months = Enum.GetValues(typeof<MonthType>).Cast().ToArray()
                                    |> Array.map(fun n -> new Month (n, this))

        member public this.GetMonth (index:int) = 
            this.Months |> Array.filter(fun p-> (int)p.MonthType = index) |> Array.find(fun p -> true) //just get the first element

        member public this.GetMonth (monthName:string) = 
            let requiredMonthType = Enum.Parse(typeof<MonthType>, monthName) |> unbox<MonthType>
            this.Months |> Array.filter(fun p-> p.MonthType = requiredMonthType) |> Array.find(fun p -> true) //first element

        member public this.Leap = 
            if this.AD % 400 = 0 then true
            else if this.AD % 100 = 0 then false
            else if this.AD % 4 = 0 then true
            else false

        member this.DaysCount = if this.Leap then 366 else 365

    type public Date(date:String) =
        
        let splitted = date.Split('-');
        
        let year = 
            try
                new Year(Int32.Parse(splitted.[2]))
            with
                | ex -> raise(System.ArgumentException("Input not formatted correctly"))

        let month = 
            try
                let month:int = Int32.Parse(splitted.[1])
                if month > 12 then raise(System.ArgumentException())
                else
                    new Month(enum<MonthType> (month), year)
            with
                | ex -> raise(System.ArgumentException("Input not formatted correctly"))

        let day = 
            try
                Int32.Parse(splitted.[0])
            with
                | ex -> raise(System.ArgumentException("Input not formatted correctly"))

        member this.Year 
            with get() = year

        member this.Month
            with get() = month

        member this.Day
            with get() = day

        member public this.GetDaysRemainingInMonth () = this.Month.GetDaysCount() - this.Day + 1    

        member public this.GetDaysRemainingInYear () = this.GetDaysRemainingInMonth() + 
                                                       (this.Year.Months 
                                                       |> Array.filter(fun p -> (int)p.MonthType > (int) this.Month.MonthType)
                                                       |> Array.map(fun p-> p.GetDaysCount())
                                                       |> Array.sum)

        member public this.GetDaysOverInYear () = this.Year.DaysCount - this.GetDaysRemainingInYear() + 1;

        member this.GetDiffInternal (secondDate:Date) = 
            if secondDate.Year.AD <> this.Year.AD  
            then this.GetDaysRemainingInYear() +
                 ([this.Year.AD + 1 .. secondDate.Year.AD - 1] |> List.map(fun p -> (new Year(p)).DaysCount) |> List.sum ) +
                 secondDate.GetDaysOverInYear() 
            else (if (int)secondDate.Month.MonthType <> (int)this.Month.MonthType 
                  then this.GetDaysRemainingInYear() - 
                       secondDate.GetDaysRemainingInYear() + 
                       1 // to include both starting and ending dates
                  else secondDate.Day - this.Day + 1)
        
        member public this.GetDiff (secondDate:Date) = 
            if this.Year.AD > secondDate.Year.AD ||
               (this.Year.AD = secondDate.Year.AD && (int)this.Month.MonthType > (int)secondDate.Month.MonthType) ||
               (this.Year.AD = secondDate.Year.AD && (int)this.Month.MonthType = (int)secondDate.Month.MonthType && this.Day > secondDate.Day)
            then secondDate.GetDiffInternal(this) //reverse the dates
            else this.GetDiffInternal(secondDate)