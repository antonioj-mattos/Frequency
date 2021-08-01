module Frequency.TimeSheet

open NodaTime
open FsToolkit.ErrorHandling

/// Break of at least 15 minutes
type BreakFrom15Minutes = private BreakFrom15Minutes of Interval

/// Break of at least 15 minutes and maximum of 3 hours
type BreakFrom15To180Minutes = private BreakFrom15To180Minutes of Interval

/// Break of at least 1 hour
type BreakFrom60Minutes = private BreakFrom60Minutes of Interval

/// Break of at least 1 hour and maximum of 3 hours
type BreakFrom60To180Minutes = private BreakFrom60To180Minutes of Interval

/// Schedule from 2 hours up to 7 hours
type ScheduleUntil7Hours = private ScheduleUntil7Hours of Interval

/// Schedule from 7 hours up to 8 hours
type ScheduleUntil8Hours = private ScheduleUntil8Hours of Interval


[<RequireQualifiedAccess>]
module ConstrainedType =

    /// <sumary>
    /// Tries to create a break of at least the minimum duration and
    /// optionally up to the maximum duration.
    /// </sumary>
    let createBreak fieldName ctor minDuration (maxDuration: Duration option) (interval: Interval) =

        let reachesMinDuration = interval.Duration >= minDuration

        let exceedsMaxDuration =
            maxDuration
            |> Option.map (fun maxDuration -> interval.Duration > maxDuration)
            |> Option.defaultValue false

        if not reachesMinDuration then
            let msg =
                sprintf "%s must be greater than or equal to %s" fieldName (minDuration.ToString())

            Error msg
        elif exceedsMaxDuration then
            let msg =
                sprintf "%s must not be greater than %s" fieldName (maxDuration.ToString())

            Error msg
        else
            Ok(ctor interval)

    /// <sumary>
    /// Tries to create a schedule of at least the minimum duration and
    /// up to the maximum duration, optionally containing a break.
    ///
    /// If provided, the break must start at least 30 minutes after the
    /// schedule starts and it must end at least 30 minutes before the
    /// schedule ends.
    /// </sumary>
    let createSchedule fieldName ctor minDuration maxDuration (breakInterval: Interval option) (interval: Interval) =

        let duration =
            let breakDuration =
                breakInterval
                |> Option.map (fun breakInterval -> breakInterval.Duration)
                |> Option.defaultValue Duration.Zero

            interval.Duration - breakDuration

        let reachesMinDuration = duration >= minDuration
        let exceedsMaxDuration = duration > maxDuration

        let containedBreak =
            let breakOffSet = Duration.FromMinutes(30L)

            breakInterval
            |> Option.map
                (fun breakInterval ->
                    interval.Contains(breakInterval.Start.Minus(breakOffSet))
                    && interval.Contains(breakInterval.End.Plus(breakOffSet)))
            |> Option.defaultValue true

        if not reachesMinDuration then
            let msg =
                sprintf "%s must be greater than %s" fieldName (minDuration.ToString())

            Error msg
        elif exceedsMaxDuration then
            let msg =
                sprintf "%s must not be greater than %s" fieldName (maxDuration.ToString())

            Error msg
        elif not containedBreak then
            let msg =
                sprintf "%s break must be contained within the schedule" fieldName

            Error msg

        else
            Ok(ctor interval)


[<RequireQualifiedAccess>]
module BreakFrom15Minutes =

    let value (BreakFrom15Minutes interval) = interval

    /// <sumary>
    /// Tries to create a break of at least 15 minutes.
    /// </sumary>
    let create fieldName interval =
        ConstrainedType.createBreak fieldName BreakFrom15Minutes (Duration.FromMinutes(15L)) None interval


[<RequireQualifiedAccess>]
module BreakFrom15To180Minutes =

    let value (BreakFrom15To180Minutes interval) = interval

    /// <sumary>
    /// Tries to create a break of at least 15 minutes and maximum of 3 hours.
    /// </sumary>
    let create fieldName interval =
        ConstrainedType.createBreak
            fieldName
            BreakFrom15To180Minutes
            (Duration.FromMinutes(15L))
            (Duration.FromMinutes(180L) |> Some)
            interval


[<RequireQualifiedAccess>]
module BreakFrom60Minutes =

    let value (BreakFrom60Minutes interval) = interval

    /// <sumary>
    /// Tries to create a break of at least 1 hour.
    /// </sumary>
    let create fieldName interval =
        ConstrainedType.createBreak fieldName BreakFrom60Minutes (Duration.FromMinutes(60L)) None interval


[<RequireQualifiedAccess>]
module BreakFrom60To180Minutes =

    let value (BreakFrom60To180Minutes interval) = interval

    /// <sumary>
    /// Tries to create a break of at least 1 hour and maximum of 3 hours.
    /// </sumary>
    let create fieldName interval =
        ConstrainedType.createBreak
            fieldName
            BreakFrom60To180Minutes
            (Duration.FromMinutes(60L))
            (Duration.FromMinutes(180L) |> Some)
            interval


[<RequireQualifiedAccess>]
module ScheduleUntil7Hours =

    let value (ScheduleUntil7Hours interval) = interval

    /// <sumary>
    /// Tries to create a schedule from 2 hours up to 7 hours,
    /// optionally with a break.
    /// </sumary>
    let create
        (fieldName: string)
        (breakInterval: Interval option)
        (interval: Interval)
        : Result<ScheduleUntil7Hours, string> =
        ConstrainedType.createSchedule
            fieldName
            ScheduleUntil7Hours
            (Duration.FromMinutes(120L))
            (Duration.FromMinutes(420L))
            breakInterval
            interval


[<RequireQualifiedAccess>]
module ScheduleUntil8Hours =

    let value (ScheduleUntil8Hours interval) = interval

    /// <sumary>
    /// Tries to create a schedule from 7 hours up to 8 hours,
    /// with a break.
    /// </sumary>
    let create
        (fieldName: string)
        (breakInterval: Interval)
        (interval: Interval)
        : Result<ScheduleUntil8Hours, string> =
        ConstrainedType.createSchedule
            fieldName
            ScheduleUntil8Hours
            (Duration.FromMinutes(421L))
            (Duration.FromMinutes(480L))
            (breakInterval |> Some)
            interval


[<RequireQualifiedAccess>]
module TaeSchedule =

    type Full =
        { Hours: ScheduleUntil8Hours
          Break: BreakFrom60To180Minutes }

    type Partial =
        { Hours: ScheduleUntil7Hours
          OptionalBreak: BreakFrom15To180Minutes option }

    /// <sumary>
    /// Tries to create a full schedule for Taes, with a break.
    /// </sumary>
    let createFullSchedule (breakInterval: Interval) (interval: Interval) : Result<Full, string> =
        result {
            let! breakInterval' = BreakFrom60To180Minutes.create "Tae full schedule break" breakInterval
            let! schedule = ScheduleUntil8Hours.create "Tae full schedule" breakInterval interval

            return
                { Hours = schedule
                  Break = breakInterval' }
        }

    /// <sumary>
    /// Tries to create a partial schedule for Taes, optionally with a break.
    /// </sumary>
    let createPartialSchedule (breakInterval: Interval option) (interval: Interval) : Result<Partial, string> =
        result {
            let! breakInterval' =
                breakInterval
                |> Option.traverseResult (BreakFrom15To180Minutes.create "Tae partial schedule break")

            let! schedule = ScheduleUntil7Hours.create "Tae partial schedule" breakInterval interval

            return
                { Hours = schedule
                  OptionalBreak = breakInterval' }
        }


[<RequireQualifiedAccess>]
module ProfessorSchedule =

    type Full =
        { Hours: ScheduleUntil8Hours
          Break: BreakFrom60Minutes }

    type Partial =
        { Hours: ScheduleUntil7Hours
          OptionalBreak: BreakFrom15Minutes option }

    /// <sumary>
    /// Tries to create a full schedule for Professors, with a break.
    /// </sumary>
    let createFullSchedule (breakInterval: Interval) (interval: Interval) : Result<Full, string> =
        result {
            let! breakInterval' = BreakFrom60Minutes.create "Professor full schedule break" breakInterval
            let! schedule = ScheduleUntil8Hours.create "Professor full schedule" breakInterval interval

            return
                { Hours = schedule
                  Break = breakInterval' }
        }

    /// <sumary>
    /// Tries to create a partial schedule for Professors, optionally with a break.
    /// </sumary>
    let createPartialSchedule (breakInterval: Interval option) (interval: Interval) : Result<Partial, string> =
        result {
            let! breakInterval' =
                breakInterval
                |> Option.traverseResult (BreakFrom15Minutes.create "Professor partial schedule break")

            let! schedule = ScheduleUntil7Hours.create "Professor partial schedule" breakInterval interval

            return
                { Hours = schedule
                  OptionalBreak = breakInterval' }
        }
