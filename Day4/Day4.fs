module Day4

open CommonLib.Files
open System
open System

type EventType =
    | BeginShift
    | WakeUp
    | FallAsleep

type Event = {
    Date: DateTime;
    Type: EventType;
    GuardId: int
}

let orderEvents eventArr =
    Array.sortBy (fun x -> x.Date) eventArr

let getMinutesMap (eventsArr:Event[]) =
    let mutable minutesMap = Map.ofArray <| Array.map (fun x -> x.GuardId, (Array.zeroCreate 60:int[])) eventsArr
    let mutable guardOnDuty = 0
    let mutable sleepMinute = 0
    for event in eventsArr do
        if (event.Type = BeginShift) then
            guardOnDuty <- event.GuardId
        elif (event.Type = FallAsleep) then
            sleepMinute <- event.Date.Minute
        else
            let currentValue = minutesMap.[guardOnDuty]
            for i = sleepMinute to event.Date.Minute do
                Array.set currentValue i (currentValue.[i] + 1)
            minutesMap <- minutesMap.Add (guardOnDuty, currentValue)
    minutesMap

let getBestMinute (minutesMap:Map<int,int[]>) = 
    let mutable chosenGuard = 0
    let mutable choseMinute = 0
    let mutable numOfTimes = 0
    for i in minutesMap do
        let curNumOfTimes = Array.max (i.Value)
        if (curNumOfTimes > numOfTimes) then
            choseMinute <- Array.findIndex (fun x -> x = curNumOfTimes) i.Value
            numOfTimes <- curNumOfTimes
            chosenGuard <- i.Key
    chosenGuard, choseMinute

let getSleepiestGuard (minutesMap:Map<int,int[]>) =
    let countSleepingMinutes minuteArr =
        Array.sum minuteArr
    let mutable sleepiestGuard = 0
    let mutable numOfMinutes = 0
    for i in minutesMap do
        let sleepingMinutes = countSleepingMinutes (i.Value)
        if (numOfMinutes < sleepingMinutes) then
            numOfMinutes <- sleepingMinutes
            sleepiestGuard <- i.Key
    sleepiestGuard, Array.findIndex (fun x -> x = Array.max (minutesMap.[sleepiestGuard])) minutesMap.[sleepiestGuard]

let parseLineIntoEvent (str:string) =
    let date = str.[str.IndexOf('[') + 1 .. str.IndexOf("]") - 1] |> Convert.ToDateTime
    let mutable eventType:EventType = WakeUp
    let mutable guardId = 0

    if (str.IndexOf("falls") <> -1) then eventType <- FallAsleep
    elif (str.IndexOf("wakes") <> -1) then eventType <- WakeUp
    else eventType <- BeginShift

    if (eventType = BeginShift) then
        guardId <- str.[str.IndexOf('#') + 1 .. str.IndexOf(" b") - 1] |> int
    
    { Date = date; Type = eventType; GuardId = guardId }

let getSleepiestGuardFromFile fileName =
    let sleepiestGuard = getLineValuesFromFilePath fileName
                        |> Array.map parseLineIntoEvent
                        |> orderEvents
                        |> getMinutesMap
                        |> getSleepiestGuard
    fst sleepiestGuard * snd sleepiestGuard


let GetBestMinuteFromFile fileName =
    let bestMinute = getLineValuesFromFilePath fileName
                        |> Array.map parseLineIntoEvent
                        |> orderEvents
                        |> getMinutesMap
                        |> getBestMinute
    fst bestMinute * snd bestMinute
   