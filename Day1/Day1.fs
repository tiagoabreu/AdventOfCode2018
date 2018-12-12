module Day1
open CommonLib.Files

let getIntFromStringWithSignal s =
    System.Int32.Parse(s, System.Globalization.NumberStyles.AllowLeadingSign)

let getNewFrequency currentFrequency change =
    currentFrequency + change

let getIntValuesFromFile fileName =
    getLineValuesFromFilePath fileName
    |> Array.map getIntFromStringWithSignal 

let getInitialResultingFrequency (fileName:string) (initialFrequency:int) =
    getIntValuesFromFile fileName
    |> Array.fold getNewFrequency initialFrequency

let getFirstDuplicateFrequency (fileName:string) (initialFrequency:int) =
    let values = getIntValuesFromFile fileName
    let mutable currentFrequency = initialFrequency
    let mutable history : Set<int> = set []
    let mutable doubleFrequency = 0
    while (doubleFrequency = 0) do
        for i in values do
            currentFrequency <- getNewFrequency currentFrequency i
            if (history.Contains currentFrequency && doubleFrequency = 0)
            then doubleFrequency <- currentFrequency
            else history <- history.Add currentFrequency
    doubleFrequency