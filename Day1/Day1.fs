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