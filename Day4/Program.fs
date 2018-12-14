// Learn more about F# at http://fsharp.org

open System
open Day4

[<EntryPoint>]
let main argv =
    let result = getSleepiestGuardFromFile "input.txt"
    printfn "%A" (result)
    let result = GetBestMinuteFromFile "input.txt"
    printfn "%A" (result)
    let text = Console.ReadLine()
    0 
