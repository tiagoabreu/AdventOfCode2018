// Learn more about F# at http://fsharp.org

open System
open Day11

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let result = getLargestPowerLevelCoordinate 5468
    printfn "%A" (result)
    let text = Console.ReadLine()
    0 // return an integer exit code
