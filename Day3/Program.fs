// Learn more about F# at http://fsharp.org

open System
open Day3

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let result = getDuplicatesFromFile "input.txt"
    printfn "%A" (result)
    let result = getNonDuplicateClaimFromFile "input.txt"
    printfn "%A" (result)
    let text = Console.ReadLine()
    0 
