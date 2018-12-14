// Learn more about F# at http://fsharp.org

open System
open Day5

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let result = getLengthFromReactionInFile "input.txt"
    printfn "%A" (result)
    let result = getMinimalReactionInFile "input.txt"
    printfn "%A" (result)
    let text = Console.ReadLine()
    0 // return an integer exit code
