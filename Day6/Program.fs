// Learn more about F# at http://fsharp.org

open System
open Day6

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let result = getLargestFiniteAreaFromFile "input.txt"
    printfn "%A" (result)
    let text = Console.ReadLine()
    0 // return an integer exit code
