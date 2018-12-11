// Learn more about F# at http://fsharp.org

open System
open Day1

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let result = getInitialResultingFrequency "input.txt" 0
    printfn "%A" (result)
    let text = Console.ReadLine()
    0
    0 // return an integer exit code
