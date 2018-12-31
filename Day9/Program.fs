// Learn more about F# at http://fsharp.org

open System
open Day9

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let result = playMarbles 431 7095000
    printfn "%A" (result)
    let text = Console.ReadLine()
    0 // return an integer exit code
