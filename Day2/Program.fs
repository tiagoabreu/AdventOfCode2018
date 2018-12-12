// Learn more about F# at http://fsharp.org

open System
open Day2

[<EntryPoint>]
let main argv =
    let result = getCheckSumOfFile "input.txt"  [|2; 3|]
    printfn "%A" (result)
    let result = getValidIdInFile "input.txt"
    printfn "%A" (result)
    let text = Console.ReadLine()
    0 // return an integer exit code
