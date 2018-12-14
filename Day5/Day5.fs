module Day5

open System
open CommonLib.Files

let triggerReaction (chain:char[]) =
    let mutable newChain = chain
    let isCharToRemove idx chr = 
            chain.Length > idx + 1 
            && chain.[idx] <> '-' 
            && chr <> chain.[idx+1]
            && (Char.ToLower chr) = (Char.ToLower chain.[idx+1])

    for idx,chr in Array.indexed chain do
        if (isCharToRemove idx chr) then
            Array.set newChain idx '-'
            Array.set newChain (idx+1) '-'
    Array.filter (fun x -> x <> '-') newChain

let rec getLengthOfFinalReaction currentLength (chain:char[]) =
    let reactionChain = triggerReaction chain
    if (currentLength = reactionChain.Length) then
        reactionChain.Length
    else getLengthOfFinalReaction reactionChain.Length reactionChain

let getMinimalReaction (chain:char[]) =
    let mutable minimalReactionLength = Int32.MaxValue
    let charArr = [|'a'..'z'|]
    for i in charArr do
        let currentReactionLength = getLengthOfFinalReaction Int32.MaxValue
                                    <| Array.filter (fun x -> (Char.ToLower x) <> i) chain
        if (currentReactionLength < minimalReactionLength) then minimalReactionLength <- currentReactionLength
    minimalReactionLength

let getLengthFromReactionInFile fileName =
    (getLineValuesFromFilePath fileName).[0]
    |> explode 
    |> getLengthOfFinalReaction Int32.MaxValue 

let getMinimalReactionInFile fileName =
    (getLineValuesFromFilePath fileName).[0]
    |> explode 
    |> getMinimalReaction

