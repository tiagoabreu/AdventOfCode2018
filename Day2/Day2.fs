module Day2

open CommonLib.Files

let arrayHasElementRepeated arr numOfRepetitions =
    let getOccurencesPerLetter str =
        Array.countBy (fun elem -> elem) str

    let isCorrectNumberOfOccurences (occurenceElem : 'a * int) =
        snd occurenceElem = numOfRepetitions

    getOccurencesPerLetter arr
    |> Array.tryFind isCorrectNumberOfOccurences
    |> Option.isSome

let calculateCheckSum (arr:int[]) =
    Array.fold (fun acc cur -> acc * cur) 1 arr

let incrementCountInCheckSum (checkSumValues:(int * int)[]) valueToIncrement =
    let index = Array.findIndex (fun x -> fst x = valueToIncrement) checkSumValues
    let tupletoChange = checkSumValues.[index]
    Array.set checkSumValues index (valueToIncrement, snd tupletoChange + 1)

let getCheckSumOfIds (repetitionCheck:int[]) (idArr:string[]) =
    let checkSumValues : (int * int)[] = Array.map (fun x -> (x, 0)) repetitionCheck

    let processRepetitions str =
        let processRepetition charArr repetition  =
            if (arrayHasElementRepeated charArr repetition) then
                    incrementCountInCheckSum checkSumValues repetition

        let charArr = explode str
        Array.map (processRepetition charArr) repetitionCheck |> ignore

    Array.map processRepetitions idArr |> ignore

    Array.map (fun x -> snd x) checkSumValues
    |> calculateCheckSum

let getCheckSumOfFile fileName (repetitionCheck:int[]) =
    getLineValuesFromFilePath fileName
    |> getCheckSumOfIds repetitionCheck